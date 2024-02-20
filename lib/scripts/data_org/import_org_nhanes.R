# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing data

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")

print("Loaded dependencies")

#### Defining functions ####
#convert time to minutes (https://stackoverflow.com/questions/40476796/r-function-to-convert-time-of-day-to-total-minutes)
time_dif <- function(x) difftime(as.POSIXct(x, format = "%H:%M"), as.POSIXct("00:00", format = "%H:%M"), units = "min")

nhanes_names <- function(dl_df, dt_group, nh_tble) {
  #puts human readable names on the NHANES tables
  diet_names <- nhanesA::nhanesTableVars(data_group = dt_group,
                                         nh_table = nh_tble,
                                         namesonly = FALSE)

  my_ord <- match(names(dl_df), diet_names$Variable.Name)
  names(dl_df) <- diet_names$Variable.Description[my_ord]
  return(dl_df)
}

download_org_nhanes <- function(dt_group, nh_tble) {
  #downloads and relabels table
  dl_tble <- nhanesA::nhanes(nh_tble)
  
  dl_tble <- dl_tble[,sapply(dl_tble, function(x) !all(is.na(x)))]
  
  dl_tble_vars <- nhanesA::nhanesTableVars(data_group=dt_group, 
                                             nh_table = nh_tble, 
                                             namesonly = TRUE)
  
  dl_tble <- nhanesA::nhanesTranslate(nh_tble, dl_tble_vars, data = dl_tble)
  
  dl_tble <- nhanes_names(dl_tble, dt_group, nh_tble)
  return(dl_tble)
}

convert_dummy <- function(df){
  my_list <- sapply(df, function(x){
  if (is.factor(x) | is.character(x)){
    if(length(unique(x)) > 3){
      return(fastDummies::dummy_cols(x, remove_selected_columns = TRUE,
                                     ignore_na = TRUE, remove_first_dummy = TRUE))
    }else{
      return(fastDummies::dummy_cols(x, remove_selected_columns = TRUE,
                                     ignore_na = TRUE, remove_first_dummy = FALSE))
    }
  }#END if (is.factor(x))
  else{
    return(x)
    }
  })
  return(data.frame(my_list, check.names = FALSE))
}

save_features <- function(vect = raw_features_used, vec_names = "name", feature_vect){
  names(feature_vect) <- rep(vec_names, length(feature_vect))
  return(c(raw_features_used, feature_vect))
}
  
#### Establish directory layout and other constants ####
output_dir <- file.path("output", "tables")

drop_criteria <- c("Dietary recall status" = c("Reported consuming breast-milk"),#DEMO_I
                   ""
)

# tables to add:
#   Physical Activity PAQ_I
#   Oral Health	OHQ_I
#   Occupation	OCQ_I
#   Medical Conditions	MCQ_I
#   Prescription Medications	RXQ_RX_I
#   Preventive Aspirin Use	RXQASA_I

#   Volatile Toxicant	VTQ_I VTQ_I

# LAB:
#   HDL_I
#   Acrylamide & Glycidamide	AMDGYD_I
#   Arsenic - Total - Urine	UTAS_I
#   Cholesterol - Low - Density Lipoprotein (LDL) & Triglycerides	TRIGLY_I
  # Glyphosate (GLYP) - Urine (Surplus)	SSGLYP_I
#   Organophosphate Insecticides - Dialkyl Phosphate Metabolites - Urine	OPD_I   
  



# Add later:
#   Reproductive Health	RHQ_I Doc	RHQ_I Data [XPT - 1.2 MB]	February 2018
#   Sexual Behavior	SXQ_I Doc	SXQ_I Data [XPT - 1.9 MB]	December 2017
#   Sleep Disorders	SLQ_I Doc	SLQ_I Data [XPT - 360.2 KB]	March 2018
#   Smoking - Cigarette Use	SMQ_I Doc	SMQ_I Data [XPT - 2.6 MB]	September 2017
#   Smoking - Household Smokers	SMQFAM_I Doc	SMQFAM_I Data [XPT - 312.9 KB]	September 2017
#   Smoking - Recent Tobacco Use	SMQRTU_I Doc	SMQRTU_I Data [XPT - 1.5 MB]	September 2017
#   Smoking - Secondhand Smoke Exposure	SMQSHS_I Doc	SMQSHS_I Data [XPT - 1.1 MB]	September 2017

#### Loading in data ####
food_codes <- readxl::read_excel(file.path("Data", "WWEIA1516_foodcat_FNDDS.xlsx"), 
                                 trim_ws = T, na = c("", "NA"))

raw_features_used <- c()

# Download raw NHANES data files ## 
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
nhanesA::nhanesTables(data_group="DEMO", year=2015)
demo_2015 <- download_org_nhanes("DEMO", "DEMO_I")
raw_features_used <- save_features(raw_features_used, "DEMO_I", colnames(demo_2015))
demo_2015 <- convert_dummy(demo_2015)
write.csv(demo_2015, file = file.path(output_dir, "demo_2015.csv"),
          row.names = FALSE)

PAQ_2015 <- download_org_nhanes("Q", "PAQ_I")
raw_features_used <- save_features(raw_features_used, "PAQ_I", colnames(PAQ_2015))
PAQ_2015 <- convert_dummy(PAQ_2015)

RXQ_2015 <- download_org_nhanes("Q", "RXQ_RX_I")

my_RXQ <- RXQ_2015[,1:2]

my_RXQ <- my_RXQ[!duplicated(my_RXQ), ]

no_RX <- my_RXQ[my_RXQ$`In the past 30 days, have you used or taken medication for which a prescription is needed?  Do not include prescription vitamins` == "No", "Respondent sequence number."]

nhanesA::nhanesTables(data_group="DIET", year=2015)

d1_24supp_2015 <- download_org_nhanes("DIET", "DS1IDS_I")

d1_diet_2015 <- download_org_nhanes("DIET", "DR1IFF_I")

d1_diet_2015 <- d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]
raw_features_used <- save_features(raw_features_used, "DR1IFF_I", colnames(d1_diet_2015))
d1_diet_2015$`USDA food code` <- food_codes$category_description[match(d1_diet_2015$`USDA food code`, food_codes$food_code)]

dir.create(file.path(output_dir), showWarnings = FALSE)
write.csv(d1_diet_2015, file = file.path(output_dir, "d1_diet_2015.csv"),
          row.names = FALSE)

nhanesA::nhanesTables(data_group="EXAM", year=2015)
BP_2015 <- nhanesA::nhanes("BPX_I")
BP_2015 <- download_org_nhanes("EXAM","BPX_I")
BP_2015$Systolic_Hypertension <- BP_2015$`Systolic:  Blood pressure (first reading) mm Hg` >= 130
BP_2015$Diastolic_Hypertension <- BP_2015$`Diastolic:  Blood pressure (first reading) mm Hg` >= 80
write.csv(BP_2015, file = file.path(output_dir, "BP_2015.csv"),
          row.names = FALSE)
# resps: "Systolic:  Blood pressure (first reading) mm Hg", "Systolic_Hypertension", "Diastolic:  Blood pressure (first reading) mm Hg", "Diastolic_Hypertension"

nhanesA::nhanesTables(data_group="LAB", year=2015)

#### Small amount data cleaning of d1_diet_2015 and BP_2015 datasets ####
#reogranize with dummy variables to work with RF

# d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]

d1_diet_2015 <- fastDummies::dummy_cols(d1_diet_2015,
                                        select_columns = c("USDA food code"),
                                        ignore_na = TRUE, remove_selected_columns = TRUE)
d1_diet_2015 <- subset(d1_diet_2015, select = -c(2:8)) #Row sums on these doesn't make sense
d1_diet_2015 <- rowsum(d1_diet_2015[,3:ncol(d1_diet_2015)],
               group = d1_diet_2015$`Respondent sequence number.`,na.rm=T, )
# d1_diet_2015 <- rowsum(d1_diet_2015[,3:ncol(d1_diet_2015)],
#                     group = d1_diet_2015$`Respondent sequence number.`,na.rm=T, )

#rowsum removes group column and we need it downstream, so put it back:
d1_diet_2015$`Respondent sequence number.` <- row.names(d1_diet_2015)


# Create datasets from interection of BP and Diet datasets
BP_2015 <- BP_2015[!is.na(BP_2015$`Systolic:  Blood pressure (first reading) mm Hg`),]
my_intersect <- intersect(BP_2015$`Respondent sequence number.`,
                          d1_diet_2015$`Respondent sequence number.`)

d1_diet_2015 <- d1_diet_2015[d1_diet_2015$`Respondent sequence number.`  %in% my_intersect,]
BP_2015 <- BP_2015[BP_2015$`Respondent sequence number.` %in% my_intersect,]
demo_2015 <- demo_2015[demo_2015$`Respondent sequence number.`  %in% my_intersect,]

write.csv(d1_diet_2015, file = file.path(output_dir, "d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(demo_2015, file = file.path(output_dir, "demo_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(BP_2015, file = file.path(output_dir, "BP_2015_match_diet_SYST1_RF.csv"),
          row.names = FALSE)

demo_d1_diet_2015 <- merge(d1_diet_2015, demo_2015, by = "Respondent sequence number." )

write.csv(demo_d1_diet_2015, file = file.path(output_dir, "demo_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
#create no RX datasets
my_intersect <- intersect(no_RX, my_intersect)

d1_diet_2015 <- d1_diet_2015[d1_diet_2015$`Respondent sequence number.`  %in% my_intersect,]
BP_2015 <- BP_2015[BP_2015$`Respondent sequence number.` %in% my_intersect,]
demo_2015 <- demo_2015[demo_2015$`Respondent sequence number.`  %in% my_intersect,]

write.csv(d1_diet_2015, file = file.path(output_dir, "noRx_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(demo_2015, file = file.path(output_dir, "noRx_demo_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(BP_2015, file = file.path(output_dir, "noRx_BP_2015_match_diet_SYST1_RF.csv"),
          row.names = FALSE)
demo_d1_diet_2015 <- merge(d1_diet_2015, demo_2015, by = "Respondent sequence number." )
demo_d1_diet_2015 <- demo_d1_diet_2015[, !sapply(demo_d1_diet_2015, is.factor)]
write.csv(demo_d1_diet_2015, file = file.path(output_dir, "noRx_demo_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)

my_intersect <- intersect(PAQ_2015$`Respondent sequence number.`, my_intersect)

d1_diet_2015 <- d1_diet_2015[d1_diet_2015$`Respondent sequence number.`  %in% my_intersect,]
BP_2015 <- BP_2015[BP_2015$`Respondent sequence number.` %in% my_intersect,]
demo_2015 <- demo_2015[demo_2015$`Respondent sequence number.`  %in% my_intersect,]

write.csv(d1_diet_2015, file = file.path(output_dir, "PAQ_noRx_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(demo_2015, file = file.path(output_dir, "PAQ_noRx_demo_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(BP_2015, file = file.path(output_dir, "PAQ_noRx_BP_2015_match_diet_SYST1_RF.csv"),
          row.names = FALSE)
PAQ_demo_d1_diet_2015 <- merge(PAQ_2015, demo_d1_diet_2015, by = "Respondent sequence number." )
PAQ_demo_d1_diet_2015 <- PAQ_demo_d1_diet_2015[, !sapply(PAQ_demo_d1_diet_2015, is.factor)]
write.csv(PAQ_demo_d1_diet_2015, file = file.path(output_dir, "PAQ_noRx_demo_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)

#### Add Phthalates and Plasticizers Metabolites - Urine (PHTHTE_I) ####
nhanesA::nhanesTables(data_group="LAB", year=2015)
PHTHTE_2015 <- download_org_nhanes("LAB","PHTHTE_I")
raw_features_used <- save_features(raw_features_used, "PHTHTE_I", colnames(PHTHTE_2015))
PHTHTE_2015 <- convert_dummy(PHTHTE_2015)

write.csv(PHTHTE_2015, file = file.path(output_dir, "PHTHTE_2015.csv"),
          row.names = FALSE)
my_intersect <- intersect(PHTHTE_2015$`Respondent sequence number.`, my_intersect)

d1_diet_2015 <- d1_diet_2015[d1_diet_2015$`Respondent sequence number.`  %in% my_intersect,]
BP_2015 <- BP_2015[BP_2015$`Respondent sequence number.` %in% my_intersect,]
demo_2015 <- demo_2015[demo_2015$`Respondent sequence number.`  %in% my_intersect,]

write.csv(d1_diet_2015, file = file.path(output_dir, "PHTHTE_PAQ_noRx_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(demo_2015, file = file.path(output_dir, "PHTHTE_PAQ_noRx_demo_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(BP_2015, file = file.path(output_dir, "PHTHTE_PAQ_noRx_BP_2015_match_diet_SYST1_RF.csv"),
          row.names = FALSE)
PHTHTE_PAQ_demo_d1_diet_2015 <- merge(PHTHTE_2015, PAQ_demo_d1_diet_2015, by = "Respondent sequence number." )
PHTHTE_PAQ_demo_d1_diet_2015 <- PHTHTE_PAQ_demo_d1_diet_2015[, !sapply(PAQ_demo_d1_diet_2015, is.factor)]
write.csv(PHTHTE_PAQ_demo_d1_diet_2015, file = file.path(output_dir, "PHTHTE_PAQ_noRx_demo_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)

raw_features <- cbind(read.table(text = names(raw_features_used)), raw_features_used)
names(raw_features) <- c("Table", "Feature")
raw_features <- na.omit(raw_features)
write.csv(raw_features,
          file = file.path("output","graphics", "13_Feb_2024", "raw_predictor_features.csv"),
          row.names = FALSE)

#Short version for testing ML code for completion
short_rsn <- my_intersect[1:200]
sh_d1_diet_2015 <- d1_diet_2015[d1_diet_2015$`Respondent sequence number.`  %in% short_rsn,]
sh_demo_2015 <- demo_2015[demo_2015$`Respondent sequence number.`  %in% short_rsn,]
sh_BP_2015 <- BP_2015[BP_2015$`Respondent sequence number.` %in% short_rsn,]

write.csv(sh_d1_diet_2015, file = file.path(output_dir, "short_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(sh_demo_2015, file = file.path(output_dir, "short_demo_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)
write.csv(sh_BP_2015, file = file.path(output_dir, "short_BP_2015_match_diet_SYST1_RF.csv"),
          row.names = FALSE)

#### Create "foods only" dataset ####
# d1_diet_2015 <- nhanesA::nhanes("DR1IFF_I")
# 
# d1_diet_2015_vars <- nhanesA::nhanesTableVars(data_group = "DIET",
#                                            nh_table = "DR1IFF_I",
#                                            namesonly = TRUE)
# d1_diet_2015 <- nhanesA::nhanesTranslate(nh_table = "DR1IFF_I",
#                                       d1_diet_2015_vars, data = d1_diet_2015)
# d1_diet_2015 <- nhanes_names(d1_diet_2015, "DIET", "DR1IFF_I")
# 
# d1_diet_2015 <- d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]
# d1_diet_2015$`USDA food code` <- food_codes$food_code_description[match(d1_diet_2015$`USDA food code`, food_codes$food_code)]
# 
# dir.create(file.path(output_dir), showWarnings = FALSE)
# write.csv(d1_diet_2015, file = file.path(output_dir, "d1_diet_2015.csv"),
#           row.names = FALSE)
# 
# nhanesA::nhanesTables(data_group="EXAM", year=2015)
# BP_2015 <- nhanesA::nhanes("BPX_I")
# 
# BP_2015_vars <- nhanesA::nhanesTableVars(data_group = "EXAM",
#                                          nh_table = "BPX_I",
#                                          namesonly = TRUE)
# BP_2015 <- nhanesA::nhanesTranslate(nh_table = "BPX_I",
#                                     BP_2015_vars, data = BP_2015)
# 
# BP_2015 <- nhanes_names(BP_2015, "EXAM", "BPX_I")
# 
# write.csv(BP_2015, file = file.path(output_dir, "BP_2015.csv"),
#           row.names = FALSE)
# 
# nhanesA::nhanesTables(data_group="LAB", year=2015)
# 
# #### Small amount data cleaning of d1_diet_2015 and BP_2015 datasets ####
# #reogranize with dummy variables to work with RF
# 
# d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]
# 
# d1_diet_2015 <- fastDummies::dummy_cols(d1_diet_2015, select_columns = c("USDA food code"),
#                                      ignore_na = TRUE, remove_selected_columns = TRUE)
# 
# d1_diet_2015 <- rowsum(d1_diet_2015[,3:ncol(d1_diet_2015)],
#                     group = d1_diet_2015$`Respondent sequence number.`,na.rm=T, )
# 
# #rowsum removes group column and we need it downstream, so put it back:
# d1_diet_2015$`Respondent sequence number.` <- row.names(d1_diet_2015)
# 
# 
# # Create datasets from interection of BP and Diet datasets
# BP_2015 <- BP_2015[!is.na(BP_2015$`Systolic:  Blood pressure (first reading) mm Hg`),]
# my_intersect <- intersect(BP_2015$`Respondent sequence number.`,
#                           d1_diet_2015$`Respondent sequence number.`)
# 
# d1_diet_2015 <- d1_diet_2015[d1_diet_2015$`Respondent sequence number.`  %in% my_intersect,]
# BP_2015 <- BP_2015[BP_2015$`Respondent sequence number.` %in% my_intersect,]
# demo_2015 <- demo_2015[demo_2015$`Respondent sequence number.`  %in% my_intersect,]
# 
# write.csv(d1_diet_2015, file = file.path(output_dir, "d1_diet_2015_match_bp_SYST1_RF.csv"),
#           row.names = FALSE)
# 
