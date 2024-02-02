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
  
  dl_tble <- nhanesA::nhanes(nh_tble)
  
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
  
  
#### Establish directory layout and other constants ####
output_dir <- file.path("output", "tables")

#### Loading in data ####
food_codes <- readxl::read_excel(file.path("Data", "WWEIA1516_foodcat_FNDDS.xlsx"), 
                                 trim_ws = T, na = c("", "NA"))

# Download raw NHANES data files ## 
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
nhanesA::nhanesTables(data_group="DEMO", year=2015)
demo_2015 <- download_org_nhanes("DEMO", "DEMO_I")

demo_2015 <- convert_dummy(demo_2015)

write.csv(demo_2015, file = file.path(output_dir, "demo_2015.csv"),
          row.names = FALSE)

nhanesA::nhanesTables(data_group="DIET", year=2015)

d1_24supp_2015 <- nhanesA::nhanes("DS1IDS_I")


d1_diet_2015 <- nhanesA::nhanes("DR1IFF_I")

d1_diet_2015_vars <- nhanesA::nhanesTableVars(data_group = "DIET",
                                       nh_table = "DR1IFF_I",
                                       namesonly = TRUE)
d1_diet_2015 <- nhanesA::nhanesTranslate(nh_table = "DR1IFF_I",
                                      d1_diet_2015_vars, data = d1_diet_2015)
d1_diet_2015 <- nhanes_names(d1_diet_2015, "DIET", "DR1IFF_I")

d1_diet_2015 <- d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]
d1_diet_2015$`USDA food code` <- food_codes$food_code_description[match(d1_diet_2015$`USDA food code`, food_codes$food_code)]

dir.create(file.path(output_dir), showWarnings = FALSE)
write.csv(d1_diet_2015, file = file.path(output_dir, "d1_diet_2015.csv"),
          row.names = FALSE)

nhanesA::nhanesTables(data_group="EXAM", year=2015)
BP_2015 <- nhanesA::nhanes("BPX_I")

BP_2015_vars <- nhanesA::nhanesTableVars(data_group = "EXAM",
                                           nh_table = "BPX_I",
                                           namesonly = TRUE)
BP_2015 <- nhanesA::nhanesTranslate(nh_table = "BPX_I",
                                    BP_2015_vars, data = BP_2015)

BP_2015 <- nhanes_names(BP_2015, "EXAM", "BPX_I")

write.csv(BP_2015, file = file.path(output_dir, "BP_2015.csv"),
          row.names = FALSE)

nhanesA::nhanesTables(data_group="LAB", year=2015)

#### Small amount data cleaning of d1_diet_2015 and BP_2015 datasets ####
#reogranize with dummy variables to work with RF

d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]

d1_diet_2015 <- fastDummies::dummy_cols(d1_diet_2015, select_columns = c("USDA food code"),
                                     ignore_na = TRUE, remove_selected_columns = TRUE)

d1_diet_2015 <- rowsum(d1_diet_2015[,3:ncol(d1_diet_2015)],
                    group = d1_diet_2015$`Respondent sequence number.`,na.rm=T, )

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


#### Create a new diet/demo vs BP dataset by combining diet and demo data ####

demo_d1_diet_2015 <- merge(d1_diet_2015, demo_2015, by = "Respondent sequence number." )

write.csv(demo_d1_diet_2015, file = file.path(output_dir, "demo_d1_diet_2015_match_bp_SYST1_RF.csv"),
          row.names = FALSE)

#### Create "foods only" dataset ####
d1_diet_2015 <- nhanesA::nhanes("DR1IFF_I")

d1_diet_2015_vars <- nhanesA::nhanesTableVars(data_group = "DIET",
                                           nh_table = "DR1IFF_I",
                                           namesonly = TRUE)
d1_diet_2015 <- nhanesA::nhanesTranslate(nh_table = "DR1IFF_I",
                                      d1_diet_2015_vars, data = d1_diet_2015)
d1_diet_2015 <- nhanes_names(d1_diet_2015, "DIET", "DR1IFF_I")

d1_diet_2015 <- d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]
d1_diet_2015$`USDA food code` <- food_codes$food_code_description[match(d1_diet_2015$`USDA food code`, food_codes$food_code)]

dir.create(file.path(output_dir), showWarnings = FALSE)
write.csv(d1_diet_2015, file = file.path(output_dir, "d1_diet_2015.csv"),
          row.names = FALSE)

nhanesA::nhanesTables(data_group="EXAM", year=2015)
BP_2015 <- nhanesA::nhanes("BPX_I")

BP_2015_vars <- nhanesA::nhanesTableVars(data_group = "EXAM",
                                         nh_table = "BPX_I",
                                         namesonly = TRUE)
BP_2015 <- nhanesA::nhanesTranslate(nh_table = "BPX_I",
                                    BP_2015_vars, data = BP_2015)

BP_2015 <- nhanes_names(BP_2015, "EXAM", "BPX_I")

write.csv(BP_2015, file = file.path(output_dir, "BP_2015.csv"),
          row.names = FALSE)

nhanesA::nhanesTables(data_group="LAB", year=2015)

#### Small amount data cleaning of d1_diet_2015 and BP_2015 datasets ####
#reogranize with dummy variables to work with RF

d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]

d1_diet_2015 <- fastDummies::dummy_cols(d1_diet_2015, select_columns = c("USDA food code"),
                                     ignore_na = TRUE, remove_selected_columns = TRUE)

d1_diet_2015 <- rowsum(d1_diet_2015[,3:ncol(d1_diet_2015)],
                    group = d1_diet_2015$`Respondent sequence number.`,na.rm=T, )

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

