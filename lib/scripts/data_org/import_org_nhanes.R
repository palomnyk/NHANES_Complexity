# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing data
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html

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
                                         namesonly = FALSE,
                                         nchar = 160)

  my_ord <- match(names(dl_df), diet_names$Variable.Name)
  names(dl_df) <- diet_names$Variable.Description[my_ord]
  return(dl_df)
}

download_org_nhanes <- function(dt_group, nh_tble) {
  print(paste(dt_group, nh_tble))
  #downloads and relabels table
  dl_tble <- nhanesA::nhanes(nh_tble)
  # print(dl_tble)
  # dl_tble <- dl_tble[,sapply(dl_tble, function(x) !all(is.na(x)))]
  print(names(dl_tble)[1:10])
  dl_tble <- nhanesA::nhanesTranslate(nh_table = nh_tble,
                                      details = TRUE,
                                      colnames = names(dl_tble),
                                      data = dl_tble)
  
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
tst_df <- data.frame(idvar = c("a", "a", "b", "b", "b", "c"),
                     item =  c("A", "C", "A", "B", "B",  "C"),
                     count = c(1,2, 1,1,2, 5))
dummy_df <- data.frame(idvar = c("a", "b", "c"),
                       item_A = c(1, 1, 0),
                       item_B = c(0, 3, 0),
                       item_C = c(2, 0, 5))
row.names(dummy_df) <- c("a", "b", "c")
                     
two_column_dummy <- function(df, id_colnm, item_colnm, count_colnm) {
  # Function for using one column as the item (dummy car), and another as a count
  # Arguments:
  # dataframe (df) that has at least an id, item, and count column
  # a string for the column name of ids (id_colnm), 
  # a string for the column name of items (item_colnm), 
  # and a string for the column name of counts (count_colnm)
  print(paste(id_colnm, item_colnm, count_colnm))
  uniq_items <- unique(df[,item_colnm])
  uniq_items <- paste0(item_colnm, "_",uniq_items)
  print(uniq_items)
  id_var <- unique(df[,id_colnm])
  new_df <- data.frame(matrix(0 ,nrow = length(id_var), ncol = length(uniq_items)))
  names(new_df) <- uniq_items
  row.names(new_df) <- id_var
print(id_var)
  for (id in 1:nrow(new_df)) {
    my_id <- as.character(id_var[id])
    print(my_id)
    my_sub <- df[df[,id_colnm]==my_id,#subset of df argument
                           c(item_colnm,count_colnm,
                             id_colnm)]
    for (rw in row.names(my_sub)){
      # print(rw)
      food <- my_sub[rw, item_colnm]
      print(food)
      my_cell <- new_df[my_id, paste0(item_colnm, "_", food)]
      my_grams <- my_sub[rw, count_colnm]
      new_df[my_id, paste0(item_colnm, "_", food)] <- my_cell + my_grams
    }
  }
  new_df[,id_colnm] <- row.names(new_df)
  new_df <- new_df[ , order(names(new_df))]
  return(new_df)
}
unit_test1 <- two_column_dummy(df = tst_df,
                 id_colnm = "idvar",
                 item_colnm = "item",
                 count_colnm = "count")
identical(unit_test1, dummy_df)


#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "nhanesA_tables")
dir.create(output_dir)
# drop_criteria <- c("Dietary recall status" = c("Reported consuming breast-milk"),#DEMO_I
#                    ""
# )

#### Loading in data ####
food_codes <- readxl::read_excel(file.path("Data", "WWEIA1516_foodcat_FNDDS.xlsx"), 
                                 trim_ws = T, na = c("", "NA"))

import_tables <- read.csv(file.path("Data", "nhanesTablesToAdd.csv"),
                           header = T, sep = ",", comment.char = "#")

short_col_name <- c()
long_col_name <- c()
dt_grp_name <- c()
nh_table_name <- c()
year <- c()
full_df <- data.frame()
not_added <- c()

for (i in 1:nrow(import_tables)){
  yr <- import_tables$year[i]
  dt_grp <- import_tables$data_group[i]
  nh_tbl <- import_tables$nh_table[i]
  fname <- paste0(nh_tbl,"_", yr, ".csv")
  print(fname)
  fname_path <- file.path(output_dir, fname)
  if (!file.exists(fname_path)){
    print(paste(fname_path, "not found - Downloading!"))
    dl_tble <- nhanesA::nhanes(nh_tbl)
    write.csv(dl_tble, fname_path, row.names = FALSE)
    Sys.sleep(2)
  }else{
    dl_tble <- read.csv(fname_path, header = TRUE, check.names = FALSE)
  }
  dl_tble <- dl_tble[,sapply(dl_tble, function(x) !all(is.na(x)))]
  short_col_name <- c(short_col_name, names(dl_tble))
  num_col <- ncol(dl_tble)
  dt_grp_name <- c(dt_grp_name, rep(dt_grp, num_col))
  nh_table_name <- c(nh_table_name, rep(nh_tbl, num_col))
  year <- c(year, rep(yr, num_col))
  
  dl_tble <- nhanesA::nhanesTranslate(nh_table = nh_tbl,
                           data = dl_tble,
                           details = TRUE,
                           colnames = names(dl_tble))
  
  dl_tble <- nhanes_names(dl_tble, dt_grp, nh_tbl)
  
  attr(dl_tble, "names") <- sub("[[:punct:]]$", "", names(dl_tble)) 
  long_col_name <- c(long_col_name, names(dl_tble))
  fname <- paste0(nh_tbl,"_", yr, "_lg.csv")
  fname_path <- file.path(output_dir, fname)
  if (!file.exists(fname_path)){#save with long name
    write.csv(dl_tble, fname_path, row.names = FALSE)
  }
  if(length(unique(dl_tble$`Respondent sequence number`)) == nrow(dl_tble)){
    if (nrow(full_df) < 1){
      full_df <- dl_tble
    }else{
      print(paste(nrow(full_df), nrow(dl_tble)))
      full_df <- merge(full_df, dl_tble, by = "Respondent sequence number",
                       all = TRUE)
    }
  }else{
    print(paste("Didn't add", nh_tbl))
    not_added <- c(not_added, nh_tbl)
  }
}

print(paste("Any NA in respondent ID:", any(is.na(full_df$`Respondent sequence number`))))
stopifnot(any(is.na(full_df$`Respondent sequence number`)))
print(paste("Respondent IDs are unique:",
            length(unique(full_df$`Respondent sequence number`) == nrow(full_df))))
stopifnot(length(unique(full_df$`Respondent sequence number`) == nrow(full_df)))

# RXQ_2015 <- download_org_nhanes("Q", "RXQ_RX_I")
# test <- grepl("hypertension", RXQ_2015$`ICD-10-CM code 1 description.`, ignore.case = TRUE)

nhanesA::nhanesTables(data_group="DIET", year=2015)

# d1_24supp_2015 <- download_org_nhanes("DIET", "DS1IDS_I")

#### Orgnaize diet data####
# d1_diet_2015 <- download_org_nhanes("DIET", "DR1IFF_I")
d1_diet_2015 <- nhanesA::nhanes("DR1IFF_I")
d1_diet_2015 <- nhanesA::nhanesTranslate(nh_table = "DR1IFF_I",
                                  data = d1_diet_2015,
                                  details = TRUE,
                                  colnames = names(d1_diet_2015))
d1_diet_2015 <- nhanes_names(d1_diet_2015,"DIET", "DR1IFF_I")
attr(d1_diet_2015, "names") <- sub("[[:punct:]]$", "", names(d1_diet_2015))
# d1_diet_2015 <- d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]
d1_diet_2015$`USDA food code` <- food_codes$food_code_description[match(d1_diet_2015$`USDA food code`, food_codes$food_code)]

USDA_food_g_only <- two_column_dummy(d1_diet_2015,
                    id_colnm = "Respondent sequence number",
                    item_colnm = "USDA food code",
                    count_colnm = "Gram weight of the food/individual component")

write.csv(USDA_food_g_only, file = file.path(output_dir, "USDA_food_g_2015.csv"),
          row.names = FALSE)

USDA_food_cat_only <- subset(d1_diet_2015, select = c("USDA food code", "Respondent sequence number"))
  
USDA_food_cat_only <- fastDummies::dummy_cols(USDA_food_cat_only,
                                              select_columns = c("USDA food code"),
                                              ignore_na = TRUE, remove_selected_columns = TRUE)

USDA_food_cat_only <- rowsum(USDA_food_cat_only,
       group = USDA_food_cat_only$`Respondent sequence number`,na.rm=T, )
USDA_food_cat_only$`Respondent sequence number` <- row.names(USDA_food_cat_only)

dir.create(file.path(output_dir), showWarnings = FALSE)
write.csv(USDA_food_cat_only, file = file.path(output_dir, "USDA_food_cat_only_2015.csv"),
          row.names = FALSE)

BPQ_I_2015 <- nhanesA::nhanes("BPQ_I")
BPQ_I_2015 <- nhanesA::nhanesTranslate(nh_table = "BPQ_I",
                                         data = BPQ_I_2015,
                                         details = TRUE,
                                         colnames = names(BPQ_I_2015))
BPQ_I_2015 <- nhanes_names(BPQ_I_2015,"Q", "BPQ_I")
attr(BPQ_I_2015, "names") <- sub("[[:punct:]]$", "", names(BPQ_I_2015))

no_RX <- BPQ_I_2015[BPQ_I_2015$`Because of {your/SP's} (high blood pressure/hypertension), {have you/has s/he} ever been told to . . . take prescribed medicine` == "Yes", "Respondent sequence number"]

USDA_food_cat_only <- USDA_food_cat_only[USDA_food_cat_only$`Respondent sequence number` %in% no_RX,]

write.csv(USDA_food_cat_only, file = file.path(output_dir, "USDA_food_cat_only_2015_noRx.csv"),
          row.names = FALSE)



BPQ_I_2015 <- fastDummies::dummy_cols(BPQ_I_2015,
                                        select_columns = c("USDA food code"),
                                        ignore_na = TRUE, remove_selected_columns = TRUE)
d1_diet_2015 <- subset(d1_diet_2015, select = -c(2:8)) #Row sums on these doesn't make sense
d1_diet_2015 <- rowsum(d1_diet_2015[,3:ncol(d1_diet_2015)],
               group = d1_diet_2015$`Respondent sequence number.`,na.rm=T, )
# d1_diet_2015 <- rowsum(d1_diet_2015[,3:ncol(d1_diet_2015)],
#                     group = d1_diet_2015$`Respondent sequence number.`,na.rm=T, )

#rowsum removes group column and we need it downstream, so put it back:
d1_diet_2015$`Respondent sequence number.` <- row.names(d1_diet_2015)


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
