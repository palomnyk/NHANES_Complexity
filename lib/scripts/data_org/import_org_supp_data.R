# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing supplementary data
# Plan for the supplemental data:
#     - combine the two days of supplements (DS1IDS_I and DS2IDS_I) as was done for the food.
#     - Remove rows for "NO PRODUCT INFORMATION AVAILABLE" in DSDMTCH column
#     - create vars for the supplement names (DSDSUPP/"Supplement Name")
#         - Use "Reported serving size/label serving size" as value
#     - variables for the total quantities of the nutrients
# Plan for merge with diet data:
#     - The common variables between these are the nutrients
#       - Remove nutrient vars from both
#       - rbind dummy var.test
#       - Aggregate nutrient variables by summing iteratively

rm(list = ls()) #clear workspace

pwd <- getwd()
print(paste("Working in", pwd))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")
if (!requireNamespace("readxl", quietly = TRUE)) BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("fastDummies", quietly = TRUE)) BiocManager::install("fastDummies")
library("fastDummies")

print("Loaded packages")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "diet")
dir.create(output_dir)

#### Loading in data ####
id_var <- "Respondent sequence number"
#Load demographic data to remove children
demo_2015 <- download_org_nhanes(dt_group = "DEMO",
                                 nh_tble = "DEMO_I")
adult_seqn <- demo_2015[,id_var][
  which(demo_2015$`Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age`>18)]
print(paste("Number of participants:", nrow(demo_2015)))
print(paste("Number of adults:", length(adult_seqn)))
### Organize diet data, only individual foods first ####
d1_sup_simp_2015 <- download_org_nhanes("DIET", "DS1IDS_I")
d2_sup_simp_2015 <- download_org_nhanes("DIET", "DS2IDS_I")

# keep only adults in both days of supplemental data
d1_sup_simp_2015 <- d1_sup_simp_2015[d1_sup_simp_2015[,id_var] %in% adult_seqn, ]
d1_sup_simp_2015 <- type.convert(d1_sup_simp_2015, as.is = TRUE)#Reset column types automatically (for factors)

d2_sup_simp_2015 <- d2_sup_simp_2015[d2_sup_simp_2015[,id_var] %in% adult_seqn, ]
d2_sup_simp_2015 <- type.convert(d2_sup_simp_2015, as.is = TRUE)#Reset column types automatically (for factors)

#join the supplement names of both days together
sup_info_cols <- c(id_var, "Supplement Name", "Reported serving size/label serving size")
d1d2_sup_names_2015 <- rbind(d1_sup_simp_2015[,sup_info_cols, drop = FALSE],
                             d2_sup_simp_2015[,sup_info_cols, drop = FALSE])
d1d2_sup_names_2015 <- two_column_dummy(d1d2_sup_names_2015,
                                        id_var,
                                        "Supplement Name",
                                        "Reported serving size/label serving size")

nutri_food_cat_g_2015 <- read.csv(file.path(output_dir, "d1d2_nutri_food_g_cat_g_2015.csv"),
                                  header = TRUE,
                                  check.names = FALSE,
                                  row.names = "Respondent sequence number")
nutri_food_cat_g_2015[,id_var] <- row.names(nutri_food_cat_g_2015)

# The common columns between the food and the supplements are the nutrients
common_columns <- intersect(names(nutri_food_cat_g_2015), names(d1_sup_simp_2015))

sup_nutri_only <- rbind(d1_sup_simp_2015[,common_columns],
                        d2_sup_simp_2015[,common_columns])

sup_nutri_only <- aggregate(sup_nutri_only,
                            by = list(sup_nutri_only[,id_var]),
                            FUN = function(x) {sum(x, na.rm = TRUE)})

# The id var is now called "Group.1" - revert it back to old name. 
sup_nutri_only[,id_var] <- sup_nutri_only[,"Group.1"]
sup_nutri_only <- sup_nutri_only[,-which(names(sup_nutri_only) %in% c("Group.1"))]
row.names(sup_nutri_only) <- sup_nutri_only[,id_var]

# remove id var from common_columns so that it doesn't get put in the next df
common_columns <- common_columns[! common_columns %in% c(id_var)]

# Combine supplemental data with dietary data and revert to pre-merge row names
sup_diet_2015 <- merge(nutri_food_cat_g_2015, d1d2_sup_names_2015,
                       by = 0, all = TRUE)
row.names(sup_diet_2015) <- sup_diet_2015$Row.names

##### Sum values for nutrient data #####
for (cl in common_columns){
  print(cl)
  for (rw in row.names(sup_nutri_only)){
    if (rw %in% row.names(sup_diet_2015)){
      old_value <- sup_diet_2015[rw, cl]
      print(old_value)
      added_val <- sup_nutri_only[rw,cl]
      print(added_val)
      sup_diet_2015[rw,cl] <- sum(c(old_value, added_val), na.rm = TRUE)
    }
  }
}

# Revert column names again
sup_diet_2015 <- sup_diet_2015[, !(colnames(sup_diet_2015) %in% c("Row.names"))]

sup_diet_2015[,id_var] <- row.names(sup_diet_2015)

write.csv(sup_diet_2015,
          file.path("Data", "diet", "d1d2_nutri_cat_g_food_g_supp_dose.csv"),
          row.names = FALSE)
save_all_transforms(file.path("Data", "diet"),
                    "d1d2_nutri_cat_g_food_g_supp_dose",
                    sup_diet_2015)

print("End of R script.")
# 
# 
# # tot_sup_2015 <- download_org_nhanes("DIET", "DSQIDS_I")
# # # keep only adults
# # tot_sup_2015 <- tot_sup_2015[tot_sup_2015[,id_var] %in% adult_seqn, ]
# # tot_sup_2015 <- type.convert(tot_sup_2015, as.is = TRUE)#Reset column types automatically (for factors)
# 
# pdf(file.path("output", "sup_explore.pdf"))
# hist(table(tot_sup_2015$`For how long have you been taking {PRODUCT NAME} or a similar type of product`), breaks=200)
# hist(table(tot_sup_2015$`On the days that (you/SP) took (PRODUCT NAME), how much did (you/SP), usually take on a single day`), breaks=200)
# hist(table(tot_sup_2015$`Reported serving size/label serving size`), breaks=200)
# 
# # The supplement name is missing
# names(tot_sup_2015)[3] <- "supplement"
# 
# supp_time_2015 <- two_column_dummy(tot_sup_2015,
#                                    "Respondent sequence number",
#                                    "supplement",
#                                    "For how long have you been taking {PRODUCT NAME} or a similar type of product")
# 
# write.csv(supp_time_2015, file.path("Data", "diet", "supple_time_2015.csv"),
#           row.names = FALSE)
