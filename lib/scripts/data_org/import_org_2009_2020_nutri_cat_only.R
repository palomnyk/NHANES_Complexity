# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing dietary data from multiple years
  
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
in_dir <- file.path("lib", "datasets")
output_dir <- file.path("Data", "diet", "multi_year")
dir.create(output_dir)
id_var <- "Respondent sequence number"
import_table <- read.csv(file = file.path(in_dir, "years_diet_tables.csv"),
                        header = TRUE)
#Empty vars to fill up
multi_yr_diet <- data.frame()#empty df to fill up
all_d1d2_sqns <- c()

#array of all column names for troubleshooting
diet_table_cols <- c()
unique_resp_ids <- 0

for (rw in 1:nrow(import_table)){
  print(rw)
  #### Read both days of diet  ####
  yr <- import_table$year_start[rw]
  nh_tbl <-  import_table$d1_diet[rw]
  fname <- file.path(output_dir, paste0(nh_tbl, "_", yr, ".csv"))
  d1_diet <- nhanes_dl_save_if_not_local(f_path = fname, dt_grp = "DIET", nh_tbl = nh_tbl)

  # Day 2 diet
  nh_tbl <-  import_table$d2_diet[rw]
  fname <- file.path(output_dir, paste0(nh_tbl, "_", yr, ".csv"))
  d2_diet <- nhanes_dl_save_if_not_local(fname, "DIET", nh_tbl)
  
  print(paste(import_table$year_start[rw], "dif1:", length(setdiff(names(d1_diet), names(d2_diet)))))
  print(paste(import_table$year_start[rw], "dif2:", length(setdiff(names(d2_diet), names(d1_diet)))))
  
  ##### Combine diet day dataframes #####
  # Record which sqns are d1d2, all others are single day
  d1d2_sqns <- intersect(d1_diet$`Respondent sequence number`, d2_diet$`Respondent sequence number`)
  all_d1d2_sqns <- c(all_d1d2_sqns, d1d2_sqns)
  # Check for day 2 sqns that don't have day 1 partner
  print(paste("testing", yr))
  if (! all(d2_diet$`Respondent sequence number` %in% d1d2_sqns)){
    orphan_d2_sqn <- setdiff(d2_diet$`Respondent sequence number`, d1d2_sqns)
    print(paste(yr, "orphan_d2_sqn:", paste(orphan_d2_sqn, collapse = ",")))
    }
  combined_diets <- rbind(d1_diet, d2_diet)
  
  # record columns for troubleshooting
  my_cols <- names(combined_diets)
  names(my_cols) <- rep(import_table$year_start[rw], ncol(combined_diets))
  diet_table_cols <- c(diet_table_cols, my_cols)

  ##### Remove children #####
  #Load demographic data to remove children
  nh_tbl <- import_table$demo[rw]
  fname <- file.path(output_dir, paste0(nh_tbl,"_", yr, ".csv"))
  demo_tab <- nhanes_dl_save_if_not_local(fname, "DEMO", nh_tbl)

  adult_seqn <- demo_tab[,id_var][which(demo_tab[,import_table$age_col[rw]] > 18)]
  print(paste("Number of participants:", nrow(demo_tab)))
  print(paste("Number of adults:", length(adult_seqn)))
  #keep only adults
  combined_diets <- combined_diets[combined_diets[,id_var] %in% adult_seqn, ]
  combined_diets <- type.convert(combined_diets, as.is = TRUE)#Reset column types automatically (for factors)
  unique_resp_ids <- sum(c(unique_resp_ids,
                           length(unique(combined_diets$`Respondent sequence number`))))
  print(paste("nrow ncol d1:", nrow(d1_diet), ncol(d1_diet)))
  print(paste("nrow ncol d2:", nrow(d2_diet), ncol(d2_diet)))
  print(paste("nrow ncol d1d2:", nrow(combined_diets), ncol(combined_diets)))
  print(paste("nrow ncol multi:", nrow(multi_yr_diet), ncol(multi_yr_diet)))

  ##### Label food names #####
  nh_tbl <-  import_table$code_table[rw]
  fname <- file.path(output_dir, paste0(nh_tbl, "_", yr, ".csv"))
  food_codes <- nhanes_dl_save_if_not_local(f_path = fname, dt_grp = "Q", nh_tbl = nh_tbl)
  combined_diets$food_name <- food_codes$`Short Food Code Description`[match(combined_diets$`USDA food code`, food_codes$`Food Code`)]
  print(paste("food_name in d1d2?", ("food_name" %in% names(combined_diets))))
  stopifnot("food_name" %in% names(combined_diets))
  ##### Label WWEIA categories #####
  cat_path <- file.path("Data","diet","wweia_food_category", import_table$wweia_cats[rw])
  cat_df <- readxl::read_excel(cat_path, sheet = 1)
  combined_diets$food_cat <- cat_df$category_description[match(combined_diets$`USDA food code`, cat_df$`food_code`)]
  if (nrow(multi_yr_diet) == 0){
    print("Replacing multi_yr_diet df")
    multi_yr_diet <- combined_diets
  }else{
    print("food_name" %in% names(multi_yr_diet))
    print(paste(import_table$year_start[rw], "dif1:", length(setdiff(names(combined_diets), names(multi_yr_diet)))))
    print(paste(import_table$year_start[rw], "dif2:", length(setdiff(names(multi_yr_diet), names(combined_diets)))))

    drop <- c(setdiff(names(multi_yr_diet), names(combined_diets)), setdiff(names(multi_yr_diet), names(combined_diets)))
    print(paste("Dropping from both d1d2 and multi year in ", yr, paste(drop, collapse =  " ")))
    combined_diets <- combined_diets[,!(names(combined_diets) %in% drop)]
    multi_yr_diet <- multi_yr_diet[,!(names(multi_yr_diet) %in% drop)]
    multi_yr_diet <- rbind(multi_yr_diet, combined_diets)
  }
}
#### Organized all years dataset ####
#move food_name and cat_name columns to front
multi_yr_diet <- multi_yr_diet[,c(ncol(multi_yr_diet),1:(ncol(multi_yr_diet)-2))]


test <- data.frame(table(diet_table_cols))

unique_resp_ids == length(unique(multi_yr_diet$`Respondent sequence number`))

# organize nutrient variables first
nutr_vars <- c(21:ncol(multi_yr_diet))
nutr_multi_yr_diet <- rowsum(multi_yr_diet[,nutr_vars],
                       group = multi_yr_diet[,id_var], na.rm=T)
nutr_multi_yr_diet[,id_var] <- row.names(nutr_multi_yr_diet)
# Add the days of data to nutr data
nutr_multi_yr_diet$days_collected <- unlist(lapply(nutr_multi_yr_diet$`Respondent sequence number`, function(sqn){
  ifelse(sqn %in% all_d1d2_sqns, 2, 1)}))

#### Create nutritional variation column ####
num_uniq_foods <- aggregate(`USDA food code` ~ `Respondent sequence number`, data = multi_yr_diet, FUN = length)
names(num_uniq_foods)[names(num_uniq_foods) == "USDA food code"] <- "num_uniq_foods"

save_df <- merge(nutr_multi_yr_diet, num_uniq_foods, by = "Respondent sequence number")
write.csv(save_df, file = file.path(output_dir, "all_days-nutr_only-2009_2020.csv"),
          row.names = FALSE)

#### Make food category grams ####
cat_g_only <- two_column_dummy(data.frame(multi_yr_diet, check.names = F),
                                     id_colnm = "Respondent sequence number",
                                     item_colnm = "food_cat",
                                     count_colnm = "Gram weight of the food/individual component")
print(paste("Number of adult diet participants:", nrow(cat_g_only)))

save_df <- merge(cat_g_only, num_uniq_foods, by = "Respondent sequence number")
write.csv(save_df, file = file.path(output_dir, "all_days-cat_g-2009_2020.csv"),
          row.names = FALSE)

all_days_nutri_cat_g <- merge(cat_g_only, num_uniq_foods, by = "Respondent sequence number")

save_df <- merge(all_days_nutri_cat_g, num_uniq_foods, by = "Respondent sequence number")
write.csv(save_df, file = file.path(output_dir, "all_days-nutri_cat_g-2009_2020.csv"),
          row.names = FALSE)
# #Make food grams dummys
# USDA_food_g_only <- two_column_dummy(data.frame(multi_yr_diet, check.names = F),
#                     id_colnm = "Respondent sequence number",
#                     item_colnm = "food_cat",
#                     count_colnm = "Gram weight of the food/individual component")
# print(paste("Number of adult diet participants:", nrow(USDA_food_g_only)))
# 
# save_all_transforms(output_dir, "d1d2_food_g_2009-2020", USDA_food_g_only)
# 
# nutri_food_g <- data.frame(merge(USDA_food_g_only, nutr_multi_yr_diet, by = id_var),
#                            check.names=FALSE)
# save_all_transforms(output_dir, "d1d2_nutri_food_g_2009-2020", nutri_food_g)
# 
# USDA_food_simple <- data.frame(subset(multi_yr_diet, select = c("food_name", id_var)),
#                                check.names = FALSE)
# 
# USDA_food_simple <- simple_dummy(USDA_food_simple, "food_name", id_var, "_")
# 
# USDA_food_simple <- rowsum(USDA_food_simple,
#                              group = USDA_food_simple[,id_var],na.rm=T, )
# USDA_food_simple[,id_var] <- row.names(USDA_food_simple)
# save_all_transforms(output_dir, "d1d2_food_2009-2020", USDA_food_simple)
# 
# nutri_food_simple <- merge(USDA_food_simple, nutr_multi_yr_diet, by = id_var)
# save_all_transforms(output_dir, "d1d2_nutri_food_2009-2020", nutri_food_simple)

#### Food categories ####
# cat_g_only <- two_column_dummy(d1d2_diet_2015,
#                                      id_colnm = "Respondent sequence number",
#                                      item_colnm = "USDA food code",
#                                      count_colnm = "Gram weight of the food/individual component")
# print(paste("Number of adult diet participants:", nrow(cat_g_only)))
#
# write.csv(cat_g_only, file = file.path(output_dir, "d1d2_cat_g_2015.csv"),
#           row.names = FALSE)
#
# nutri_food_g <- merge(cat_g_only, nutr_d1d2_diet_2015, by = id_var)
# write.csv(nutri_food_g, file = file.path(output_dir, "d1d2_nutri_cat_g_2015.csv"),
#           row.names = FALSE)
#
# cat_simple <- subset(d1d2_diet_2015, select = c("USDA food code", id_var))
#
# cat_simple <- fastDummies::dummy_cols(cat_simple,
#                                             select_columns = c("USDA food code"),
#                                             ignore_na = TRUE, remove_selected_columns = TRUE)
#
# cat_simple <- rowsum(cat_simple,
#                            group = cat_simple[,id_var],na.rm=T, )
# cat_simple[,id_var] <- row.names(cat_simple)
#
# write.csv(cat_simple, file = file.path(output_dir, "d1d2_cat_2015.csv"),
#           row.names = FALSE)
#
# nutri_food_simple <- merge(cat_simple, nutr_d1d2_diet_2015, by = id_var)
# write.csv(nutri_food_simple, file = file.path(output_dir, "d1d2_nutri_cat_2015.csv"),
#           row.names = FALSE)
# # G. Category grams + food grams + nutrient (10)
# nutri_food_g_cat_g <- merge(nutri_food_g, USDA_food_g_only)
# write.csv(nutri_food_g_cat_g, file = file.path(output_dir, "d1d2_nutri_food_g_cat_g_2015.csv"),
#           row.names = FALSE)
# # H. Category simple + food simple + nutrient (11)
# nutri_food_cat <- merge(nutri_food_simple, USDA_food_simple)
# write.csv(nutri_food_cat, file = file.path(output_dir, "d1d2_nutri_food_cat_2015.csv"),
#           row.names = FALSE)

print("End R script.")
