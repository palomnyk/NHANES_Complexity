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
#E
multi_yr_diet <- data.frame()#empty df to fill up

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

  # # Day 2 diet
  # nh_tbl <-  import_table$d2_diet[rw]
  # fname <- file.path(output_dir, paste0(nh_tbl, "_", yr, ".csv"))
  # d2_diet <- nhanes_dl_save_if_not_local(fname, "DIET", nh_tbl)
  # 
  # print(paste(import_table$year_start[rw], "dif1:", length(setdiff(names(d1_diet), names(d2_diet)))))
  # print(paste(import_table$year_start[rw], "dif2:", length(setdiff(names(d2_diet), names(d1_diet)))))
  # 
  # 
  # # combine d1 and d2
  # # need to average values that come from both d1d2
  # d121_sqns <- intersect(d1_diet$`Respondent sequence number`, d2_diet$`Respondent sequence number`)
  # 
  # d1d2_diet <- rbind(d1_diet, d2_diet)
  d1d2_diet <- d1_diet
  my_cols <- names(d1d2_diet)
  names(my_cols) <- rep(import_table$year_start[rw], ncol(d1d2_diet))
  diet_table_cols <- c(diet_table_cols, my_cols)

  #Load demographic data to remove children
  nh_tbl <- import_table$demo[rw]
  fname <- file.path(output_dir, paste0(nh_tbl,"_", yr, ".csv"))
  demo_tab <- nhanes_dl_save_if_not_local(fname, "DEMO", nh_tbl)
  
  adult_seqn <- demo_tab[,id_var][which(demo_tab[,import_table$age_col[rw]] > 18)]
  print(paste("Number of participants:", nrow(demo_tab)))
  print(paste("Number of adults:", length(adult_seqn)))
  #keep only adults
  d1d2_diet <- d1d2_diet[d1d2_diet[,id_var] %in% adult_seqn, ]
  d1d2_diet <- type.convert(d1d2_diet, as.is = TRUE)#Reset column types automatically (for factors)
  unique_resp_ids <- sum(c(unique_resp_ids, 
                           length(unique(d1d2_diet$`Respondent sequence number`))))
  print(paste("nrow ncol d1d2:", nrow(d1d2_diet), ncol(d1d2_diet)))
  print(paste("nrow ncol multi:", nrow(multi_yr_diet), ncol(multi_yr_diet)))
  
  #Import codes for WWEIA categories 
  nh_tbl <-  import_table$code_table[rw]
  fname <- file.path(output_dir, paste0(nh_tbl, "_", yr, ".csv"))
  food_codes <- nhanes_dl_save_if_not_local(f_path = fname, dt_grp = "Q", nh_tbl = nh_tbl)
  d1d2_diet$food_name <- food_codes$`Short Food Code Description`[match(d1d2_diet$`USDA food code`, food_codes$`Food Code`)]
  print(paste("food_name in d1d2?", ("food_name" %in% names(d1d2_diet))))
  stopifnot("food_name" %in% names(d1d2_diet))
  if (nrow(multi_yr_diet) == 0){
    print("Replacing multi_yr_diet df")
    multi_yr_diet <- d1d2_diet
  }else{
    print("food_name" %in% names(multi_yr_diet))
    print(paste(import_table$year_start[rw], "dif1:", length(setdiff(names(d1d2_diet), names(multi_yr_diet)))))
    print(paste(import_table$year_start[rw], "dif2:", length(setdiff(names(multi_yr_diet), names(d1d2_diet)))))
    
    drop <- c(setdiff(names(multi_yr_diet), names(d1d2_diet)), setdiff(names(multi_yr_diet), names(d1d2_diet)))
    print(paste("Dropping from both d1d2 and multi year in ", yr, paste(drop, collapse =  " ")))
    d1d2_diet <- d1d2_diet[,!(names(d1d2_diet) %in% drop)]
    multi_yr_diet <- multi_yr_diet[,!(names(multi_yr_diet) %in% drop)]
    multi_yr_diet <- rbind(multi_yr_diet, d1d2_diet)
  }
}
#move food_name col to front
multi_yr_diet <- multi_yr_diet[,c(ncol(multi_yr_diet),1:(ncol(multi_yr_diet)-1))]

test <- data.frame(table(diet_table_cols))

unique_resp_ids == length(unique(multi_yr_diet$`Respondent sequence number`))

#organize nutrient variables first
nutr_vars <- c(21:ncol(multi_yr_diet))
nutr_multi_yr_diet <- rowsum(multi_yr_diet[,nutr_vars],
                       group = multi_yr_diet[,id_var], na.rm=T)
nutr_multi_yr_diet[,id_var] <- row.names(nutr_multi_yr_diet)

save_all_transforms(output_dir, "d1_nutr_only_2009-2020", nutr_multi_yr_diet)
# multi_yr_diet <- multi_yr_diet[, !sapply(multi_yr_diet, is.factor)]
#Make food grams dummys
USDA_food_g_only <- two_column_dummy(data.frame(multi_yr_diet, check.names = F),
                    id_colnm = "Respondent sequence number",
                    item_colnm = "food_name",
                    count_colnm = "Gram weight of the food/individual component")
print(paste("Number of adult diet participants:", nrow(USDA_food_g_only)))

save_all_transforms(output_dir, "d1_food_g_2009-2020", USDA_food_g_only)

nutri_food_g <- data.frame(merge(USDA_food_g_only, nutr_multi_yr_diet, by = id_var),
                           check.names=FALSE)
save_all_transforms(output_dir, "d1_nutri_food_g_2009-2020", nutri_food_g)

USDA_food_simple <- data.frame(subset(multi_yr_diet, select = c("food_name", id_var)),
                               check.names = FALSE)

USDA_food_simple <- fastDummies::dummy_cols(USDA_food_simple,
                                            select_columns = c("USDA food code"),
                                            ignore_na = TRUE, remove_selected_columns = TRUE)

USDA_food_simple <- rowsum(USDA_food_simple,
                             group = USDA_food_simple[,id_var],na.rm=T, )
USDA_food_simple[,id_var] <- row.names(USDA_food_simple)
save_all_transforms(output_dir, "d1_food_2009-2020", USDA_food_simple)

nutri_food_simple <- merge(USDA_food_simple, nutr_multi_yr_diet, by = id_var)
save_all_transforms(output_dir, "d1_nutri_food_2009-2020", nutri_food_simple)

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
