# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing dietary data
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
# Datasets to create:
# A. Individual foods
#   a. alone (1) and with nutrients (2)
# B. Food categories
#   a. alone (3) and with nutrients (4)
# C. Nutients alone (5)
# D. Individual foods grams
#   a. alone (6) and with nutrients (7)
# F. Food categories grams
#   a. alone (8) and with nutrients (9)
# G. Category grams + food grams + nutrient (10)
# H. Category simple + food simple + nutrient (11)

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
food_codes <- readxl::read_excel(file.path("Data", "diet", "WWEIA1516_foodcat_FNDDS.xlsx"), 
                                 trim_ws = T, na = c("", "NA"))
id_var <- "Respondent sequence number"
#Load demographic data to remove children
demo_2015 <- download_org_nhanes(dt_group = "DEMO",
                                 nh_tble = "DEMO_I")
adult_seqn <- demo_2015[,id_var][
  which(demo_2015$`Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age`>18)]
print(paste("Number of participants:", nrow(demo_2015)))
print(paste("Number of adults:", length(adult_seqn)))
#### Organize diet data, only individual foods first ####
d1_diet_2015 <- download_org_nhanes("DIET", "DR1IFF_I")
# Day 2 diet
d2_diet_2015 <- download_org_nhanes("DIET", "DR2IFF_I")

setdiff(names(d1_diet_2015), names(d2_diet_2015))
setdiff(names(d2_diet_2015), names(d1_diet_2015))

#combine d1 and d2
d1d2_diet_2015 <- rbind(d1_diet_2015, d2_diet_2015)

print(paste("nrow ncol d1:", nrow(d1_diet_2015), ncol(d1_diet_2015)))
print(paste("nrow ncol d2:", nrow(d2_diet_2015), ncol(d2_diet_2015)))
print(paste("nrow ncol d1d2:", nrow(d1d2_diet_2015), ncol(d1d2_diet_2015)))

#keep only adults
d1d2_diet_2015 <- d1d2_diet_2015[d1d2_diet_2015[,id_var] %in% adult_seqn, ]
d1d2_diet_2015 <- type.convert(d1d2_diet_2015, as.is = TRUE)#Reset column types automatically (for factors)

#organize nutrient variables first
nutr_vars <- c(20:ncol(d1d2_diet_2015))
nutr_d1d2_diet_2015 <- rowsum(d1d2_diet_2015[,nutr_vars],
                       group = d1d2_diet_2015[,id_var], na.rm=T)
nutr_d1d2_diet_2015[,id_var] <- row.names(nutr_d1d2_diet_2015)

dir.create(file.path(output_dir), showWarnings = FALSE)
save_all_transforms(output_dir, "d1d2_nutr_only_2015", nutr_d1d2_diet_2015)

num_only <- nutr_d1d2_diet_2015[,which(! names(nutr_d1d2_diet_2015) %in% c(id_var))]
min_val <- min(num_only[num_only > 0])



# d1d2_diet_2015 <- d1d2_diet_2015[, !sapply(d1d2_diet_2015, is.factor)]
#Save usda food codes for overwriting
orig_fd_code <- d1d2_diet_2015$`USDA food code`
#Make food grams dummys
d1d2_diet_2015$`USDA food code` <- food_codes$food_code_description[match(orig_fd_code, food_codes$food_code)]

USDA_food_g_only <- two_column_dummy(d1d2_diet_2015,
                    id_colnm = "Respondent sequence number",
                    item_colnm = "USDA food code",
                    count_colnm = "Gram weight of the food/individual component")
print(paste("Number of adult diet participants:", nrow(USDA_food_g_only)))

save_all_transforms(output_dir, "d1d2_food_g_2015", USDA_food_g_only)

nutri_food_g <- merge(USDA_food_g_only, nutr_d1d2_diet_2015, by = id_var)
save_all_transforms(output_dir, "d1d2_nutri_food_g_2015", nutri_food_g)

USDA_food_simple <- data.frame(
  subset(d1d2_diet_2015, select = c("USDA food code", id_var)), check.names = F)

USDA_food_simple <- fastDummies::dummy_cols(USDA_food_simple,
                                              select_columns = c("USDA food code"),
                                              ignore_na = TRUE, remove_selected_columns = TRUE)

USDA_food_simple$`USDA food code` <- as.character(USDA_food_simple$`USDA food code`)

USDA_food_simple <- rowsum(USDA_food_simple,
                             group = USDA_food_simple[,id_var],na.rm=T, )
USDA_food_simple[,id_var] <- row.names(USDA_food_simple)

save_all_transforms(output_dir, "d1d2_food_2015", USDA_food_simple)

nutri_food_simple <- merge(USDA_food_simple, nutr_d1d2_diet_2015, by = id_var)
save_all_transforms(output_dir, "d1d2_nutri_food_2015", nutri_food_simple)
# d1_tot_diet_2015 <- download_org_nhanes(dt_group = "DIET", nh_tble = "DR1TOT_I")

#### Food categories ####
d1d2_diet_2015$`USDA food code` <- food_codes$category_description[match(orig_fd_code, food_codes$food_code)]

cat_g_only <- two_column_dummy(d1d2_diet_2015,
                                     id_colnm = "Respondent sequence number",
                                     item_colnm = "USDA food code",
                                     count_colnm = "Gram weight of the food/individual component")
print(paste("Number of adult diet participants:", nrow(cat_g_only)))
save_all_transforms(output_dir, "d1d2_cat_g_2015", cat_g_only)

nutri_food_g <- merge(cat_g_only, nutr_d1d2_diet_2015, by = id_var)
save_all_transforms(output_dir, "d1d2_nutri_cat_g_2015", nutri_food_g)

cat_simple <- subset(d1d2_diet_2015, select = c("USDA food code", id_var))

cat_simple <- fastDummies::dummy_cols(cat_simple,
                                            select_columns = c("USDA food code"),
                                            ignore_na = TRUE, remove_selected_columns = TRUE)

cat_simple <- rowsum(cat_simple,
                           group = cat_simple[,id_var],na.rm=T, )
cat_simple[,id_var] <- row.names(cat_simple)
save_all_transforms(output_dir, "d1d2_cat_2015", cat_simple)

nutri_food_simple <- merge(cat_simple, nutr_d1d2_diet_2015, by = id_var)
save_all_transforms(output_dir, "d1d2_nutri_cat_2015", nutri_food_simple)

# G. Category grams + food grams + nutrient (10)
nutri_food_g_cat_g <- merge(nutri_food_g, USDA_food_g_only)
save_all_transforms(output_dir, "d1d2_nutri_food_g_cat_g_2015", nutri_food_g_cat_g)

# H. Category simple + food simple + nutrient (11)
nutri_food_cat <- merge(nutri_food_simple, USDA_food_simple)
save_all_transforms(output_dir, "d1d2_nutri_food_cat_2015", nutri_food_cat)

print("End R script.")
