# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing dietary data
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
# Datasets to create:
# A. Individual foods
#   a. alone (1) and with nutrients (2)
# B. Food categories
#   a. alone (3) and with nutrients (4)
# C. Nutrients alone (5)
# D. Individual foods grams
#   a. alone (6) and with nutrients (7)
# F. Food categories grams
#   a. alone (8) and with nutrients (9)
# G. Category grams + food grams + nutrient (10)
# H. Category simple + food simple + nutrient (11)

rm(list = ls()) #clear workspace

chooseCRANmirror(graphics=FALSE, ind=66)

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) install.packages("nhanesA")
library("nhanesA")
if (!requireNamespace("readxl", quietly = TRUE)) BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("fastDummies", quietly = TRUE)) BiocManager::install("fastDummies")
library("fastDummies")

print("Loaded packages")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "diet")
dir.create(file.path(output_dir), showWarnings = FALSE)

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
#### Organize diet data ####
# d1_diet_2015 <- download_org_nhanes("DIET", "DR1IFF_I")
d1_diet_2015 <- nhanesA::nhanes("DR1IFF_I")
d1_diet_2015 <- nhanesA::nhanesTranslate(nh_table = "DR1IFF_I",
                                  data = d1_diet_2015,
                                  details = TRUE,
                                  colnames = names(d1_diet_2015))
d1_diet_2015 <- nhanes_names(d1_diet_2015,"DIET", "DR1IFF_I")
attr(d1_diet_2015, "names") <- sub("[[:punct:]]$", "", names(d1_diet_2015))
#keep only adults
d1_diet_2015 <- d1_diet_2015[d1_diet_2015[,id_var] %in% adult_seqn, ]
d1_diet_2015 <- type.convert(d1_diet_2015, as.is = TRUE)#Reset column types automatically (for factors)

nutr_vars <- c(20:ncol(d1_diet_2015))
nutr_d1_diet_2015 <- rowsum(d1_diet_2015[,nutr_vars],
                       group = d1_diet_2015[,id_var], na.rm=T)
nutr_d1_diet_2015[,id_var] <- row.names(nutr_d1_diet_2015)

my_sqns <- unique(d1_diet_2015$`Respondent sequence number`)
same_energy <- vector(length = length(my_sqns))

##### Check if the macro nutrients sum to proper energy level #####
for (sqn in 1:length(my_sqns)){
  the_sqn <- my_sqns[sqn]
  multi_en <- d1_diet_2015[d1_diet_2015$`Respondent sequence number` == the_sqn,"Energy (kcal"]
  same_energy[sqn] <- (sum(multi_en) == nutr_d1_diet_2015[nutr_d1_diet_2015$`Respondent sequence number` == the_sqn, "Energy (kcal"])
}

prot_cal <- nutr_d1_diet_2015$`Protein (gm` * 4
carb_cal <- nutr_d1_diet_2015$`Carbohydrate (gm` * 4
fat_cal <- nutr_d1_diet_2015$`Total fat (gm` * 9

###### Add calculated Atwater general_energy to nutrition data ######
nutr_d1_diet_2015$Atwater_gen_energy <- prot_cal + carb_cal + fat_cal

sum(nutr_d1_diet_2015$`Energy (kcal`)/sum(c(prot_cal, carb_cal,fat_cal)) - 1

prot_cal <- d1_diet_2015$`Protein (gm` * 4
carb_cal <- d1_diet_2015$`Carbohydrate (gm` * 4
fat_cal <- d1_diet_2015$`Total fat (gm` * 9

sum(d1_diet_2015$`Energy (kcal`)/sum(c(prot_cal, carb_cal,fat_cal)) - 1

##### Create nutritional variation column #####
num_uniq_foods <- aggregate(`USDA food code` ~ `Respondent sequence number`, data = d1_diet_2015, FUN = length)
names(num_uniq_foods)[names(num_uniq_foods) == "USDA food code"] <- "num_uniq_foods"
save_df <- merge(nutr_d1_diet_2015, num_uniq_foods, by = "Respondent sequence number")
write.csv(save_df, file = file.path(output_dir, "d1-nutr_only-2015.csv"),
          row.names = FALSE)

# Save usda food codes for overwriting 
orig_fd_code <- d1_diet_2015$`USDA food code`
##### Make food grams dummys #####
d1_diet_2015$`USDA food code` <- food_codes$food_code_description[match(orig_fd_code, food_codes$food_code)]

USDA_food_g_only <- two_column_dummy(d1_diet_2015,
                    id_colnm = "Respondent sequence number",
                    item_colnm = "USDA food code",
                    count_colnm = "Gram weight of the food/individual component")
print(paste("Number of adult diet participants:", nrow(USDA_food_g_only)))

save_df <- merge(USDA_food_g_only, num_uniq_foods, by = "Respondent sequence number")
write.csv(save_df, file = file.path(output_dir, "d1-food_g-2015.csv"),
          row.names = FALSE)

nutri_food_g <- merge(USDA_food_g_only, nutr_d1_diet_2015, by = id_var)
save_df <- merge(nutri_food_g, num_uniq_foods, by = id_var)
write.csv(save_df, file = file.path(output_dir, "d1-nutri_food_g-2015.csv"),
          row.names = FALSE)

USDA_food_simple <- subset(d1_diet_2015, select = c("USDA food code", id_var))

USDA_food_simple <- fastDummies::dummy_cols(USDA_food_simple,
                                              select_columns = c("USDA food code"),
                                              ignore_na = TRUE, remove_selected_columns = TRUE)

USDA_food_simple <- rowsum(USDA_food_simple,
                             group = USDA_food_simple[,id_var],na.rm=T, )
USDA_food_simple[,id_var] <- row.names(USDA_food_simple)

save_df <- merge(USDA_food_simple, num_uniq_foods, by = id_var)
write.csv(save_df, file = file.path(output_dir, "d1-food-2015.csv"),
          row.names = FALSE)

nutri_food_simple <- merge(USDA_food_simple, nutr_d1_diet_2015, by = id_var)
save_df <- merge(USDA_food_simple, num_uniq_foods, by = id_var)
write.csv(save_df, file = file.path(output_dir, "d1-nutri_food-2015.csv"),
          row.names = FALSE)

# d1_tot_diet_2015 <- download_org_nhanes(dt_group = "DIET", nh_tble = "DR1TOT_I")

#### Food categories ####
d1_diet_2015$`USDA food code` <- food_codes$category_description[match(orig_fd_code, food_codes$food_code)]

print(head(d1_diet_2015$`USDA food code`))

cat_g_only <- two_column_dummy(d1_diet_2015,
                                     id_colnm = "Respondent sequence number",
                                     item_colnm = "USDA food code",
                                     count_colnm = "Gram weight of the food/individual component")
print(paste("Number of adult diet participants:", nrow(cat_g_only)))
save_df <- merge(cat_g_only, num_uniq_foods, by = id_var)
write.csv(cat_g_only, file = file.path(output_dir, "d1-cat_g-2015.csv"),
          row.names = FALSE)

nutri_food_g <- merge(cat_g_only, nutr_d1_diet_2015, by = id_var)
save_df <- merge(nutri_food_g, num_uniq_foods, by = id_var)
write.csv(save_df, file = file.path(output_dir, "d1-nutri_cat_g-2015.csv"),
          row.names = FALSE)

cat_simple <- subset(d1_diet_2015, select = c("USDA food code", id_var))

cat_simple <- fastDummies::dummy_cols(cat_simple,
                                            select_columns = c("USDA food code"),
                                            ignore_na = TRUE, remove_selected_columns = TRUE)

cat_simple <- rowsum(cat_simple,
                           group = cat_simple[,id_var],na.rm=T, )
cat_simple[,id_var] <- row.names(cat_simple)
save_df <- merge(cat_simple, num_uniq_foods, by = id_var)
write.csv(save_df, file = file.path(output_dir, "d1-cat-2015.csv"),
          row.names = FALSE)

nutri_food_simple <- merge(cat_simple, nutr_d1_diet_2015, by = id_var)
save_df <- merge(nutri_food_simple, num_uniq_foods, by = id_var)
write.csv(save_df, file = file.path(output_dir, "d1-nutri_cat-2015.csv"),
          row.names = FALSE)
##### G. Category grams + food grams + nutrient (10) #####
nutri_food_g_cat_g <- merge(nutri_food_g, USDA_food_g_only)
save_df <- merge(nutri_food_g_cat_g, num_uniq_foods, by = id_var)
write.csv(save_df, file = file.path(output_dir, "d1-nutri_food_g_cat_g-2015.csv"),
          row.names = FALSE)
##### H. Category simple + food simple + nutrient (11) #####
nutri_food_cat <- merge(nutri_food_simple, USDA_food_simple)
save_df <- merge(nutri_food_cat, num_uniq_foods, by = id_var)
write.csv(save_df, file = file.path(output_dir, "d1-nutri_food_cat-2015.csv"),
          row.names = FALSE)

print("Reached end of R script")
