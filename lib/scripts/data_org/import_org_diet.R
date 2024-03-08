# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing dietary data
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
# Datasets to create:
#   A. Individual foods
#     a. alone (1) and with nutrients (2)
#   B. Food categories
#     a. alone (3) and with nutrients (4)
#   C. Nutients alone (5)
#   D. Individual foods grams
#     a. alone (6) and with nutrients (7)
#   F. Food categories grams
#     a. alone (8) and with nutrients (9)
  
rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")

print("Loaded dependencies")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "diet")
dir.create(output_dir)
# drop_criteria <- c("Dietary recall status" = c("Reported consuming breast-milk"),#DEMO_I
#                    ""
# )

#### Loading in data ####
food_codes <- readxl::read_excel(file.path("Data", "WWEIA1516_foodcat_FNDDS.xlsx"), 
                                 trim_ws = T, na = c("", "NA"))
id_var <- "Respondent sequence number"
#Load demographic data to remove children
demo_2015 <- download_org_nhanes(dt_group = "DEMO",
                                 nh_tble = "DEMO_I")
adult_seqn <- demo_2015[,id_var][
  which(demo_2015$`Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age`>=18)]
print(paste("Number of participants:", nrow(demo_2015)))
print(paste("Number of adults:", length(adult_seqn)))
#### Organize diet data, only individual foods first ####
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

dir.create(file.path(output_dir), showWarnings = FALSE)
write.csv(nutr_d1_diet_2015, file = file.path(output_dir, "d1_nutr_only_2015.csv"),
          row.names = FALSE)
# d1_diet_2015 <- d1_diet_2015[, !sapply(d1_diet_2015, is.factor)]
#Save usda food codes for overwriting
orig_fd_code <- d1_diet_2015$`USDA food code`
#Make food grams dummys
d1_diet_2015$`USDA food code` <- food_codes$food_code_description[match(orig_fd_code, food_codes$food_code)]

USDA_food_g_only <- two_column_dummy(d1_diet_2015,
                    id_colnm = "Respondent sequence number",
                    item_colnm = "USDA food code",
                    count_colnm = "Gram weight of the food/individual component")
print(paste("Number of adult diet participants:", nrow(USDA_food_g_only)))

write.csv(USDA_food_g_only, file = file.path(output_dir, "d1_USDA_food_g_2015.csv"),
          row.names = FALSE)

nutri_food_g <- merge(USDA_food_g_only, nutr_d1_diet_2015, by = id_var)
write.csv(nutri_food_g, file = file.path(output_dir, "d1_nutri_food_g_2015.csv"),
          row.names = FALSE)

USDA_food_simple <- subset(d1_diet_2015, select = c("USDA food code", id_var))

USDA_food_simple <- fastDummies::dummy_cols(USDA_food_simple,
                                              select_columns = c("USDA food code"),
                                              ignore_na = TRUE, remove_selected_columns = TRUE)

USDA_food_simple <- rowsum(USDA_food_simple,
                             group = USDA_food_simple[,id_var],na.rm=T, )
USDA_food_simple[,id_var] <- row.names(USDA_food_simple)

write.csv(USDA_food_simple, file = file.path(output_dir, "d1_food_2015.csv"),
          row.names = FALSE)

nutri_food_simple <- merge(USDA_food_simple, nutr_d1_diet_2015, by = id_var)
write.csv(nutri_food_simple, file = file.path(output_dir, "d1_nutri_food_2015.csv"),
          row.names = FALSE)

# d1_tot_diet_2015 <- download_org_nhanes(dt_group = "DIET", nh_tble = "DR1TOT_I")

#### Food categories ####
d1_diet_2015$`USDA food code` <- food_codes$category_description[match(orig_fd_code, food_codes$food_code)]

USDA_food_g_only <- two_column_dummy(d1_diet_2015,
                                     id_colnm = "Respondent sequence number",
                                     item_colnm = "USDA food code",
                                     count_colnm = "Gram weight of the food/individual component")
print(paste("Number of adult diet participants:", nrow(USDA_food_g_only)))

write.csv(USDA_food_g_only, file = file.path(output_dir, "d1_USDA_cat_g_2015.csv"),
          row.names = FALSE)

nutri_food_g <- merge(USDA_food_g_only, nutr_d1_diet_2015, by = id_var)
write.csv(nutri_food_g, file = file.path(output_dir, "d1_nutri_cat_g_2015.csv"),
          row.names = FALSE)

USDA_food_simple <- subset(d1_diet_2015, select = c("USDA food code", id_var))

USDA_food_simple <- fastDummies::dummy_cols(USDA_food_simple,
                                            select_columns = c("USDA food code"),
                                            ignore_na = TRUE, remove_selected_columns = TRUE)

USDA_food_simple <- rowsum(USDA_food_simple,
                           group = USDA_food_simple[,id_var],na.rm=T, )
USDA_food_simple[,id_var] <- row.names(USDA_food_simple)

write.csv(USDA_food_simple, file = file.path(output_dir, "d1_cat_2015.csv"),
          row.names = FALSE)

nutri_food_simple <- merge(USDA_food_simple, nutr_d1_diet_2015, by = id_var)
write.csv(nutri_food_simple, file = file.path(output_dir, "d1_nutri_cat_2015.csv"),
          row.names = FALSE)

