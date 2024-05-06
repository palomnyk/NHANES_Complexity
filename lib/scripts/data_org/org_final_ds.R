# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for combining dietary data with other datasets

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
output_dir <- file.path("Data")
dir.create(output_dir)

#### Loading in data ####
id_var <- "Respondent sequence number"

sup_diet_2015 <- read.csv(file.path("Data", "diet", "d1d2_nutri_cat_g_food_g_supp_dose.csv"),
                          check.names = FALSE, row.names = id_var)

other_NHANES <- read.csv(file = file.path("Data", "nhanesA_tables", "combined_NHANES_dum.csv"),
                         check.names = FALSE, row.names = id_var)

final_table <- merge(sup_diet_2015, other_NHANES, all = TRUE, by = 0)

names(final_table)[1] <- id_var
write.csv(final_table, file = file.path(output_dir, "diet_supp_combined_NHANES.csv"),
          row.names = FALSE)

print("End of R script.")
