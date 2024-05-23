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
if (!requireNamespace("reshape2", quietly = TRUE)) BiocManager::install("reshape2")
library("reshape2")

print("Loaded packages")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Establish directory layout and other constants ####
output_dir <- file.path("Data")
dir.create(output_dir)

#### Loading in data ####
id_var <- "Respondent sequence number"

cor_thresh <- 0.9

sup_diet_2015 <- read.csv(file.path("Data", "diet", "d1d2_nutri_cat_g_food_g_supp_dose.csv"),
                          check.names = FALSE, row.names = id_var)

other_NHANES <- read.csv(file = file.path("Data", "nhanesA_tables", "combined_NHANES_dum.csv"),
                         check.names = FALSE, row.names = id_var)

print(paste("DIET SUPP NCOL, NROW, %NA", ncol(sup_diet_2015), nrow(sup_diet_2015), mean(is.na(sup_diet_2015))))

final_table <- merge(sup_diet_2015, other_NHANES, all = TRUE, by = 0)

print(paste("FINAL NCOL, NROW, %NA", ncol(final_table), nrow(final_table), mean(is.na(final_table))))

names(final_table)[1] <- id_var

not_nums <- !unlist(lapply(final_table, is.numeric), use.names = FALSE)  
which(not_nums == TRUE)
upper_tri <- cor(final_table[,2:ncol(final_table)])

upper_tri[lower.tri(upper_tri)] <- NA

melted_cormat <- melt(upper_tri, na.rm = TRUE)

over_cor <- subset(melted_cormat, melted_cormat$Var1 != melted_cormat$Var2)
over_cor <- subset(over_cor, value > cor_thresh)

write.csv(over_cor, file = file.path(output_dir, paste0("over_cor_",cor_thresh,"_final_all.csv")),
          row.names = FALSE)

final_table <- final_table[, !colnames(final_table) %in% over_cor$Var2] #drop highly correlated variables

write.csv(final_table, file = file.path(output_dir, "diet_supp_combined_NHANES.csv"),
          row.names = FALSE)

print("End of R script.")
