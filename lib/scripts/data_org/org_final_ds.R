# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for combining dietary data with other datasets
# Usage:
# Rscript lib/scripts/data_org/org_final_ds.R\
#   --output_prefix diet_\
#   --input_table Data/diet_combined_NHANES_dum.csv

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

if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

source(file.path("lib", "scripts","data_org", "data_org_func.R"))

print("Loaded dependencies")

#### Read commandline arguements ####
option_list <- list(
  optparse::make_option(c("-p", "--output_prefix"), type="character", default="", 
                        help="prefix for output", metavar="character"),
  optparse::make_option(c("-i", "--input_table"), type="character",
                        default=file.path("Data", "nhanesA_tables", "combined_NHANES_dum.csv"),
                        help="relative path, needs to be comma seperated with header",
                        metavar="character")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print(opt)
print("Done reading cml arguments")

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

write.csv(over_cor, file = file.path(output_dir,
                                     paste0(opt$output_prefix,"over_cor_",cor_thresh,"_final_all.csv")),
          row.names = FALSE)

final_table <- final_table[, !colnames(final_table) %in% over_cor$Var2] #drop highly correlated variables

save_all_transforms(output_dir,
                    paste0(opt$output_prefix,"diet_supp_combined_NHANES"),
                    final_table)

print("End of R script.")
