# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for dividing the data into quintiles by age and looking for
# correlations to features in the 2009-2020 nutrition data.

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("readxl", quietly = TRUE)) BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("ggplot2", quietly = TRUE)) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
if (!requireNamespace("reshape2", quietly = TRUE)) BiocManager::install("reshape2")
library("reshape2")

print("Loaded dependencies")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Functions ####

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("diet_test"), 
                        help="dataset dir path")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path("output", opt$out_subdir)

numeric_only <- c("Total Cholesterol (mg/dL","Triglyceride (mg/dL", 
                  "LDL-cholesterol (mg/dL","Direct HDL-Cholesterol (mg/dL",
                  "Systolic_mean", "Diastolic_mean")
id_var <- "Respondent sequence number"

#### Loading in data ####
demo_2009_2020 <- read.csv("Data/demo/demo_2009-2020.csv", header = TRUE)
diet_df <- read.csv("Data/diet/multi_year/d1d2_nutr_only_2009-2020.csv",
                     header = TRUE, check.names = FALSE)
resp_df <- read.csv("Data/respns_vars/2009-2020cardio_respns_vars.csv",
                    header = TRUE,check.names = FALSE)

#### Plot BP vs Sodium ####
shared_seqs <- intersect(diet_df[!is.na(diet_df[,"Sodium (mg"]),id_var],
                         resp_df[!is.na(resp_df[,"Systolic_mean"]),id_var])
plot(diet_df[diet_df[,id_var] %in% shared_seqs,"Sodium (mg"], 
     resp_df[resp_df[,id_var] %in% shared_seqs,"Systolic_mean"])
cor(diet_df[diet_df[,id_var] %in% shared_seqs,"Sodium (mg"], 
    resp_df[resp_df[,id_var] %in% shared_seqs,"Systolic_mean"])

male_subdf <- demo_2009_2020[demo_2009_2020$gender == "Male" &
                             demo_2009_2020$age >= 18,]
male_age_quantile_size <- (max(male_subdf$age) - 18)/4


print("End of R script!")
