# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making graphic that compares the two of the same organization
# techniques from different epochs of diet data.

# Namespace(pred_table='Data/diet/d1_nutri_food_g_2015.csv', meta_col=True, output_label='nutr_food_grams', use_all_meta=False, out_folder='diet_test', resp_fn='Data/resp_vars/cardio_respns_vars.csv', delim='\t', meta_index_col=0, title='nutri_food_grams')

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
first_df <- read.csv("output/diet_d1d2_2009-2020/tables/my_nutri_only_d1d2_2009-2020_data.csv",
                     header = TRUE)
first_df <- first_df[,2:ncol(first_df)]
first_df$dataset <- rep("2009-2020", nrow(first_df))

first_long <- reshape2::melt(first_df, id.vars = c("dataset","response_var"),
                             variable.name = "split")

sec_df <- read.csv("output/diet_test_d1d2/tables/nutri_only_d1d2_data.csv",
                     header = TRUE)
sec_df <- sec_df[,2:ncol(sec_df)]
sec_df$dataset <- rep("2015", nrow(sec_df))

sec_long <- reshape2::melt(sec_df, id.vars = c("dataset","response_var"), variable.name = "split")

comb_long <- rbind(first_long, sec_long)

#### Order response vars such that numeric are 1st and boolean are 2nd ####
my_order <- c(
  "Total Cholesterol (mg/dL",	"Triglyceride (mg/dL",	"LDL-cholesterol (mg/dL",
  "Direct HDL-Cholesterol (mg/dL",	"Systolic_mean",	"Diastolic_mean",
  "Respondent sequence number",
  "Systolic_hypertension", "Diastolic_hypertension",	"hypertension_either",
  "unhealthy_tot_chol",	"unhealthy_trig",	"unhealthy_ldl",	"unhealthy_hdl")

# comb_long <- comb_long[comb_long$response_var != id_var,]

comb_long$response_var <- factor(comb_long$response_var, levels = my_order)

pdf(file.path("output/diet_d1d2_2009-2020/", "graphics", "all_diet_orgs.pdf"), width = 18, height = 10)

g <- ggplot2::ggplot(comb_long, aes(x=response_var, y=value, color=dataset)) + 
  geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(label = paste("Nutrients only")) +
  # geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
  ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) 
  # ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
  # ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
  # ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
  # ggplot2::facet_grid(~ f_column)
g

dev.off()

print("End of R script!")
