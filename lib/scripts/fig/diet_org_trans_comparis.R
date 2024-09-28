# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making graphic that compares the various methods of organizing
# diet data.

# Rscript lib/scripts/fig/diet_org_trans_comparis.R -r all_combined_d1d2 -o final_all_trans 

### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("readxl", quietly = TRUE)) BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("ggplot2", quietly = TRUE)) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")
# source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Functions ####
org_meth_plot <- function (df_table, title_text){
  means <- aggregate(df_table$score, by=list(df_table$org_method), mean)
  names(means) <- c("org_method", "m")
  stdv <- aggregate(df_table$score, by=list(df_table$org_method), sd)
  names(stdv) <- c("org_method", "s")
  means <- merge(means, stdv)
  means$x <- round(means$m, 3)
  g <- ggplot2::ggplot(df_table, aes(x=response_var, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
    ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
    ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
    ggplot2::facet_grid(~ org_method)
  return(g)
}
resp_vars_plot <- function (df_table, title_text){
  means <- aggregate(df_table$score, by=list(df_table$response_var), mean)
  names(means) <- c("response_var", "m")
  stdv <- aggregate(df_table$score, by=list(df_table$response_var), sd)
  names(stdv) <- c("response_var", "s")
  means <- merge(means, stdv)
  means$x <- round(means$m, 3)
  g <- ggplot2::ggplot(df_table, aes(x=org_method, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
    ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
    ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
    ggplot2::facet_grid(~ response_var)
  return(g)
}

plot_data <- function (df_table, f_column = "org_method", title_text){
  means <- aggregate(df_table[,"score"], by=list(df_table[,f_column]), mean)
  names(means) <- c(f_column, "m")
  stdv <- aggregate(df_table$score, by=list(df_table[,f_column]), sd)
  names(stdv) <- c(f_column, "s")
  means <- merge(means, stdv)
  means$x <- round(means$m, 3)
  g <- ggplot2::ggplot(df_table, aes(x=response_var, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
    ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
    ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
    ggplot2::facet_grid(~ f_column)
  return(g)
}

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("proper_days"), 
                        help="dataset dir path"),
  optparse::make_option(c("-r", "--fn_root"), type="character", 
                        default=file.path("cat_grams_d1d2"), 
                        help="Root of filename to compare.")
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
suffixs <- c("", "_LOG", "_MM", "_SCALE")
id_var <- "Respondent sequence number"
#### Loading in data ####

# get file names in output_dir
dir_files <- list.files(file.path(output_dir, "tables"), pattern = "_scores.csv")

print(paste("data files found:", paste(dir_files, collapse = ", ")))

response_var <- vector(mode = "character")
org_method <- vector(mode = "character")
score <- vector(mode = "numeric")

score_files <- c()

print("Data loaded!")
# Iterate through files and populate variables
for (suf in 1:length(suffixs)){
  suffix <- suffixs[suf]
  f_path <- file.path("output", opt$out_subdir, "tables", paste0(opt$fn_root, suffix, "_scores.csv"))
  if (file.exists(f_path)){
    my_table <- read.csv(f_path,
                         header = T, check.names = F)
    print(my_table)
    for (r in 2:nrow(my_table)){
      if (my_table$response_var[r] != id_var){
        response_var <- c(response_var, rep(my_table$response_var[r], 10))
        org_method <- c(org_method, rep(suffix, 10))
        score <- c(score, unlist(my_table[r, 3:ncol(my_table)]))
      }
    }
    
  }else{
    print(paste(f_path, "does not exist"))
  }
}

print("Script complete!")


big_table <- data.frame(org_method, response_var, score)

print(unique(big_table$response_var))
old <- unique(big_table$response_var)

#### Order response vars such that numeric are 1st and boolean are 2nd ####
my_order <- c(
  "Total Cholesterol (mg/dL",	"Triglyceride (mg/dL",	"LDL-cholesterol (mg/dL",
  "Direct HDL-Cholesterol (mg/dL",	"Systolic_mean",	"Diastolic_mean",
  "Systolic_hypertension", "Diastolic_hypertension",	"hypertension_either",
  "unhealthy_tot_chol",	"unhealthy_trig",	"unhealthy_ldl",	"unhealthy_hdl")

big_table$response_var <- factor(big_table$response_var, levels = my_order)

pdf(file.path(output_dir, "graphics", "all_diet_orgs.pdf"), width = 18, height = 10)

org_meth_plot(big_table, "Nutrition data org strategies full result")
resp_vars_plot(big_table, "Nutrition data org strategies full result")

num_table <- big_table[!(big_table$response_var %in% numeric_only),]
org_meth_plot(num_table, "Nutrition data organization strategies CAT ONLY")
resp_vars_plot(num_table, "Nutrition data organization strategies CAT ONLY")

num_table <- big_table[big_table$response_var %in% numeric_only,]
org_meth_plot(num_table, "Nutrition data organization strategies NUM ONLY")
resp_vars_plot(num_table, "Nutrition data organization strategies NUM ONLY")

dev.off()
