# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making PCA to compare dataset transformations

rm(list = ls()) #clear workspace

print(paste("Working in", getwd()))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("openxlsx", quietly = TRUE))  BiocManager::install("openxlsx")
library("openxlsx")
if (!require("ggplot2")) BiocManager::install("ggplot2")
library("ggplot2")

print("Loaded packages")

#### Establish directory layout and other constants ####
output_dir <- file.path("output", "dataset_PCA")
dir.create(file.path(output_dir))
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))

input_dir <- file.path("Data", "diet")
fn_root <- "d1d2_cat_g_2015"
suffixs <- c("", "_LOG", "_MM", "_SCALE")
id_var <- "Respondent sequence number"

#### Loading in data ####
meta_fp <- file.path("Data","respns_vars","cardio_respns_vars.csv")
meta_df <- read.csv(meta_fp, header = TRUE,
                    row.names = id_var,
                    check.names = FALSE)

#### PCA ####
pdf(file.path(output_dir, "graphics", paste0(fn_root, "PCA.pdf")))
for (suf in 1:length(suffixs)){
  suffix <- suffixs[suf]
  f_path <- file.path(input_dir, paste0(fn_root, suffix, ".csv"))
  print(f_path)
  my_table <- read.csv(f_path, header = TRUE,
                       row.names = id_var,
                       check.names = FALSE)
  sadfsdaf
  #create PCA
  my_prcmp <- prcomp(na.omit(my_table), 
                     center = TRUE,
                     scale = TRUE)
  #extract PCA matrix and convert to dataframe
  myPCA <- data.frame(my_prcmp$x)
  print("Creating proportion of variance explained")
  my_var_exp <- my_prcmp$sdev^2/sum(my_prcmp$sdev^2)
  
  #### Make plots ####
  # safe_ids <- row.names(meta_df)[which( row.names(meta_df) %in% row.names(my_table))]
  my_meta_df <- meta_df[which( row.names(meta_df) %in% row.names(my_table)),]
  for(mdc in 1:ncol(meta_df)){
    md <- colnames(meta_df)[mdc]
    print(md)
    print(paste(length(myPCA$PC1), length(factor(my_meta_df[,md]))))
    g <- ggplot2::ggplot(myPCA, aes(x=PC1, y=PC2, col = my_meta_df[,md])) +
      ggplot2::geom_point(size=0.5) +
      ggtitle(paste0("PCA_", fn_root, suffix )) + # Blank Title for the Graph
      xlab(paste0("PC 1, ", round(my_var_exp[1],2)*100, "%")) +
      ylab(paste0("PC 2, ", round(my_var_exp[2],2)*100, "%")) +
      labs(color = md)
    print(g)
  }

}

dev.off()

# , col = factor(meta_df[,mdc])

# print("Checking for columns/chems with zero variance")
# 
# zero_var <- which(apply(mtbmcs_df, 2, var) == 0)
# 
# print(paste("Num with zero var:", length(zero_var)))
# print("removing the following:")
# print(paste(names(zero_var), collapse = ", "))
# mtbmcs_df <- mtbmcs_df[, which(apply(mtbmcs_df, 2, var) != 0)]

print("End R script.")

