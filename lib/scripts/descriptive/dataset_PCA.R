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
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded packages")

#### Read commandline arguements ####
option_list <- list(
  optparse::make_option(c("-r", "--filename_root"), type="character", 
                        default="d1d2_food_g_2009-2020", metavar="character",
                        help="filename before '_TRANSFORMATION' or '.csv'"),
  optparse::make_option(c("-i", "--input_dir"), type="character",
                        default=file.path("Data", "diet", "multi_year"),
                        help="relative path to folder holding csv tables",
                        metavar="character")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print(opt)
print("Done reading cml arguments")

#### Establish directory layout and other constants ####
output_dir <- file.path("output", "dataset_PCA")
dir.create(file.path(output_dir))
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))

input_dir <- file.path("Data", "diet", "multi_year")
suffixs <- c("", "_LOG", "_SCALE")# "_MM",
id_var <- "Respondent sequence number"

#### Loading in data ####
meta_fp <- file.path("Data","respns_vars","2009-2020cardio_respns_vars.csv")
meta_df <- read.csv(meta_fp, header = TRUE,
                    row.names = id_var,
                    check.names = FALSE)

#### PCA ####
pdf(file.path(output_dir, "graphics", paste0(opt$filename_root, "PCA.pdf")))
for (suf in 1:length(suffixs)){
  suffix <- suffixs[suf]
  f_path <- file.path(input_dir, paste0(opt$filename_root, suffix, ".csv"))
  print(f_path)
  my_table <- read.csv(f_path, header = TRUE,
                       row.names = id_var,
                       check.names = FALSE)
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
      ggtitle(paste0("PCA_", opt$filename_root, suffix )) + # Blank Title for the Graph
      xlab(paste0("PC 1, ", round(my_var_exp[1],2)*100, "%")) +
      ylab(paste0("PC 2, ", round(my_var_exp[2],2)*100, "%")) +
      labs(color = md)
    print(g)
  }
}

dev.off()

print("End R script.")

