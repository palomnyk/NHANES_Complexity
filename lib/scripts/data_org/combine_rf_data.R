# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for combining the "_scores.csv" files from each response var into one final
# diet data.

rm(list = ls()) #clear workspace
chooseCRANmirror(graphics=FALSE, ind=66)

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

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("unit_test"), 
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
id_var <- "Respondent sequence number"
dataset_name <- FALSE
#### Loading in data ####

# get file names in output_dir
dir_files <- list.files(file.path(output_dir, "tables"), pattern = "_scores.csv")

print(paste("data files found:", paste(dir_files, collapse = ", ")))

final_table <- data.frame()

print("Data loaded!")
# Iterate through files and populate variables
for (fl in 1:length(dir_files)) {
  print(fl)
  full_name <- dir_files[fl]
  print(full_name)
  if (grepl(opt$fn_root, full_name) & startsWith(full_name, "1col") & endsWith(full_name, "_scores.csv")){
    part_name <- gsub("1col", "", full_name)
    my_splits <- unlist(strsplit(part_name, c("?"), fixed = TRUE))
    print(my_splits)
    my_column <- my_splits[1]
    my_ds <- gsub("_scores.csv", "", my_splits[2])
    print(paste(my_ds, dataset_name))
    if (dataset_name == FALSE){
      dataset_name <- my_ds
      print(paste("renamed", dataset_name))
    }
    else{
      print(dataset_name == my_ds)
      stopifnot(dataset_name == my_ds)
    }
    f_path <- file.path(output_dir, "tables", full_name)
    col_table <- read.csv(f_path, header = T, check.names = F)
    if (nrow(final_table) == 0){
      final_table <- col_table
      print("added first row to final table")
    }else{
      final_table <- rbind(final_table, col_table)
    }
  }# End first if
}# End main for loop

print("Finished loop!")
f_path <- file.path(output_dir, "tables", paste0(dataset_name,"_scores.csv"))
write.csv(final_table, file = f_path, row.names = FALSE)

print("Reached end of R script!")
