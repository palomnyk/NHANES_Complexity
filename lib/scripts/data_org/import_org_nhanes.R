# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing data
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
# Usage:
# Rscript lib/scripts/data_org/import_org_nhanes.R\
#   --output_prefix diet_\
#   --input_table Data/diet_nhanesTablesToAdd.csv

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")
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
                        default=file.path("lib", "datasets", "nhanesTablesToAdd.csv"),
                        help="relative path, needs to be comma seperated with header",
                        metavar="character")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print(opt)
print("Done reading cml arguments")
#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "nhanesA_tables")
dir.create(output_dir)
id_var <- "Respondent sequence number"
cor_thresh <- 0.9

#### Loading in data ####
import_tables <- read.csv(file.path(opt$input_table),
                          header = T, sep = ",", comment.char = "#")
print(table(import_tables$data_group))
short_col_name <- c()
long_col_name <- c()
dt_grp_name <- c()
nh_table_name <- c()
year <- c()
full_df <- data.frame()
not_added <- c()

for (i in 1:nrow(import_tables)){
  yr <- import_tables$year[i]
  dt_grp <- import_tables$data_group[i]
  nh_tbl <- import_tables$nh_table[i]
  fname <- paste0(nh_tbl,"_", yr, ".csv")
  print(fname)
  fname_path <- file.path(output_dir, fname)
  if (!file.exists(fname_path)){
    print(paste(fname_path, "not found - Downloading!"))
    dl_tble <- nhanesA::nhanes(nh_tbl)
    write.csv(dl_tble, fname_path, row.names = FALSE)
    Sys.sleep(2)
  }else{
    dl_tble <- read.csv(fname_path, header = TRUE, check.names = FALSE)
  }
  dl_tble <- dl_tble[,sapply(dl_tble, function(x) !all(is.na(x)))]
  short_col_name <- c(short_col_name, names(dl_tble))
  num_col <- ncol(dl_tble)
  dt_grp_name <- c(dt_grp_name, rep(dt_grp, num_col))
  nh_table_name <- c(nh_table_name, rep(nh_tbl, num_col))
  year <- c(year, rep(yr, num_col))
  
  #TODO: Drop columns listed in nhanesTablesToAdd.csv (drop_col)
  dl_tble <- nhanesA::nhanesTranslate(nh_table = nh_tbl,
                                      data = dl_tble,
                                      details = TRUE,
                                      colnames = names(dl_tble))
  
  dl_tble <- nhanes_names(dl_tble, dt_grp, nh_tbl)
  
  attr(dl_tble, "names") <- sub("[[:punct:]]$", "", names(dl_tble)) 
  long_col_name <- c(long_col_name, names(dl_tble))
  fname <- paste0(nh_tbl,"_", yr, "_lg.csv")
  fname_path <- file.path(output_dir, fname)
  if (!file.exists(fname_path)){#save with long name
    write.csv(dl_tble, fname_path, row.names = FALSE)
  }
  if(length(unique(dl_tble$`Respondent sequence number`)) == nrow(dl_tble)){
    if (nrow(full_df) < 1){
      full_df <- dl_tble
    }else{
      print(paste(nrow(full_df), nrow(dl_tble)))
      full_df <- merge(full_df, dl_tble, by = id_var,
                       all = TRUE)
    }
  }else{
    print(paste("Didn't add", nh_tbl))
    not_added <- c(not_added, nh_tbl)
  }
}

codebook <- data.frame(short_col_name,
                       long_col_name,
                       dt_grp_name,
                       nh_table_name,
                       year)

write.csv(codebook, file = file.path(output_dir, paste0(opt$output_prefix, "comb_NHANES_summary.csv")),
          row.names = FALSE)

no_var <- vapply(full_df, function(x) length(unique(x)) == 1, logical(1L))
print(paste("Num dropped features with no variance:", sum(no_var)))
full_df <- full_df[vapply(full_df, function(x) length(unique(x)) > 1, logical(1L))]#remove columns with no variance
row.names(full_df) <- full_df[,id_var]

print(paste("B4 DUM NCOL, NROW, %NA", ncol(full_df), nrow(full_df), mean(is.na(full_df))))

full_df <- convert_dummy(full_df)
print(paste("AFT DUM NCOL, NROW, %NA", ncol(full_df), nrow(full_df), mean(is.na(full_df))))

print(paste("Any NA in respondent ID:", any(is.na(full_df[,id_var]))))
stopifnot(any(!is.na(full_df[,id_var])))
print(paste("Respondent IDs are unique:",
            length(unique(full_df[,id_var])) == nrow(full_df)))
stopifnot(length(unique(full_df[,id_var])) == nrow(full_df))

#### Create correlation matrix, remove cor features #####
cor_matrx <- cor(full_df)

upper_tri <- cor_matrx
upper_tri[lower.tri(upper_tri)] <- NA

melted_cormat <- melt(upper_tri, na.rm = TRUE)

over_cor <- subset(melted_cormat, melted_cormat$Var1 != melted_cormat$Var2)
over_cor <- subset(over_cor, value > cor_thresh)

write.csv(over_cor, 
          file = file.path(output_dir, paste0(opt$output_prefix,"over_cor_",cor_thresh,"_combined_NHANES_dum.csv")),
          row.names = FALSE)

full_df <- full_df[, !colnames(full_df) %in% over_cor$Var2] #drop highly correlated variables

write.csv(full_df, file = file.path(output_dir, paste0(opt$output_prefix, "combined_NHANES_dum.csv")),
          row.names = FALSE)

print("End R script.")
