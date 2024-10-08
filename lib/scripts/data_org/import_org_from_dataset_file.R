# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for averaging and organizing data from a data import table

rm(list = ls()) #clear workspace
chooseCRANmirror(graphics=FALSE, ind=66)

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

#### Load functions ####
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Parse commandline arguments ####
option_list <- list(
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("exam"), 
                        help="dataset subdir path with in Data dir"),
  optparse::make_option(c("-f", "--infile"), type="character", 
                        default="lib/datasets/helper_features_2009-2020.csv", 
                        help="full relative path, usually in /lib/datasets/, of data import file"),
  optparse::make_option(c("-n", "--outfile"), type="character", 
                        default="Data/demo/helper_features_2009-2020.csv", 
                        help="File (full relative path) in outdir.")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

# --------------------------------------------------------------------------
print("Establishing directory layout and other constants.")
# --------------------------------------------------------------------------
#### Establish directory layout and other constants ####
output_dir <- file.path("Data", opt$out_subdir)
dir.create(output_dir)

id_name <- "Respondent sequence number"
#### Loading in data ####
import_tables <- read.csv(file = file.path(opt$infile),
                          header = T, comment.char = "#",
                          check.names =F)

import_cols <- c(unique(import_tables$column_short_name),"SEQN")
target_names <- c(unique(import_tables$our_name),id_name)

full_table <- data.frame(matrix(nrow = 0, ncol = length(import_cols)))
names(full_table) <- target_names

uniq_tables <- unique(import_tables$nh_table)
#### Build tables ####
for (ut in 1:length(uniq_tables)){
  uniq_table <- uniq_tables[ut]
  subtable <- import_tables[import_tables$nh_table == uniq_table, ]
  target_cols <- c(unique(subtable$column_short_name), "SEQN")
  yr <- subtable$year_start[1]
  dt_grp <- subtable$data_group[1]
  nh_tbl <- subtable$nh_table[1]
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
  dl_tble <- dl_tble[,target_cols]
  names(dl_tble) <- target_names[match(names(dl_tble), import_cols)]
  row.names(dl_tble) <- dl_tble[,id_name]
  # dl_tble <- dl_tble[,!names(dl_tble) %in% c(id_name)]
  # dl_tble$nh_table <- rep(nh_tbl, nrow(dl_tble))
  # dl_tble$year <- rep(yr, nrow(dl_tble))
  for (cn in names(dl_tble)) {
    full_table[row.names(dl_tble),cn] <- dl_tble[,cn]
  }
}

# write.csv(full_table[,target_names], file = file.path(paste("raw_", opt$outfile)),
#           row.names = FALSE)

full_table <- data.frame(convert_dummy(full_table), check.names = FALSE)

write.csv(full_table, file = file.path(opt$outfile),
          row.names = FALSE)

print("End R script.")
