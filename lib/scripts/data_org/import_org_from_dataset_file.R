# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for averaging and organizing demographic data for the 2009-2020

rm(list = ls()) #clear workspace
chooseCRANmirror(graphics=FALSE, ind=66)

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")

#### Load functions ####
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("exam"), 
                        help="dataset subdir path with in Data dir"),
  optparse::make_option(c("-f", "--infile"), type="character", 
                        default=file.path("body_weight_2009-2020.csv"), 
                        help="File name (full relative path) in /lib/datasets/."),
  optparse::make_option(c("-n", "--outfile"), type="character", 
                        default=file.path("body_weight_2009-2020.csv"), 
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
my_cols <- c(target_names, "year", "nh_table_name", id_name)

full_table <- data.frame()

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
  dl_tble <- dl_tble[,import_cols]
  names(dl_tble) <- target_names[match(names(dl_tble), import_cols)]
  
  dl_tble$nh_table <- rep(nh_tbl, nrow(dl_tble))
  dl_tble$year <- rep(yr, nrow(dl_tble))
  
  if (ncol(full_table) == 0) full_table <- dl_tble
  else full_table <- rbind(full_table, dl_tble)
}

write.csv(full_table[,target_names], file = file.path(opt$outfile),
          row.names = FALSE)

print("End R script.")
