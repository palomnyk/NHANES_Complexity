# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing data
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")

print("Loaded dependencies")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "nhanesA_tables")
dir.create(output_dir)
id_var <- "Respondent sequence number"

#### Loading in data ####
import_tables <- read.csv(file.path("Data", "nhanesTablesToAdd.csv"),
                           header = T, sep = ",", comment.char = "#")
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

full_df <- full_df[vapply(full_df, function(x) length(unique(x)) > 1, logical(1L))]
row.names(full_df) <- full_df[,id_var]
full_df <- convert_dummy(full_df)

print(paste("Any NA in respondent ID:", any(is.na(full_df[,id_var]))))
stopifnot(any(!is.na(full_df[,id_var])))
print(paste("Respondent IDs are unique:",
            length(unique(full_df[,id_var])) == nrow(full_df)))
stopifnot(length(unique(full_df[,id_var])) == nrow(full_df))

write.csv(full_df, file = file.path(output_dir, "combined_NHANES_dum.csv"),
          row.names = FALSE)

print("End R script.")
