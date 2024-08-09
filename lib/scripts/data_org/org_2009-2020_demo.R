# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for averaging and organizing demographic data for the 2009-2020

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")

#### Load functions ####
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

# --------------------------------------------------------------------------
print("Establishing directory layout and other constants.")
# --------------------------------------------------------------------------
#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "demo")
dir.create(output_dir)

age <- c()
ethnicity <- c()
gender <- c()
year <- c()
id <- c()
id_name <- "Respondent sequence number"
#### Loading in data ####
import_tables <- read.csv(file = file.path("lib", "datasets", "demo_2009_2020.csv"),
                          header = T, comment.char = "#",
                          check.names =F)

my_cols <- unique(import_tables$our_name)

full_table <- data.frame()

uniq_tables <- unique(import_tables$nh_table)
#### Build tables ####
for (ut in 1:length(uniq_tables)){
  uniq_table <- uniq_tables[ut]
  subtable <- import_tables[import_tables$nh_table == uniq_table, ]
  age <- c()
  gender <- c()
  ethnicity <- c()
  study_year <- c()
  year <- c()
  nh_table_name <- c()
  for (i in 1:nrow(subtable)){
    yr <- subtable$year_start[i]
    dt_grp <- subtable$data_group[i]
    nh_tbl <- subtable$nh_table[i]
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
    target_col <- subtable$column_short_name[i]
    our_col <- subtable$our_name[i]
    data_col <- dl_tble[,target_col]
    num_col <- length(data_col)
    if (our_col == "gender") gender <- c(gender, data_col)
    if (our_col == "age_years") age <- c(age, data_col)
    if (our_col == "ethnicity") ethnicity <- c(ethnicity, data_col)
    
  }
  nh_table_name <- c(nh_table_name, rep(nh_tbl, num_col))
  year <- c(year, rep(yr, num_col))
  id <- c()
  minor_table <- data.frame(age, gender, ethnicity, year, nh_table_name)
  minor_table[,id_name] <- dl_tble[,"SEQN"]
  if (ncol(full_table) == 0) full_table <- minor_table
  else full_table <- rbind(full_table, minor_table)
}

write.csv(full_table, file = file.path(output_dir, "demo_2009-2020.csv"),
          row.names = FALSE)

print("End R script.")
