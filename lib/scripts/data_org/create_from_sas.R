# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing dietary data from multiple years

rm(list = ls()) #clear workspace

pwd <- getwd()
print(paste("Working in", pwd))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("haven", quietly = TRUE)) BiocManager::install("haven")
library("haven")

#### Establish directory layout and other constants ####
in_dir <- file.path("lib", "datasets")
output_dir <- file.path("Data", "diet", "multi_year")
dir.create(output_dir)

sas_files <- c("DRXFCD_I", "DRXFCD_J")
sas_years <- c(2015, 2017)

for (sas in 1:length(sas_files)) {
  sas_f <- sas_files[sas]
  data <- read_xpt(file.path("Data", paste0(sas_files[sas],".XPT")))
  names(data) <- c("Food Code","Short Food Code Description", "Long Food Code Description")
  
  write.csv(data, file = file.path(output_dir, paste0(sas_f,"_",sas_years[sas],".csv")),
            row.names = FALSE)
}



print("End R script.")
