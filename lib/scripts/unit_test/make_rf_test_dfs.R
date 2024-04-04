# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making dataset for testing my random foret script
# Need at least one numeric column and one categorical column

rm(list = ls()) #clear workspace

pwd <- getwd()
print(paste("Working in", pwd))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("datasets", quietly = TRUE)) BiocManager::install("datasets")
library("datasets")

print("Loaded packages")

#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "unit_test")
dir.create(output_dir)

#### Loading in data ####
mtc_df <- datasets::mtcars
print("Downloaded mtcars")

mpg_df <- mtc_df[1]
mpg_df$good_milage <- mpg_df$mpg > 20
sum(mpg_df$good_milage)

mtc_df <- subset(mtc_df, select = -c(1))

cor_pvs <- apply(mtc_df, MARGIN = 2, function(x){cor(x, mpg_df[,1])^2})

write.csv(mtc_df, file.path(output_dir, "mtc_predictor.csv"), row.names = FALSE)
write.csv(mpg_df, file.path(output_dir, "mtc_response.csv"), row.names = FALSE)

print("End R script.")

