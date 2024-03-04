# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for averaging and organizing BP dataset

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")

if (!requireNamespace("optparse", quietly = TRUE)) install.packages("optparse")
library("optparse")
print("Reading cml arguments")

option_list <- list(
  optparse::make_option(c("-d", "--homedir"), type="character", 
                        default=file.path('~','git',"NHANES_Complexity"), 
                        help="dataset git repo path"),
  optparse::make_option(c("-a", "--data"), type="character", default="BP_2015.csv",
                        help="filename in tables folder"),
  optparse::make_option(c("-l", "--delim"), type="character", default="\t",
                        help="metadata file deliminator", metavar="character"),
  optparse::make_option(c("-c", "--colnames"), type="character", default=1,
                        help="metadata file row to use for row names"),
  # callback = strsplit(), callback_args = "split = ','"),
  optparse::make_option(c("-o", "--output_fname"), type="character", default="descriptives",
                        help="output file name")
); 

opt_parser <- optparse::OptionParser(option_list=option_list);

opt <- optparse::parse_args(opt_parser);

print(opt)

# --------------------------------------------------------------------------
print("Establishing directory layout and other constants.")
# --------------------------------------------------------------------------
home_dir <- opt$homedir
project <- opt$project
output_dir <- file.path(home_dir, 'output')
#### Establish directory layout and other constants ####
output_dir <- file.path("output")


tables to download and organize for 2015 NHANES:
  HDL_I
  TRIGLY_I
  TCHOL_I

#### Loading in data ####
my_table <- read.csv(file = file.path(output_dir,"tables", opt$data),
                     header = T,
                     check.names =F)

prefix_to_average <- c("Systolic:  Blood pressure", "Diastolic: Blood pressure")
threshold <- c(130, 80)
new_df <- data.frame(matrix(nrow = nrow(my_table), ncol = 0))

for (i in 1:length(prefix_to_average)){
  pre <- prefix_to_average[i]
  print(pre)
  my_cols <- unlist(lapply(names(my_table), function(x){startsWith(x, pre)}))
  print(my_cols)
  my_cols <- names(my_table)[my_cols]
  my_mean <- unlist(rowMeans(my_table[ , my_cols], na.rm=T))
  new_df[,paste0(pre, "_mean")] <- my_mean
  new_df[,paste0(pre, "_hypertension")] <- my_mean >= threshold[i]
}

any_hyper <- apply(array, margin, ...)


# hyper_df <- my_table[my_table$Systolic_Hypertension == TRUE,]
# normo_df <- my_table[my_table$Systolic_Hypertension == FALSE,]
# 
# write.csv(hyper_df,
#           file = file.path(output_dir,"tables", "PHTHTE_PAQ_noRx_BP_2015_match_diet_SYST1_RF_hyper.csv"),
#           row.names = FALSE)
# 
# write.csv(normo_df,
#           file = file.path(output_dir,"tables", "PHTHTE_PAQ_noRx_BP_2015_match_diet_SYST1_RF_normo.csv"),
#           row.names = FALSE)

