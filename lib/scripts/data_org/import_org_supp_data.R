# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing supplementary data
# Plan for the supplemental data:
#     - create vars for the supplement names (DSDSUPP)
#         - show presence or absence 
#     - variables for the total quantities of the nutrients
# This can merge with the dietary by summing any column with the same name
# (ie. the nutrients ) and joining the other columns. Also, this data can all
# come from DS1IDS_I and DS2IDS_I, which makes it easier for me to work with.

rm(list = ls()) #clear workspace

pwd <- getwd()
print(paste("Working in", pwd))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")
if (!requireNamespace("readxl", quietly = TRUE)) BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("fastDummies", quietly = TRUE)) BiocManager::install("fastDummies")
library("fastDummies")

print("Loaded packages")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Establish directory layout and other constants ####
output_dir <- file.path("Data", "diet")
dir.create(output_dir)

#### Loading in data ####
id_var <- "Respondent sequence number"
#Load demographic data to remove children
demo_2015 <- download_org_nhanes(dt_group = "DEMO",
                                 nh_tble = "DEMO_I")
adult_seqn <- demo_2015[,id_var][
  which(demo_2015$`Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age`>18)]
print(paste("Number of participants:", nrow(demo_2015)))
print(paste("Number of adults:", length(adult_seqn)))
### Organize diet data, only individual foods first ####
# d1_sup_simp_2015 <- download_org_nhanes("DIET", "DS1IDS_I")
# d2_sup_simp_2015 <- download_org_nhanes("DIET", "DS2IDS_I")
# 
# d1_sup_simp_2015 <- rbind(d1_sup_simp_2015, d2_sup_simp_2015)
# 
# #keep only adults
# d1_sup_simp_2015 <- d1_sup_simp_2015[d1_sup_simp_2015[,id_var] %in% adult_seqn, ]
# d1_sup_simp_2015 <- type.convert(d1_sup_simp_2015, as.is = TRUE)#Reset column types automatically (for factors)

tot_sup_2015 <- download_org_nhanes("DIET", "DSQIDS_I")

# tot_sup_2015 <- nhanesA::nhanes("DSQIDS_I")
# tot_sup_2015 <- nhanes_names(tot_sup_2015, "DIET", "DSQIDS_I")

# pdf(file.path("output", "sup_explore.pdf"))
hist(table(tot_sup_2015$`For how long have you been taking {PRODUCT NAME} or a similar type of product`), breaks=200)
hist(table(tot_sup_2015$`On the days that (you/SP) took (PRODUCT NAME), how much did (you/SP), usually take on a single day`), breaks=200)
hist(table(tot_sup_2015$`Reported serving size/label serving size`), breaks=200)

# num_na <- colSums(is.na(tot_sup_2015))
# names(num_na) <- names(tot_sup_2015)
# 
# num_na <- t(data.frame(num_na))
# names(num_na) <- names(tot_sup_2015)

# The supplement name is missing
names(tot_sup_2015)[3] <- "supplement"




# dev.off()


