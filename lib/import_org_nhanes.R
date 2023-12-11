# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for importing and organizing data

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")

print("Loaded dependencies")

#### Defining functions ####
#convert time to minutes (https://stackoverflow.com/questions/40476796/r-function-to-convert-time-of-day-to-total-minutes)
time_dif <- function(x) difftime(as.POSIXct(x, format = "%H:%M"), as.POSIXct("00:00", format = "%H:%M"), units = "min")

nhanes_names <- function(dl_df, dt_group, nh_tble) {
  #puts human readable names on the NHANES tables
  diet_names <- nhanesA::nhanesTableVars(data_group = dt_group,
                                         nh_table = nh_tble,
                                         namesonly = FALSE)

  my_ord <- match(names(dl_df), diet_names$Variable.Name)
  names(dl_df) <- diet_names$Variable.Description[my_ord]
  return(dl_df)
}
#### Establish directory layout and other constants ####
# --------------------------------------------------------------------------
output_dir <- file.path("output", "tables")

#### Loading in data ####
food_codes <- readxl::read_excel(file.path("Data", "WWEIA1516_foodcat_FNDDS.xlsx"), 
                                 trim_ws = T, na = c("", "NA"))



# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
nhanesA::nhanesTables(data_group="DEMO", year=2015)
demo_2015 <- nhanesA::nhanes("DEMO_I")

nhanesA::nhanesTables(data_group="DIET", year=2015)
diet_2015 <- nhanesA::nhanes("DR1IFF_I")

diet_names <- nhanesA::nhanesTableVars(data_group = "DIET",
                                       nh_table = "DR1IFF_I",
                                       namesonly = FALSE)

diet_2015 <- nhanes_names(diet_2015, "DIET", "DR1IFF_I")

diet_2015$`USDA food code` <- food_codes$food_code_description[match(diet_2015$`USDA food code`, food_codes$food_code)]
# any(diet_2015$`USDA food code` == 11112210)

dir.create(file.path(output_dir), showWarnings = FALSE)
write.csv(diet_2015, file = file.path(output_dir, "diet_2015.csv"))

nhanesA::nhanesTables(data_group="EXAM", year=2015)
BP_2015 <- nhanesA::nhanes("BPX_I")

BP_2015 <- nhanes_names(BP_2015, "EXAM", "BPX_I")

write.csv(BP_2015, file = file.path(output_dir, "BP_2015.csv"))


