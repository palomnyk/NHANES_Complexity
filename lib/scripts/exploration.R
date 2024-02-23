# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for exploring NHANES
# TODO: Separate food intake from other metadata and redo PCAS
# TODO: Look into merging FPED into this data
# https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-overview/
# https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/

rm(list = ls()) #clear workspace

# --------------------------------------------------------------------------
print("Loading dependencies")
# --------------------------------------------------------------------------
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("tidyverse", quietly = TRUE)) BiocManager::install("tidyverse")
library("tidyverse")
if (!requireNamespace("haven", quietly = TRUE)) BiocManager::install("haven")
library("haven")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
library(RColorBrewer)
#set color palette
palette( brewer.pal(7,"Accent") )

# --------------------------------------------------------------------------
print("Defining functions")
# --------------------------------------------------------------------------
#convert time to minutes (https://stackoverflow.com/questions/40476796/r-function-to-convert-time-of-day-to-total-minutes)
time_dif <- function(x) difftime(as.POSIXct(x, format = '%H:%M'), as.POSIXct('00:00', format = '%H:%M'), units = 'min')

# --------------------------------------------------------------------------
print("Establishing directory layout and other constants.")
# --------------------------------------------------------------------------
setwd("C:/Users/aaron.yerke/git/NHANES_Complexity")#delete before running as Rscript
output_dir <- file.path("output", "graphics")

# --------------------------------------------------------------------------
print("Loading in data.")
# --------------------------------------------------------------------------
code_book <- haven::read_xpt(file.path("data", "P_DRxfcd.XPT"))
#https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&Cycle=2017-2020
nutrition_day1 <- haven::read_xpt(file.path("data", "P_DR1IFF.XPT"))
nutrition_day1$DR1_020 <- sapply(nutrition_day1$DR1_020, time_dif)#convert from char to double to make stats easier
blood_pressure <- haven::read_xpt(file.path("data", "P_BPXO.XPT"))
  
# --------------------------------------------------------------------------
print("Creating PCA plots.")
# --------------------------------------------------------------------------

pdf(file=file.path(output_dir, "leave_one_out_PCA_plots.pdf"))
for(feat_num in 1:ncol(nutrition_day1)){
  missing_feat <- nutrition_day1[,feat_num]
  missing_name <- colnames(nutrition_day1)[feat_num]
  missing_label <- attr(missing_feat, "label")
  short_table <- nutrition_day1[,-feat_num]
  print(missing_name)
  
  #create PCA
  my_prcmp = prcomp(na.omit(short_table), 
                    center = TRUE,
                    scale = TRUE)
  
  #extract PCA matrix and convert to dataframe
  myPCA = data.frame(my_prcmp$x)
  #% each component explains
  var_exp <- my_prcmp$sdev^2/sum(my_prcmp$sdev^2)
  
  # par(bty = 'l',
  #     mar = c(5, 4, 5, 2) + 0.1,
  #     cex.lab = 1.25
  # )
  plot(myPCA$PC1, myPCA$PC2,
       col=as.factor(unlist(missing_feat)),
       #pch = c(24,21,22)[as.numeric(as.factor(metadata['structural location',]))],
       #bg = as.factor(metadata['root vs stem',]),
       pch = 21,
       cex = 0.5,
       xlab = paste("PCA1",round(var_exp[1], 2)*100, "%"),
       ylab = paste("PCA2",round(var_exp[2], 2)*100, "%"),
       main = paste("PCA missing and colored by:", missing_name),
       sub = missing_label
  )
  # legend('topright', 
  #        legend = c('crown root', 'lateral root', 'stem'),
  #        pt.bg = as.factor(c('root', 'root', 'stem')),
  #        pch = c(24,21,22),
  #        cex = 1
  # )
}#end for-loop

dev.off()


fake_data <- data.frame(my_binom <- rbinom(10000, 100, runif(100)))

for (i in 1:10){
  my_binom <- rbinom(10000, i, runif(100))
  fake_data <- cbind(fake_data, my_binom)
}
for (i in 1:10){
  my_binom <- runif(1000, 0, i)
  fake_data <- cbind(fake_data, my_binom)
}

#create PCA
my_prcmp = prcomp(na.omit(fake_data), 
                  center = TRUE,
                  scale = TRUE)

#extract PCA matrix and convert to dataframe
myPCA = data.frame(my_prcmp$x)
#% each component explains
var_exp <- my_prcmp$sdev^2/sum(my_prcmp$sdev^2)

# par(bty = 'l',
#     mar = c(5, 4, 5, 2) + 0.1,
#     cex.lab = 1.25
# )


pdf(file=file.path(output_dir, "testing_binom_hyp.pdf"))
plot(myPCA$PC1, myPCA$PC2,
     #pch = c(24,21,22)[as.numeric(as.factor(metadata['structural location',]))],
     #bg = as.factor(metadata['root vs stem',]),
     pch = 21,
     cex = 0.5,
     xlab = paste("PCA1",round(var_exp[1], 2)*100, "%"),
     ylab = paste("PCA2",round(var_exp[2], 2)*100, "%"),
)
dev.off()

hist(na.omit(blood_pressure$BPXOSY1),
     main = "Systolic blood pressure",
     breaks = 300,
     xlab = "2017-March 2020 Pre-Pandemic Dietary Data - Continuous NHANES"
     )

install.packages("nhanesA")
library(nhanesA)
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html

nhanesTables(data_group='DEMO', year=2015)

demo_2015 <- nhanes('DEMO_I')

nhanesTables(data_group='DIET', year=2015)

diet_2015 <- nhanes('DIET_I')


