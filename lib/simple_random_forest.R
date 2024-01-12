# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for running random forest on NHANES data

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("randomForest", quietly = TRUE)) BiocManager::install("randomForest")
library("randomForest")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
if (!requireNamespace("fastDummies", quietly = TRUE)) install.packages("fastDummies")
library("fastDummies")

print("Loaded dependencies")

#### Read commandline arguements ####
option_list <- list(
  optparse::make_option(c("-d", "--homedir"), type="character", 
                        default=file.path('~','git',"lognorm_vs_CODA"), 
                        help="dataset dir path", metavar="character"),
  optparse::make_option(c("-p", "--project"), type="character", default=NULL, 
                        help="project folder", metavar="character"),
  optparse::make_option(c("-m", "--metadata"), type="character", default=NULL,
                        help="metadata file path with filename", metavar="character"),
  optparse::make_option(c("-l", "--metadata_delim"), type="character", default="\t",
                        help="metadata file deliminator", metavar="character"),
  optparse::make_option(c("-r", "--metadata_rowname"), type="character", default=NULL,
                        help="metadata file row to use for row names", metavar="character"),
  optparse::make_option(c("-n", "--num_cycles"), type="numeric", default=20,
                        help="Number of times to shuffle data and run loop again", 
                        metavar="character")
);

print("Done reading cml arguments")

#### Establish directory layout and other constants ####
# --------------------------------------------------------------------------
output_dir <- file.path("output", "tables")
print("Done reading cml arguments")
cv_folds <- 12
predictor_var <- "`Systolic...Blood.pressure..first.reading..mm.Hg`"

#### Loading in data ####
diet_2015 <- read.csv(file = file.path(output_dir, "diet_2015.csv"), header = T,
                      check.names = T)
BP_2015 <- read.csv(file = file.path(output_dir, "BP_2015.csv"), header = T,
                    check.names = T)

print("loeaded data")

#### Small amount data cleaning ####
row.names(BP_2015) <- BP_2015$Respondent.sequence.number.
BP_2015 <- BP_2015["Systolic...Blood.pressure..first.reading..mm.Hg"]
BP_2015 <- na.omit(BP_2015)
hyper1_2015 <- BP_2015 >= 130
my_intersect <- intersect(row.names(BP_2015),
                          diet_2015$Respondent.sequence.number.)
diet_2015 <- diet_2015[diet_2015$Respondent.sequence.number. %in% my_intersect,]
my_resp <- BP_2015[my_intersect,]
names(my_resp) <- my_intersect

cross_vals <- split(sample(my_intersect, size = length(my_intersect)), cut(seq(length(my_intersect)), cv_folds))

#reogranize with dummy variables to work with RF
diet_2015 <- fastDummies::dummy_cols(diet_2015, select_columns = c("USDA.food.code"),
                                ignore_na = TRUE, remove_selected_columns = TRUE)
# test1 <- aggregate(test, )

diet_2015 <- rowsum(diet_2015[,3:ncol(diet_2015)],
                    group = diet_2015$Respondent.sequence.number.,na.rm=T)

#### Loop through for random forest ####
r_sqs <- c()

my_resp <- hyper1_2015

for (cv in 1:cv_folds){
  test_fold <- as.character(cross_vals[[cv]]) #SEQN for testing
  train_fold <- as.character(my_intersect[!my_intersect %in% test_fold]) #SEQN for training
  predct_tst <- diet_2015[test_fold,]
  predct_trn <- diet_2015[train_fold,]
  resp_trn <- my_resp[train_fold, ]
  resp_tst <- my_resp[test_fold , ]
  rf <- randomForest::randomForest(predct_trn, resp_trn)
  print("made rf")
  pred <- predict(rf, predct_tst)
  my_lm <- lm(pred ~ resp_tst)
  r_sqs <- c(r_sqs, summary(my_lm)$r.squared)
  print(paste(r_sqs, collapse = ", "))
}

write.csv(rf$importance, file = file.path(output_dir, "rf_importance_hyperDiet2015.csv"))





