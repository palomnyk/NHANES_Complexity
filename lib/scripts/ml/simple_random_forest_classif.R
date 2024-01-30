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
diet_2015 <- read.csv(file = file.path(output_dir, "demo_diet_2015_match_bp_SYST1_RF.csv"), header = T,
                      check.names = T)
BP_2015 <- read.csv(file = file.path(output_dir, "BP_2015_match_diet_SYST1_RF.csv"), header = T,
                    check.names = T)

print("loeaded data")

#### Small amount data cleaning ####
#create response var
row.names(BP_2015) <- BP_2015$Respondent.sequence.number.
BP_2015 <- BP_2015["Systolic...Blood.pressure..first.reading..mm.Hg"]
BP_2015 <- na.omit(BP_2015)
hyper1_2015 <- BP_2015 >= 130
my_intersect <- intersect(row.names(BP_2015),
                          row.names(diet_2015))
diet_2015 <- diet_2015[row.names(diet_2015) %in% my_intersect,]
names(hyper1_2015) <- my_intersect

cross_vals <- split(sample(my_intersect, size = length(my_intersect)), cut(seq(length(my_intersect)), cv_folds))


rf_classif <- function(cross_vals, my_resp, my_predict, test_fold, intrsct, 
                       cv_folds = 10,
                       importance_fn) {
  accuracies <- c()
  print("In rf_classif")
  for (cv in 1:cv_folds){
    print(cv_folds)
    test_fold <- as.character(cross_vals[[cv]]) #SEQN for testing
    train_fold <- as.character(intrsct[!intrsct %in% test_fold]) #SEQN for training
    predct_tst <- my_predict[test_fold,]
    predct_trn <- my_predict[train_fold,]
    resp_trn <- as.factor(my_resp[train_fold, ])
    resp_tst <-as.factor( my_resp[test_fold, ])
    rf <- randomForest::randomForest(predct_trn, resp_trn)
    print("made rf")
    pred <- predict(rf, predct_tst)
    my_acc <- sum(pred == resp_tst)/length(resp_tst)
    accuracies <- c(accuracies, my_acc)
    print(paste(accuracies, collapse = ", "))
  }
  write.csv(rf$importance, file = importance_fn)
  return(accuracies)
}

regress1 <- rf_classif(my_resp = hyper1_2015,
                       cross_vals = cross_vals,
                       my_predict = diet_2015,
                       test_fold = test_fold,
                       intrsct = my_intersect,
                       cv_folds = cv_folds,
                       importance_fn = file.path(output_dir, "rf_importance_hyperDiet2015.csv")
)




