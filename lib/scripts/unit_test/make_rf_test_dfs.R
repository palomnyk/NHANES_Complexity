# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making dataset for testing my random foret script
# Need at least one numeric column and one categorical column

rm(list = ls()) #clear workspace
chooseCRANmirror(graphics=FALSE, ind=66)

pwd <- getwd()
print(paste("Working in", pwd))

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("datasets", quietly = TRUE)) BiocManager::install("datasets")
library("datasets")
if (!requireNamespace("randomForest", quietly = TRUE)) BiocManager::install("randomForest")
library("randomForest")

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

mtc_df$`Respondent sequence number` <- row.names(mtc_df)
mpg_df$`Respondent sequence number` <- row.names(mpg_df)

# To run test:
# python lib/scripts/ml/rf-resp_df.py \
# 	--response_fn Data/unit_test/mtc_response.csv \
# 	--delimeter , \
# 	--pred_path Data/unit_test/mtc_predictor.csv \
# 	--out_folder unit_test \
# 	--output_label unit_test_rf \
# 	--title mtcars


write.csv(mtc_df, file.path(output_dir, "mtc_predictor.csv"), row.names = FALSE)
write.csv(mpg_df, file.path(output_dir, "mtc_response.csv"), row.names = FALSE)

# my_intersect <- intersect(row.names(mpg_df),
#                           row.names(mtc_df))

# cv_folds <- 10
# cross_vals <- split(sample(my_intersect, size = length(my_intersect)), cut(seq(length(my_intersect)), cv_folds))

# #### Loop through for random forest ####
# r_sqs <- c()
# topic <- c()

# for (colmn in 1:ncol(mpg_df)) {
  
#   for (cv in 1:cv_folds){
#     test_fold <- as.character(cross_vals[[cv]]) #SEQN for testing
#     train_fold <- as.character(my_intersect[!my_intersect %in% test_fold]) #SEQN for training
#     predct_tst <- mtc_df[test_fold,]
#     predct_trn <- mtc_df[train_fold,]
#     resp_trn <- mpg_df[train_fold, colmn]
#     resp_tst <- mpg_df[test_fold , colmn]
#     rf <- randomForest::randomForest(predct_trn, resp_trn)
#     print("made rf")
#     pred <- predict(rf, predct_tst)
#     my_lm <- lm(pred ~ resp_tst)
#     r_sqs <- c(r_sqs, summary(my_lm)$r.squared)
#     topic <- c(topic, names(mpg_df)[colmn])
#   }
# }

# print(r_sqs)
# print(topic)

# print(mean(r_sqs[which(topic == "mpg")]))

print("End R script.")

