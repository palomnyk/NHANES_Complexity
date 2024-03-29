# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making graphic that compares the various methods of organizing
# diet data.

# Namespace(pred_table='Data/diet/d1_nutri_food_g_2015.csv', meta_col=True, output_label='nutr_food_grams', use_all_meta=False, out_folder='diet_test', resp_fn='Data/resp_vars/cardio_respns_vars.csv', delim='\t', meta_index_col=0, title='nutri_food_grams')

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("readxl", quietly = TRUE)) BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("ggplot2", quietly = TRUE)) BiocManager::install("ggplot2")
library("ggplot2")


print("Loaded dependencies")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Establish directory layout and other constants ####
output_dir <- file.path("output", "diet_test")

#### Loading in data ####
id_var <- "Respondent sequence number"

# get file names in output_dir
dir_files <- list.files(file.path(output_dir, "tables"), pattern = "_data.csv")
  
print(paste("data files found:", paste(dir_files, collapse = ", ")))

response_var <- vector(mode = "character")
org_method <- vector(mode = "character")
score <- vector(mode = "numeric")

for (i in 1:length(dir_files)){
  dat_f <- dir_files[i]
  print(dat_f)
  dat_f_path <- file.path(output_dir, "tables", dat_f)
  if (file.size(dat_f_path) > 0){
    my_table <- read.csv(dat_f_path,
                         header = T)
    # print(my_table)
    for (r in 2:nrow(my_table)){
      if (my_table$response_var[r] != id_var){
        response_var <- c(response_var, rep(my_table$response_var[r], 10))
        org_method <- c(org_method, rep(gsub("_data.csv", "", dat_f), 10))
        score <- c(score, unlist(my_table[r, 3:ncol(my_table)]))
      }
    }
    
  }else{
    print(paste(dat_f, "is empty"))
  }

}

big_table <- data.frame(org_method, response_var, score)

print(unique(big_table$response_var))
old <- unique(big_table$response_var)

#### Order response vars such that numeric are 1st and boolean are 2nd ####
my_order <- c(
  "Total Cholesterol (mg/dL",	"Triglyceride (mg/dL",	"LDL-cholesterol (mg/dL",
  "Direct HDL-Cholesterol (mg/dL",	"Systolic_mean",	"Diastolic_mean",
  "Systolic_hypertension", "Diastolic_hypertension",	"hypertension_either",
  "unhealthy_tot_chol",	"unhealthy_trig",	"unhealthy_ldl",	"unhealthy_hdl")


big_table$response_var <- factor(big_table$response_var, levels = my_order)

means <- data.frame(aggregate(big_table$score, by=list(big_table$org_method), mean))
names(means) <- c("org_method", "x")
pdf(file.path(output_dir, "graphics", "all_diet_orgs.pdf"))
g <- ggplot2::ggplot(big_table, aes(x=response_var, y=score)) + 
  geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(label = paste("Nutrition data org strategies full result")) +
  ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=1) +
  facet_grid(~ org_method)
g

means <- aggregate(big_table$score, by=list(big_table$response_var), mean)
names(means) <- c("response_var", "x")

g <- ggplot2::ggplot(big_table, aes(x=org_method, y=score)) + 
  geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(label = paste("Nutrition data organization strategies full result")) +
  ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=1) +
  facet_grid(~response_var)
g


big_table <- big_table[ ! grepl("trig", big_table$response_var, ignore.case = TRUE), ]
means <- aggregate(big_table$score, by=list(big_table$org_method), mean)
names(means) <- c("org_method", "x")
g <- ggplot2::ggplot(big_table, aes(x=response_var, y=score)) + 
  geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(label = paste("Nutrition data organization strategies NO TRIG")) +
  ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=1) +
  facet_grid(~ org_method)
g

means <- aggregate(big_table$score, by=list(big_table$response_var), mean)
names(means) <- c("response_var", "x")
g <- ggplot2::ggplot(big_table, aes(x=org_method, y=score)) + 
  geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(label = paste("Nutrition data organization strategies NO TRIG")) +
  ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=1) +
  facet_grid(~response_var)
g

numeric_only <- c("Total Cholesterol (mg/dL","Triglyceride (mg/dL", 
                  "LDL-cholesterol (mg/dL","Direct HDL-Cholesterol (mg/dL",
                  "Systolic_mean", "Diastolic_mean")

num_table <- big_table[!(big_table$response_var %in% numeric_only),]
means <- aggregate(num_table$score, by=list(num_table$org_method), mean)
names(means) <- c("org_method", "m")
stdv <- aggregate(num_table$score, by=list(num_table$org_method), sd)
names(stdv) <- c("org_method", "s")
means <- merge(means, stdv)
means$x <- round(means$m, 3)
g <- ggplot2::ggplot(num_table, aes(x=response_var, y=score)) + 
  geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(label = paste("Nutrition data organization strategies NO TRIG CAT ONLY")) +
  geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
  ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
  ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
  ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
  ggplot2::facet_grid(~ org_method)
g

dev.off()
