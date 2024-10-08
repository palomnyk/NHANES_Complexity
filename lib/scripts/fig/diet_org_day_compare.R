# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for making graphic that compares the various methods of organizing
# diet data.
# Also compares d1 vs d1d2,
#   food vs cat/nutrition,
#   for each response variable.

rm(list = ls()) #clear workspace
chooseCRANmirror(graphics=FALSE, ind=66)

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE)) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")

print("Loaded dependencies")
# source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Functions ####
org_meth_plot <- function (df_table, title_text){
  means <- aggregate(df_table$score, by=list(df_table$org_name), mean)
  names(means) <- c("org_name", "m")
  stdv <- aggregate(df_table$score, by=list(df_table$org_name), sd)
  names(stdv) <- c("org_name", "s")
  means <- merge(means, stdv)
  means$x <- round(means$m, 3)
  g <- ggplot2::ggplot(df_table, aes(x=response_var, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
    ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
    ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
    ggplot2::facet_grid(~ org_name)
  return(g)
}
resp_vars_plot <- function (df_table, title_text){
  means <- aggregate(df_table$score, by=list(df_table$response_var), mean)
  names(means) <- c("response_var", "m")
  stdv <- aggregate(df_table$score, by=list(df_table$response_var), sd)
  names(stdv) <- c("response_var", "s")
  means <- merge(means, stdv)
  means$x <- round(means$m, 3)
  g <- ggplot2::ggplot(df_table, aes(x=org_name, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
    ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
    ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
    ggplot2::facet_grid(~ response_var)
  return(g)
}

plot_data <- function (df_table, f_column = "org_name", title_text){
  means <- aggregate(df_table[,"score"], by=list(df_table[,f_column]), mean)
  names(means) <- c(f_column, "m")
  stdv <- aggregate(df_table$score, by=list(df_table[,f_column]), sd)
  names(stdv) <- c(f_column, "s")
  means <- merge(means, stdv)
  means$x <- round(means$m, 3)
  g <- ggplot2::ggplot(df_table, aes(x=response_var, y=score)) + 
    geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(label = paste(title_text)) +
    geom_text(data = means, aes(label=paste("m:",x), y=x + 0.1, x = 3), color="black") +
    ggplot2::scale_x_discrete(guide = guide_axis(angle = 90)) +
    ggplot2::geom_hline(data = means, aes(yintercept = x), color="red", linetype=4) +
    ggplot2::geom_hline(data = means, aes(yintercept = x + s), color="blue", linetype=2) +
    ggplot2::geom_hline(data = means, aes(yintercept = x - s), color="blue", linetype=2) +
    ggplot2::facet_grid(~ f_column)
  return(g)
}

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("proper_days"), 
                        help="dataset dir path")
);
opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)

#### Establish directory layout and other constants ####
output_dir <- file.path("output", opt$out_subdir)

numeric_only <- c("Total Cholesterol (mg/dL","Triglyceride (mg/dL", 
                  "LDL-cholesterol (mg/dL","Direct HDL-Cholesterol (mg/dL",
                  "Systolic_mean", "Diastolic_mean")

#### Load and organize data ####
id_var <- "Respondent sequence number"

# get file names in output_dir
dir_files <- list.files(file.path(output_dir, "tables"), pattern = "_data.csv")
  
print(paste("data files found:", paste(dir_files, collapse = ", ")))

response_var <- vector(mode = "character")
org_name <- vector(mode = "character")
org_method <- vector(mode = "character")
score <- vector(mode = "numeric")
day <- vector(mode = "character")
includes_food <- vector(mode = "logical")


##### Iterate through files and populate variables #####
for (i in 1:length(dir_files)){
  dat_f <- dir_files[i]
  print(dat_f)
  dat_f_path <- file.path(output_dir, "tables", dat_f)
  if (file.size(dat_f_path) > 0){
    my_table <- read.csv(dat_f_path,
                         header = T)
    if (grepl("-d1d2-", dat_f)){
      my_day <- "d1d2"
    }else{
      my_day <- "d1"
    }
    # print(my_table)
    for (r in 2:nrow(my_table)){
      if (my_table$response_var[r] != id_var){
        response_var <- c(response_var, rep(my_table$response_var[r], 10))
        org_name <- c(org_name, rep(gsub("-2015_scores.csv", "", dat_f), 10))
        day <- c(day, rep(my_day, 10))
        score <- c(score, unlist(my_table[r, 3:ncol(my_table)]))
        my_method <- gsub("_scores.csv", "", dat_f)
        my_method <- gsub(paste0("-",my_day,"-"), "", my_method)
        my_method <- gsub("2015", "", my_method)
        org_method <- c(org_method, rep(my_method,10))
      }
    }
    if (grepl("food", dat_f, ignore.case = TRUE)){
      includes_food[i] <- TRUE
    }else{
      includes_food[i] <- FALSE
    }
    
  }else{
    print(paste(dat_f, "is empty"))
  }

}

big_table <- data.frame(org_name, org_method, response_var, day, includes_food, score)

##### Order response vars such that numeric are 1st and Boolean are 2nd #####
my_order <- c(
  "Total Cholesterol (mg/dL",	"Triglyceride (mg/dL",	"LDL-cholesterol (mg/dL",
  "Direct HDL-Cholesterol (mg/dL",	"Systolic_mean",	"Diastolic_mean",
  "Systolic_hypertension", "Diastolic_hypertension",	"hypertension_either",
  "unhealthy_tot_chol",	"unhealthy_trig",	"unhealthy_ldl",	"unhealthy_hdl")

big_table$response_var <- factor(big_table$response_var, levels = my_order)
print(unique(big_table$response_var))

#### Analysis ####
# pdf(file.path(output_dir, "graphics", "all_diet_orgs_days.pdf"), width = 18, height = 10)
##### ANOVA for days with plot #####
myLm = lm(score ~ day, data = big_table)
pval = anova(myLm)$"Pr(>F)"[1]
means <- aggregate(score ~ day, data = big_table, mean)

g <- ggplot2::ggplot(big_table, aes(x=day, y=score)) + 
  ggplot2::geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(paste0("RF Scores: 1 day VS 2 days, ANOVA pval: ",
                          round(pval,4))) +
  geom_text(data = means, aes(label = score, y = score + 0.08))
g

##### ANOVA for includes_food with plot #####
myLm = lm(score ~ includes_food, data = big_table)
pval = anova(myLm)$"Pr(>F)"[1]
means <- aggregate(score ~ includes_food, data = big_table, mean)

g <- ggplot2::ggplot(big_table, aes(x=includes_food, y=score)) + 
  ggplot2::geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(paste0("RF Scores: includes_food VS no_food, ANOVA pval: ",
                          round(pval,4))) +
  geom_text(data = means, aes(label = score, y = score + 0.08))
g

##### Compare organizational methods #####
myLm = lm(score ~ org_method, data = big_table)
pval = anova(myLm)$"Pr(>F)"[1]
means <- aggregate(score ~ org_method, data = big_table, mean)
g <- ggplot2::ggplot(big_table, aes(x=org_method, y=score)) + 
  ggplot2::geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(paste0("RF Scores: org methods (d1 and d1d2), ANOVA pval: ",
                          round(pval,4))) +
  geom_text(data = means, aes(label = round(score,4), y = score + 0.08))
g


myLm = lm(score ~ org_name, data = big_table)
pval = anova(myLm)$"Pr(>F)"[1]
means <- aggregate(score ~ org_name, data = big_table, mean)
g <- ggplot2::ggplot(big_table, aes(x=org_name, y=score)) + 
  ggplot2::geom_boxplot() +
  ggplot2::ylab("Score") +
  ggplot2::ggtitle(paste0("RF Scores: org methods with days, ANOVA pval: ",
                          round(pval,4))) +
  geom_text(data = means, aes(label = round(score,4), y = score + 0.08))+
  ggplot2::theme(axis.line = element_line(color="black"),
                 axis.ticks = element_line(color="black"),
                 panel.border = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust=1),
                 text=element_text(size=15), #change font size of all text
                 axis.text=element_text(size=15), #change font size of axis text
                 axis.title=element_text(size=17), #change font size of axis titles
                 plot.title=element_text(size=17), #change font size of plot title
                 legend.text=element_text(size=12), #change font size of legend text
                 legend.title=element_text(size=13)) #change font size of legend title
g

print("For loop for response vars")
for (resp in unique(big_table$response_var)){
  print(resp)
  my_table <- big_table[big_table$response_var == resp, ]
  myLm = lm(score ~ org_name, data = my_table)
  pval = anova(myLm)$"Pr(>F)"[1]
  means <- aggregate(score ~ org_name, data = my_table, mean)
  g <- ggplot2::ggplot(my_table, aes(x=org_name, y=score)) + 
    ggplot2::geom_boxplot() +
    ggplot2::ylab("Score") +
    ggplot2::ggtitle(paste0("RF Scores: org methods with days, ",resp," only, ANOVA pval: ",
                            round(pval,4))) +
    geom_text(data = means, aes(label = round(score,4), y = score + 0.08))+
    ggplot2::theme(axis.line = element_line(color="black"),
                   axis.ticks = element_line(color="black"),
                   panel.border = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust=1),
                   text=element_text(size=15), #change font size of all text
                   axis.text=element_text(size=15), #change font size of axis text
                   axis.title=element_text(size=17), #change font size of axis titles
                   plot.title=element_text(size=17), #change font size of plot title
                   legend.text=element_text(size=12), #change font size of legend text
                   legend.title=element_text(size=13)) #change font size of legend title
  print(g)
}



##### Facade plots ####
org_meth_plot(big_table, "Nutrition data org strategies full result")
resp_vars_plot(big_table, "Nutrition data org strategies full result")


num_table <- big_table[!(big_table$response_var %in% numeric_only),]
org_meth_plot(num_table, "Nutrition data organization strategies CAT ONLY")
resp_vars_plot(num_table, "Nutrition data organization strategies CAT ONLY")

num_table <- big_table[big_table$response_var %in% numeric_only,]
org_meth_plot(num_table, "Nutrition data organization strategies NUM ONLY")
resp_vars_plot(num_table, "Nutrition data organization strategies NUM ONLY")

big_table <- big_table[ ! grepl("trig", big_table$response_var, ignore.case = TRUE), ]
org_meth_plot(big_table, "Nutrition data organization strategies NO TRIG")
resp_vars_plot(big_table, "Nutrition data organization strategies NO TRIG")

num_table <- big_table[!(big_table$response_var %in% numeric_only),]
org_meth_plot(num_table, "Nutrition data organization strategies NO TRIG CAT ONLY")
resp_vars_plot(num_table, "Nutrition data organization strategies NO TRIG CAT ONLY")

num_table <- big_table[big_table$response_var %in% numeric_only,]
org_meth_plot(num_table, "Nutrition data organization strategies NO TRIG NUM ONLY")
resp_vars_plot(num_table, "Nutrition data organization strategies NO TRIG NUM ONLY")

# dev.off()

print("End of R script!")
