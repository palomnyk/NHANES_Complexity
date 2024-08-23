# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for dividing the data into quintiles by age and looking for
# correlations to features in the 2009-2020 nutrition data.

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("readxl", quietly = TRUE)) BiocManager::install("readxl")
library("readxl")
if (!requireNamespace("ggplot2", quietly = TRUE)) BiocManager::install("ggplot2")
library("ggplot2")
if (!requireNamespace("optparse", quietly = TRUE)) BiocManager::install("optparse")
library("optparse")
if (!requireNamespace("reshape2", quietly = TRUE)) BiocManager::install("reshape2")
library("reshape2")

print("Loaded dependencies")
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

#### Functions ####
create_plot <- function(pred_df, pred_colnam, resp_df,resp_colnm, id_var,
                        title_group = "title") {
  shared_seqs <- intersect(pred_df[!is.na(pred_df[,"Sodium (mg"]),id_var],
                           resp_df[!is.na(resp_df[,"Systolic_mean"]),id_var])
  my_cor <- cor(pred_df[pred_df[,id_var] %in% shared_seqs,"Sodium (mg"], 
                resp_df[resp_df[,id_var] %in% shared_seqs,"Systolic_mean"])
  
  plot(pred_df[pred_df[,id_var] %in% shared_seqs,"Sodium (mg"], 
       resp_df[resp_df[,id_var] %in% shared_seqs,"Systolic_mean"],
       main = paste0(title_group, ", cor: ", round(my_cor,3)),
       xlab = pred_colnam,
       ylab = resp_colnm,
       cex = 0.1)
  # return(my_cor)
}

#### Parse commandline arguements ####
option_list <- list(
  optparse::make_option(c("-o", "--out_subdir"), type="character", 
                        default=file.path("diet_test"), 
                        help="dataset dir path"),
  optparse::make_option(c("-c", "--pred_col"), type="character", 
                        default="Sodium (mg", 
                        help="dataset dir path"),
  optparse::make_option(c("-r", "--resp_df"), type="character", 
                        default="Data/respns_vars/2009-2020cardio_respns_vars.csv", 
                        help="dataset dir path"),
  optparse::make_option(c("-d", "--diet_df"), type="character",
                        default="Data/diet/multi_year/d1_nutr_only_2009-2020.csv",
                        help="dataset dir path")
  # optparse::make_option(c("-d", "--diet_df"), type="character", 
  #                       default="Data/diet/d1_nutr_only_2015.csv", 
  #                       help="dataset dir path")
);


opt_parser <- optparse::OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

print("Commandline arguments:")
print(opt)


#### Establish directory layout and other constants ####
output_dir <- file.path("output", opt$out_subdir)
dir.create( output_dir)
dir.create( file.path(output_dir, "graphics"))
dir.create( file.path(output_dir, "tables"))

id_var <- "Respondent sequence number"

#### Loading in data ####
demo_2009_2020 <- read.csv("Data/demo/demo_2009-2020.csv", header = TRUE,
                           check.names = FALSE)
diet_df <- read.csv(opt$diet_df, header = TRUE, check.names = FALSE)
cardio_df <- read.csv(opt$resp_df, header = TRUE,check.names = FALSE)

#### Create variables to cycle through ####
age_low_range <- seq(from = 18, to = 80, by = 16)
age_upper_ranges <- age_low_range + 16
genders <- unique(demo_2009_2020[,"gender"])
ethnicities <- unique(demo_2009_2020[,"ethnicity"])
num_rows <- length(ethnicities) * length(age_low_range)

resp_base_fn <- sub('\\.csv$', '', basename(opt$resp_df))
diet_base_fn <- sub('\\.csv$', '', basename(opt$diet_df))

pdf(file = file.path(output_dir, "graphics", paste0(resp_base_fn,"_VS_",opt$pred_col, ".pdf")))
for (r in 1:ncol(cardio_df)) {
  resp_col <- names(cardio_df)[r]
  for (g in 1:length(genders)){
    # Create data to fill in
    age_range_ord <- character(length = length(age_low_range))#for order in factor level
    cor_table <- data.frame(age_range = character(length = num_rows),
                            ethnicities = character(length = num_rows),
                            pearson = numeric(length = num_rows))
    row_count <- 1
    for (a in 1:length(age_low_range)){
      age_range <- paste0(age_low_range[a], "-", as.integer(age_upper_ranges[a])-1)
      age_range_ord[a] <- age_range
      for (e in 1:length(ethnicities)){
        sub_demo <- demo_2009_2020[demo_2009_2020$gender == genders[g] &
                                     demo_2009_2020$ethnicity == ethnicities[e] &
                                     demo_2009_2020$age >= age_low_range[a] &
                                     demo_2009_2020$age < age_upper_ranges[a],]
        age_range_ord[a] <- paste0(age_low_range[a], "-", age_upper_ranges[a])
        shared_seqs <- intersect(diet_df[!is.na(diet_df[,]),id_var],
                                 cardio_df[!is.na(cardio_df[,resp_col]),id_var])
        shared_seqs <- intersect(shared_seqs, sub_demo$`Respondent sequence number`)
        my_cor <- cor(diet_df[diet_df[,id_var] %in% shared_seqs,opt$pred_col], 
                      cardio_df[cardio_df[,id_var] %in% shared_seqs,resp_col],
                      method = "kendall")
        
        cor_table$age_range[row_count] <- age_range
        cor_table$ethnicities[row_count] <- ethnicities[e]
        cor_table$pearson[row_count] <- my_cor^2
        row_count <- row_count + 1
      }# for e
    }# for a
    # cor_table$age_range <- factor(cor_table$age_range, levels = age_range_ord)
    p <- ggplot2::ggplot(cor_table, aes(x = ethnicities, y=pearson, fill = age_range)) +
      ggplot2::geom_col(position="dodge", width=0.85, color="black") +
      ggplot2::ggtitle(paste0(genders[g], ": ", resp_col, " VS ", opt$pred_col)) +
      ggplot2::xlab(diet_base_fn) +
      ggplot2::ylab(expression(paste("Kendall R"^"2"))) + #create superscript
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.line = element_line(color="black"),
                     axis.ticks = element_line(color="black"),
                     panel.border = element_blank(),
                     axis.text.x = element_text(angle = 45, hjust=1),
                     text=element_text(size=15), #change font size of all text
                     axis.text=element_text(size=15), #change font size of axis text
                     axis.title=element_text(size=17), #change font size of axis titles
                     plot.title=element_text(size=17), #change font size of plot title
                     legend.text=element_text(size=15), #change font size of legend text
                     legend.title=element_text(size=16)) #change font size of legend title
    print(p)
  }# for g
}#r
dev.off()
# create_plot(diet_df, opt$pred_col, cardio_df, "Systolic_mean", id_var, "No subsets")

# sub_demo <- demo_2009_2020[demo_2009_2020$gender == "Male" &
#                              demo_2009_2020$age >= 18,]
# sub_diet <- diet_df[diet_df[,id_var] %in% sub_demo$`Respondent sequence number`,]
# create_plot(sub_diet, opt$pred_col, cardio_df, "Systolic_mean", id_var,
#             "All male adults")
# 
# sub_demo <- demo_2009_2020[demo_2009_2020$gender == "Female" &
#                              demo_2009_2020$age >= 18,]
# sub_diet <- diet_df[diet_df[,id_var] %in% sub_demo$`Respondent sequence number`,]
# create_plot(sub_diet, opt$pred_col, cardio_df, "Systolic_mean", id_var,
#             "All female adults")
# 
# sub_demo <- demo_2009_2020[demo_2009_2020$gender == "Female" &
#                              demo_2009_2020$age >= 60,]
# sub_diet <- diet_df[diet_df[,id_var] %in% sub_demo$`Respondent sequence number`,]
# create_plot(sub_diet, opt$pred_col, cardio_df, "Systolic_mean", id_var,
#             "Females 60 and older")
# 
# sub_demo <- demo_2009_2020[demo_2009_2020$gender == "Male" &
#                              demo_2009_2020$age >= 70,]
# sub_diet <- diet_df[diet_df[,id_var] %in% sub_demo$`Respondent sequence number`,]
# create_plot(sub_diet, opt$pred_col, cardio_df, "Systolic_mean", id_var,
#             "Males 70 and older")
print("End of R script!")
