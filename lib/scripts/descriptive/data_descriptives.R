#Author: Aaron Yerke, aaronyerke@gmail.com
#Script purpose to post information about the NHANES data

rm(list = ls()) #clear workspace

#### Loading dependencies ####
# if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("optparse", quietly = TRUE)) install.packages("optparse")
library("optparse")
# --------------------------------------------------------------------------
print("Reading cml arguments")
# --------------------------------------------------------------------------
option_list <- list(
  optparse::make_option(c("-a", "--data"), type="character", default="Data/respns_vars/2009-2020cardio_respns_vars.csv",
                        help="filename in tables folder"),
  optparse::make_option(c("-l", "--delim"), type="character", default="\t",
                        help="metadata file deliminator", metavar="character"),
  optparse::make_option(c("-c", "--colnames"), type="character", default=FALSE,
                        help="metadata file row to use for row names"),
  # callback = strsplit(), callback_args = "split = ','"),
  optparse::make_option(c("-o", "--output_fname"), type="character", default="descriptives",
                        help="output file name")
); 

'
Rscript ./lib/scripts/descriptive/descriptives_analysis.R \
  --data PHTHTE_PAQ_noRx_demo_d1_diet_2015_match_bp_SYST1_RF.csv \
  --colnames "Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age.," \
  --delim "," \
  --output_fname ageLowSparcity
  
Rscript ./lib/scripts/descriptive/descriptive_analysis.R \
  --data PHTHTE_PAQ_noRx_BP_2015_match_diet_SYST1_RF.csv \
  --colnames "Systolic_Hypertension,Diastolic_Hypertension,Diastolic:  Blood pressure (first reading) mm Hg,Systolic:  Blood pressure (first reading) mm Hg" \
  --delim "," \
  --output_fname cardio_respns_vars_2009-2020
  
Rscript ./lib/scripts/descriptive/data_descriptives.R \
  --data Data/respns_vars/2009-2020cardio_respns_vars.csv \
  --delim "," \
  --output_fname cardio_respns_vars_2009-2020
'

opt_parser <- optparse::OptionParser(option_list=option_list);

opt <- optparse::parse_args(opt_parser);

print(opt)

# --------------------------------------------------------------------------
print("Establishing directory layout and other constants.")
#### Establish directory layout and other constants ####
output_label <- opt$output_fname
output_dir <- file.path("output", "descriptives")
dir.create(output_dir)
dir.create(file.path(output_dir, "graphics"))
dir.create(file.path(output_dir, "tables"))

#### Loading in data ####
my_table <- read.csv(file = file.path( opt$data),
                     header = T,
                     check.names =F)

if (opt$colnames == FALSE){
  desc_columns <- names(my_table)
}else{
  desc_columns <- unlist(strsplit(opt$colnames, ","))
}
print(desc_columns)

pdf_name <- paste0(output_label, ".pdf")
pdf(file = file.path(output_dir, "graphics", pdf_name))

feature <- c()
feat_count <- c()

for (feat in desc_columns){
  counts <- my_table[,feat]
  no_na <- sum(!is.na(counts))
  supstring <- paste("n observation:", no_na)
  # print(supstring)
  if (is.numeric(counts)){
    hist(counts, main = substr(feat, 1, 65), xlab = supstring, breaks=100)
  }
  else{
    par(mar=c(20,4,4,4))
    barplot(table(counts), main = substr(feat, 1, 65), las = 3,
            xlab = supstring)
    # my_string <- paste(my_string,"\n",str(table(counts)))
    # cat(str(data.frame(table(counts))), file = cat_out_name, append = T)
  }
  feature <- c(feature, feat)
  feat_count <- c(feat_count, no_na)
}

dev.off()


count_table <- data.frame(feature, feat_count)

write.csv(count_table, file = file.path(output_dir, "tables", paste0(output_label, ".csv")),
          row.names = FALSE)

print("End of R script!")
