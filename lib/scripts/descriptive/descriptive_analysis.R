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
  optparse::make_option(c("-d", "--homedir"), type="character", 
                        default=file.path('~','git',"NHANES_Complexity"), 
                        help="dataset git repo path"),
  optparse::make_option(c("-a", "--data"), type="character", default="dframe.csv",
                        help="filename in tables folder"),
  optparse::make_option(c("-l", "--delim"), type="character", default="\t",
                        help="metadata file deliminator", metavar="character"),
  optparse::make_option(c("-c", "--colnames"), type="character", default=1,
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
  --output_fname BpLowSparcity
  
'

opt_parser <- optparse::OptionParser(option_list=option_list);

opt <- optparse::parse_args(opt_parser);

print(opt)

# --------------------------------------------------------------------------
print("Establishing directory layout and other constants.")
# --------------------------------------------------------------------------
home_dir <- opt$homedir
project <- opt$project
output_dir <- file.path(home_dir, 'output')
#### Establish directory layout and other constants ####
output_dir <- file.path("output")

#### Loading in data ####
my_table <- read.csv(file = file.path(output_dir,"tables", opt$data),
                     header = T,
                     check.names =F)

output_label <- paste0("descr_", opt$output_fname)
desc_columns <- unlist(strsplit(opt$colnames, ","))
print(desc_columns)

out_name <- paste0(output_label, ".pdf")

pdf(file = file.path(output_dir, "graphics", out_name))
for (feat in desc_columns){
  counts <- my_table[,feat]
  supstring <- paste("n observation:", length(!is.na(counts)))
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
}
dev.off()
