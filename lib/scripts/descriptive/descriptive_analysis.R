#Author: Aaron Yerke, aaronyerke@gmail.com
#Script purpose to post information about the NHANES data

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")

#### Establish directory layout and other constants ####
output_dir <- file.path("output")

#### Loading in data ####
# diet_2015 <- read.csv(file = file.path(output_dir, "demo_diet_2015_match_bp_SYST1_RF.csv"), header = T,
#                       check.names = F)
# BP_2015 <- read.csv(file = file.path(output_dir, "BP_2015_match_diet_SYST1_RF.csv"), header = T,
#                     check.names = F)
demo_2015 <- read.csv(file = file.path(output_dir,"tables", "demo_2015.csv"),
                      header = T,
                      check.names =F)



print("loeaded data")

my_table <- demo_2015
output_label <- "demo_2015_descriptives"
cat_out_name <- paste0(output_label, ".txt")
num_out_name <- paste0(output_label, ".pdf")


cat("Description of current population categories \n", file = cat_out_name)

pdf(file = file.path(output_dir, "graphics", num_out_name))
for (feat in 1:ncol(my_table)){
  my_feat <- colnames(my_table)[feat]
  counts <- my_table[,feat]
  if (is.numeric(counts)){
    hist(counts, main = substr(my_feat, 1, 65), xlab = "", breaks=100)
  }else{
    cat(paste0(my_feat, "\n"), file = cat_out_name, append = TRUE)
    print(my_feat)
    # print(table(counts))
    par(mar=c(20,4,4,4))
    barplot(table(counts), main = substr(my_feat, 1, 65), las = 3)
    # my_string <- paste(my_string,"\n",str(table(counts)))
    # cat(str(data.frame(table(counts))), file = cat_out_name, append = T)
  }
  
}
dev.off()
