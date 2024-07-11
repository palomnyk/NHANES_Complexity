# Author: Aaron Yerke (aaronyerke@gmail.com)
# Script for averaging and organizing BP dataset

rm(list = ls()) #clear workspace

#### Loading dependencies ####
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("nhanesA", quietly = TRUE)) BiocManager::install("nhanesA")
library("nhanesA")

#### Load functions ####
source(file.path("lib", "scripts","data_org", "data_org_func.R"))

# --------------------------------------------------------------------------
print("Establishing directory layout and other constants.")
# --------------------------------------------------------------------------
#### Establish directory layout and other constants ####
input_dir <- file.path("lib", "datasets")
output_dir <- file.path("Data", "respns_vars")
added_tables <- c()
not_added <- c()
full_df <- data.frame()
id_name <- "Respondent sequence number"
#### Loading in data ####
import_tables <- read.csv(file = file.path(input_dir, "response_features_tables.csv"),
                          header = T, comment.char = "#",
                          check.names =F)

for (i in 1:nrow(import_tables)){
  yr <- import_tables$year[i]
  dt_grp <- import_tables$data_group[i]
  nh_tbl <- import_tables$nh_table[i]
  if (!nh_tbl %in% added_tables){
    fname <- paste0(nh_tbl,"_", yr, ".csv")
    print(fname)
    fname_path <- file.path(output_dir, fname)
    if (!file.exists(fname_path)){
      print(paste(fname_path, "not found - Downloading!"))
      dl_tble <- nhanesA::nhanes(nh_tbl)
      write.csv(dl_tble, fname_path, row.names = FALSE)
      Sys.sleep(2)
    }else{
      dl_tble <- read.csv(fname_path, header = TRUE, check.names = FALSE)
    }
    dl_tble <- dl_tble[,sapply(dl_tble, function(x) !all(is.na(x)))]
    
    print(paste(names(dl_tble), collapse = "-"))
    save_cols <- c("SEQN", intersect(names(dl_tble),  toupper(import_tables$feature_short_name)))
    dl_tble <- dl_tble[, save_cols]
    dl_tble <- nhanesA::nhanesTranslate(nh_table = nh_tbl,
                                        data = dl_tble,
                                        details = TRUE,
                                        colnames = names(dl_tble))
    
    dl_tble <- nhanes_names(dl_tble, dt_grp, nh_tbl)
    
    attr(dl_tble, "names") <- sub("[[:punct:]]$", "", names(dl_tble)) 
    
    if(length(unique(dl_tble[,id_name])) == nrow(dl_tble)){
      if (nrow(full_df) < 1){
        full_df <- dl_tble
      }else{
        print(paste(nrow(full_df), nrow(dl_tble)))
        full_df <- merge(full_df, dl_tble, by = id_name,
                         all = TRUE)
      }
      added_tables <- c(added_tables, nh_tbl)
    }else{
      print(paste("Didn't add", nh_tbl))
      not_added <- c(not_added, nh_tbl)
    }
  }#1st if
}
#### Remove participants that are taking medication for their condition ####
rxq_2015 <- download_org_nhanes(dt_group = "Q",
                                 nh_tble = "RXQ_RX_I")
rxq_2015$all_descr <- paste(rxq_2015$`ICD 10 CM code 1 description`,
                            rxq_2015$`ICD 10 CM code 2 description`,
                            rxq_2015$`ICD 10 CM code 3 description`, sep = "")

rxq_2015$no_bp_med <- !grepl("hyperten",rxq_2015$all_descr, ignore.case = TRUE)

rxq_2015$no_chol_med <- !grepl("cholest",rxq_2015$all_descr, ignore.case = TRUE)

rxq_2015 <- rxq_2015[,c("no_bp_med","no_chol_med","Respondent sequence number")]

test <- grepl("triglyc",rxq_2015$all_descr, ignore.case = TRUE)
print(paste("any triglyceride medicine:", any(test == T)))

rxq_2015 <- aggregate(rxq_2015, by = list(rxq_2015$`Respondent sequence number`),
                      FUN = sum)


rxq_2015$`Respondent sequence number` = rxq_2015$Group.1

rxq_2015$no_bp_med <- as.logical(rxq_2015$no_bp_med)
rxq_2015$no_chol_med <- as.logical(rxq_2015$no_chol_med)

print(paste("There are", sum(rxq_2015$no_bp_med == FALSE), 
            "hbps and", sum(rxq_2015$no_chol_med == FALSE), "hchol to drop"))

nrow(rxq_2015)
nrow(full_df)
full_df <- merge(full_df, rxq_2015, by = id_name, all.x = TRUE)
#### Organize and categorize BP response vars###
prefix_to_average <- c("Systolic", "Diastolic")
threshold <- c(130, 80)

for (i in 1:length(prefix_to_average)){
  pre <- prefix_to_average[i]
  print(pre)
  my_cols <- unlist(lapply(names(full_df), function(x){startsWith(x, pre)}))
  print(my_cols)
  my_cols <- names(full_df)[my_cols]
  my_mean <- unlist(rowMeans(full_df[ , my_cols], na.rm=T))
  my_mean[which(full_df$no_bp_med == FALSE)] <- NA
  full_df[,paste0(pre, "_mean")] <- my_mean
  full_df[,paste0(pre, "_hypertension")] <- my_mean >= threshold[i]
  full_df <- full_df[,!(names(full_df) %in% my_cols)]
}
# t1 <- c(1,2,3,4,5)
# t1[which(t1 == 3)] <- NA

full_df$hypertension_either <- ifelse(full_df$`Systolic_hypertension` == TRUE | full_df$`Diastolic_hypertension` == TRUE,
                                      TRUE, FALSE)
#### Hardcode other blood lab result classifications ####
full_df$`Total Cholesterol (mg/dL`[which(full_df$no_chol_med == TRUE)] <- NA
full_df$`LDL-cholesterol (mg/dL`[which(full_df$`LDL-cholesterol (mg/dL` == TRUE)] <- NA
full_df$`Direct HDL-Cholesterol (mg/dL`[which(full_df$`Direct HDL-Cholesterol (mg/dL` == TRUE)]
full_df$unhealthy_tot_chol <- full_df$`Total Cholesterol (mg/dL` >= 200
full_df$unhealthy_trig <- full_df$`Triglyceride (mg/dL` >= 150
full_df$unhealthy_ldl <- full_df$`LDL-cholesterol (mg/dL` >= 100
full_df$unhealthy_hdl <- full_df$`Direct HDL-Cholesterol (mg/dL` < 60

remove_cols <- c("no_bp_med","no_chol_med","Group.1")
full_df <- subset(full_df, select = !(names(full_df) %in% remove_cols))


num_samples <- data.frame("num_samples" = colSums(!is.na(full_df)),
                          "resp_var" = names(full_df))
print(num_samples)
write.csv(num_samples,
          file = file.path(output_dir,"num_samples.csv"),
          row.names = FALSE)

write.csv(full_df,
          file = file.path(output_dir,"cardio_respns_vars.csv"),
          row.names = FALSE)

print("Script complete!")