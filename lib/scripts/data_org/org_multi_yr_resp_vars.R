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
output_dir <- file.path("Data", "respns_vars")
dir.create(output_dir)

added_tables <- c()
not_added <- c()
full_df <- data.frame()#final data goes here
id_name <- "Respondent sequence number"
#### Loading in data ####
import_tables <- read.csv(file = file.path("lib", "datasets", "response_features_multi_year.csv"),
                          header = T, comment.char = "#",
                          check.names =F)
my_years <- unique(import_tables$start_year)
for (yr in my_years){
  sub_table <- import_tables[import_tables$start_year == yr, ]
  year_df <- data.frame()
  
  for (i in 1:nrow(sub_table)){
    dt_grp <- sub_table$data_group[i]
    nh_tbl <- sub_table$nh_table[i]
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
      save_cols <- c("SEQN", intersect(names(dl_tble),  toupper(sub_table$feature_short_name)))
      dl_tble <- dl_tble[, save_cols]
      print(paste(names(dl_tble), collapse = "%"))
      dl_tble <- nhanesA::nhanesTranslate(nh_table = nh_tbl,
                                          data = dl_tble,
                                          details = TRUE,
                                          colnames = names(dl_tble))
      stopifnot(typeof(dl_tble) == list())
      print(paste(names(dl_tble), collapse = "+"))
      dl_tble <- nhanes_names(dl_tble, dt_grp, nh_tbl)
      stopifnot(typeof(dl_tble) == list())
      # print(paste(names(dl_tble), collapse = "*"))
      attr(dl_tble, "names") <- gsub("/, Friedewald equation (mg/dL).LBDLDL = (LBXTC-(LBDHDD + LBXTR/5), round to 0 decimal places) for LBXTR less than 400 mg/dL, and missing for LBX/$",
                                     " (mg/dL", names(dl_tble))
      attr(dl_tble, "names") <- gsub("[[:punct:]]$", "", names(dl_tble))
      attr(dl_tble, "names") <- gsub("[\r\n]", "", names(dl_tble)) 
      attr(dl_tble, "names") <- gsub("\\( ", " (", names(dl_tble))
      attr(dl_tble, "names") <- gsub("\\s+", " ", names(dl_tble))
      attr(dl_tble, "names") <- gsub("Respondent Sequence Number", id_name, names(dl_tble))
      attr(dl_tble, "names") <- gsub(" - 1st oscillometric reading", ": Blood pressure (first reading) mm Hg", names(dl_tble))
      attr(dl_tble, "names") <- gsub(" - 2nd oscillometric reading", ": Blood pressure (second reading) mm Hg", names(dl_tble))
      attr(dl_tble, "names") <- gsub(" - 3rd oscillometric reading", ": Blood pressure (third reading) mm Hg", names(dl_tble))
      
      #hack to change name of problematic column
      odd_name <- which(names(dl_tble) == "LDL-Cholesterol, Friedewald equation (mg/dL).LBDLDL = (LBXTC-(LBDHDD + LBXTR/5), round to 0 decimal places) for LBXTR less than 400 mg/dL, and missing for LBX")
      print(paste("odd_name:", odd_name))
      names(dl_tble)[odd_name] <- "LDL-cholesterol (mg/dL"
      
      print(paste(names(dl_tble), collapse = "*"))
      
      if(length(unique(dl_tble[,id_name])) == nrow(dl_tble)){
        if (nrow(year_df) < 1){
          year_df <- dl_tble
        }else{
          print(paste(yr, "merge year_df and dl_table:", nrow(year_df), nrow(dl_tble)))
          year_df <- merge(year_df, dl_tble, by = id_name,
                           all = TRUE)
        }
        added_tables <- c(added_tables, nh_tbl)
      }else{
        print(paste("Didn't add", nh_tbl))
        not_added <- c(not_added, nh_tbl)
      }
    }#1st if
    
  }#end for n in nrow(sub_table)
  
  # The 2017-2020 data only has 3 bp readings, so need to add 4th empty one
  # for rind to work properly
  sometimes_missing <- c("Systolic: Blood pressure (fourth reading if necessary) mm Hg",
            "Diastolic: Blood pressure (fourth reading if necessary) mm Hg")
  for (missing in sometimes_missing) {
    if (!missing %in% names(year_df)){
      empty_vec <- rep(NA, nrow(year_df))
      year_df <- cbind(year_df, empty_vec)
      names(year_df)[ncol(year_df)] <- missing
    }
  }
  
  #### Remove participants that are taking medication for their condition ####
  # rxq_2015 <- download_org_nhanes(dt_group = "Q",
  #                                 nh_tble = sub_table$drug_quest_table[i])
  # rxq_2015$all_descr <- paste(rxq_2015$`ICD 10 CM code 1 description`,
  #                             rxq_2015$`ICD 10 CM code 2 description`,
  #                             rxq_2015$`ICD 10 CM code 3 description`, sep = "")
  # print("herer")
  # rxq_2015$no_bp_med <- !grepl("hyperten",rxq_2015$all_descr, ignore.case = TRUE)
  # 
  # rxq_2015$no_chol_med <- !grepl("cholest",rxq_2015$all_descr, ignore.case = TRUE)
  # 
  # rxq_2015 <- rxq_2015[,c("no_bp_med","no_chol_med","Respondent sequence number")]
  # 
  # test <- grepl("triglyc",rxq_2015$all_descr, ignore.case = TRUE)
  # print(paste("any triglyceride medicine:", any(test == T)))
  # 
  # rxq_2015 <- aggregate(rxq_2015, by = list(rxq_2015$`Respondent sequence number`),
  #                       FUN = sum)
  # 
  # 
  # rxq_2015$`Respondent sequence number` = rxq_2015$Group.1
  # 
  # rxq_2015$no_bp_med <- as.logical(rxq_2015$no_bp_med)
  # rxq_2015$no_chol_med <- as.logical(rxq_2015$no_chol_med)
  # 
  # print(paste("There are", sum(rxq_2015$no_bp_med == FALSE),
  #             "hbps and", sum(rxq_2015$no_chol_med == FALSE), "hchol to drop"))
  # 
  # year_df <- merge(year_df, rxq_2015, by = id_name, all.x = TRUE)

  if (nrow(full_df) < 1){
      full_df <- year_df
  }else{
    print(paste(nrow(year_df), nrow(dl_tble)))
    full_df <- rbind(full_df, year_df)
  }
}
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
          file = file.path(output_dir,"2009-2020num_samples.csv"),
          row.names = FALSE)

write.csv(full_df,
          file = file.path(output_dir,"2009-2020cardio_respns_vars.csv"),
          row.names = FALSE)

print("Script complete!")
