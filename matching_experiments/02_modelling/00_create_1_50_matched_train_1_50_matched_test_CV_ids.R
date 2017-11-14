------------------------------------------------------------------------
  # SPLIT PATIENT IDS INTO TRAINING/TESTING FOR EACH CV FOLD
  #  ------------------------------------------------------------------------

library(tidyverse)
library(mlr)
library(xgboost)
library(palabmod)
# library(PRROC)

# globals -----------------------------------------------------------------

results_dir <- "F:/Projects/Strongbridge/results/matching_experiments/modelling/"
data_dir <- 'F:/Projects/Strongbridge/data/matching_experiments/02_modelling/'
cohort_dir <- '00_matched_train_unmatched_test/'
test_strat <- 'standard_5_fold_CV/'
full_data_dir <- paste0(data_dir, test_strat)

#Create output dir
dir.create(full_data_dir, recursive = TRUE, showWarnings = FALSE)

# NOTE: FOR NOW JUST CONVERT THE INDICES LACHLAN USED BACK INTO IDS TO ENSURE
# SAME INDICES WERE USED FOR BOTH PROJECT AND EXPERIMENT

#Load dataset used for project - only load positives and training negatives
# for standard 5 fold CV with 1:50 train/test ratio
adv_data_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
combined <- read_rds(paste0(adv_data_dir, "05_combined_train_unmatched_test_capped_freq_datediff.rds")) %>%
  dplyr::filter(subset %in% c('pos', 'train_neg'))

#Load indices used for train (not using test as just using 1:50 train data)
# NOTE: THESE INDICES GROUP MATCHED POS-NEG TOGETHER
train_indices <- read_rds(paste0(data_dir, "train_indices.rds"))

#Convert Patient ids to factor
combined$PATIENT_ID <- as.numeric(combined$PATIENT_ID)

#Get patient ids for indices
train_ids <- list()
test_ids <- list()
for (i in 1:length(train_indices)){
  train_ids[[i]] <- combined$PATIENT_ID[train_indices[[i]]]
  test_ids[[i]] <- combined$PATIENT_ID[!(combined$PATIENT_ID %in% train_ids[[i]])]
  print(paste(length(train_ids[[i]]), length(test_ids[[i]]),length(train_ids[[i]])+ length(test_ids[[i]]) ))
  print(any(test_ids[[i]] %in% train_ids[[i]]))
}

#Save 
write_rds(list(train_ids = train_ids, test_ids = test_ids), 
          paste0(full_data_dir, "1_50_matched_train_1_50_matched_test_CV_ids.rds"))
