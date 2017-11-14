
#  ------------------------------------------------------------------------
# PRELIMINARY MODELLING - XGBOOST FREQUENCIES ONLY
#  ------------------------------------------------------------------------

rm(list = ls())
gc()

library(tidyverse)
library(mlr)
library(xgboost)
library(palabmod)
source('F:/Shaun/Strongbridge/Code/strongbridge_ppp/matching_experiments/02_modelling/funcs_run_CV.R')
source('F:/Shaun/Strongbridge/Code/strongbridge_ppp/matching_experiments/02_modelling/funcs_make_CV_indices.R')
# library(PRROC)

# globals -----------------------------------------------------------------

data_dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/results/matching_experiments/modelling/"
data_dir <- 'F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/'
cohort_dir <- '00_matched_train_unmatched_test/'
test_strat <- '1_1000_unmatched_test/'
full_results_dir <- paste0(results_dir, test_strat, cohort_dir)
modelling_data_dir <- paste0('F:/Projects/Strongbridge/data/matching_experiments/02_modelling/', test_strat)

combined_data <- read_rds(paste0(data_dir, cohort_dir, 
                                 '01_combined_freq_datediff_topcoded_unmatched_test_original_index.rds'))

combined_data$PATIENT_ID <- as.numeric(combined_data$PATIENT_ID)

unmatched_1_1000 <- F
standard_CV <- T

#  ------------------------------------------------------------------------
#  ------------- Perform CV for 1:50 train, 1:1000 test -------------------
#  ------------------------------------------------------------------------
if (unmatched_1_1000) {

  
  # remove subset and patient IDs to define modelling data:
  combined_model <- select(combined_data, -subset, -PATIENT_ID, -test_patient_id, -lookback_date, -index_date)
  character_cols <- which(sapply(combined_model, class) == "character")
  combined_model[,character_cols] <- sapply(combined_model[,character_cols], as.numeric)
  #combined_model[,c(69,125,126,127)] <- sapply(combined_model[,c(69,125,126,127)], as.numeric)
  combined_model$label <- as.factor(combined_model$label)
  
  # #### REMOVE SPECIALITY VARIABLES:
  # combined_model <- combined_model %>% select(-starts_with("S"))
  # train_model <- train_model %>% select(-starts_with("S"))
  # ################################
  
  
  
  # read in pre-created indices
  CV_ids <- read_rds(paste0(modelling_data_dir, "1_50_matched_train_1_1000_unmatched_test_CV_ids.rds"))
  
  CV_indices <- convert_ids_to_indices(combined_data$PATIENT_ID, CV_ids$train_ids, 
                                       CV_ids$test_ids)
  #Run CV including freq and DD
  res <- run_cross_validation(combined_model, full_results_dir, 'original_indexes_freq_dd', 
                              test_indices = CV_indices$test_indices, 
                              train_indices = CV_indices$train_indices)
  #Run CV including freq ONLY
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('AVG_CLAIM_CNT|label|AGE|GENDER')),
                                   full_results_dir, 'original_indexes_freq', 
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
  #Run CV including DD ONLY
  res_dd <- run_cross_validation(dplyr::select(combined_model, matches('DT_DIFF|label|AGE|GENDER')),
                                   full_results_dir, 'original_indexes_dd', 
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
}
#  ------------------------------------------------------------------------
#  ------------- Perform CV for 1:50 train, 1:50 test -------------------
#  ------------------------------------------------------------------------
if (standard_CV){
  #TRY RUNNING STANDARD 5 FOLD CV
  combined_data <- combined_data[combined_data$subset != 'test_neg',]
#  combined_data$test_patient_id[is.na(combined_data$test_patient_id)] <- combined_data$PATIENT_ID[is.na(combined_data$test_patient_id)]
#  matched_ids <- as.factor(combined_data$test_patient_id)
  
  #Remove columns not used for modelling
  combined_model <- select(combined_data,  -PATIENT_ID, -test_patient_id, -lookback_date, -subset, -index_date)
  #Convert char columns to numeric
  character_cols <- which(sapply(combined_model, class) == "character")
  combined_model[,character_cols] <- sapply(combined_model[,character_cols], as.numeric)
  
  #Convert label to factor
  combined_model$label <- as.factor(combined_model$label)
  
  #Create output dir and set modelling input dir
  test_strat <- 'standard_5_fold_CV/'
  full_results_dir <- paste0(results_dir, test_strat, cohort_dir)
  modelling_data_dir <- paste0('F:/Projects/Strongbridge/data/matching_experiments/02_modelling/', test_strat)
  
  #Quick final QC
  pos_val_check <- palab::positive_values_check(combined_model, suffix='AVG_CLAIM_CNT')
  pos_val_check_dd <- palab::positive_values_check(combined_model, suffix='DIFF')
  palab::time_units_check(combined_model[combined_model$label == 0,], combined_model[combined_model$label==1,], prefix1='AVG_CLAIM_CNT', prefix2='AVG_CLAIM_CNT', str_function=ends_with)
  
  # read in pre-created indices
  CV_ids <- read_rds(paste0(modelling_data_dir, "1_50_matched_train_1_50_matched_test_CV_ids.rds"))
  CV_indices <- convert_ids_to_indices(combined_data$PATIENT_ID, CV_ids$train_ids, 
                                       CV_ids$test_ids)
  
  #Run CV including freq and DD
  res <- run_cross_validation(combined_model, full_results_dir, 'freq_dd', 
                              test_indices = CV_indices$test_indices, 
                              train_indices = CV_indices$train_indices)
  #Run CV including freq ONLY
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('AVG_CLAIM|label|AGE|GENDER')),
                                   full_results_dir, 'freq', 
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
  #Run CV including DD ONLY
  res_dd <- run_cross_validation(dplyr::select(combined_model, matches('DT_DIFF|label|AGE|GENDER')),
                                 full_results_dir, 'dd', 
                                 test_indices = CV_indices$test_indices, 
                                 train_indices = CV_indices$train_indices)
  
  
  #Run CV including dd/freq ONLY
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('AVG_CLAIM|DT_DIFF|label')),
                                   full_results_dir, 'freq_dd_only', 
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('DT_DIFF|label')),
                                   full_results_dir, 'dd_only', 
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('AVG_CLAIM|label')),
                                   full_results_dir, 'freq_only', 
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
}
