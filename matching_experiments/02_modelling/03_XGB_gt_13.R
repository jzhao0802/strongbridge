
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

data_dir <- "F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/"
model_data_dir <- "F:/Projects/Strongbridge/data/matching_experiments/02_modelling/"
results_dir <- "F:/Projects/Strongbridge/results/matching_experiments/modelling/"
cohort_dir <- '02_gt_13_months_train/'
#cohort_dir <- '01_matched_train_unmatched_test/'
test_strat <- '1_1000_unmatched_test/'
full_results_dir <- paste0(results_dir, test_strat, cohort_dir)
modelling_data_dir <- paste0('F:/Projects/Strongbridge/data/matching_experiments/02_modelling/', test_strat)

# Data in -----------------------------------------------------------------
new_indexes <- T

if (new_indexes) {
  suffix <- '_new_indexes'  
} else {
  suffix <- ''
}
combined_data <- readRDS(paste0(data_dir, cohort_dir, "02_combined_freq_datediff_topcoded", suffix, ".rds")) %>%
  dplyr::select(-contains('.1'))

combined_data$index_date <- lubridate::mdy(combined_data$index_date)

combined_data$PATIENT_ID <- as.numeric(combined_data$PATIENT_ID)

unmatched_1_1000 <- F
standard_CV <- T

#  ------------------------------------------------------------------------
#  ------------- Perform CV for 1:50 train, 1:1000 test -------------------
#  ------------------------------------------------------------------------
if (unmatched_1_1000) {
  testing_data <- readRDS(paste0(modelling_data_dir, "1_1000_unmatched_test.rds")) %>%
    dplyr::filter(subset == 'test_neg')
  #  ------------------------------------------------------------------------
  # PREPROCESS
  #  ------------------------------------------------------------------------
  
  
  # change label to a factor:
  combined_data$label <- as.factor(combined_data$label)
  character_cols <- which(sapply(combined_data, class) == "character")
  combined_data[,character_cols] <- sapply(combined_data[,character_cols], as.numeric)
  
  #Convert char cols to numeric (not an issue to do this now as will remove all 
  #char cols coerced to NA)
  testing_data$PATIENT_ID <- as.numeric(testing_data$PATIENT_ID)
  character_cols <- which(sapply(testing_data, class) == "character")
  testing_data[,character_cols] <- sapply(testing_data[,character_cols], as.numeric)
  testing_data$label <- as.factor(testing_data$label)
  
  
  #Merge test and training data
  combined_data <- bind_rows(combined_data, testing_data)
  #Set NA freqs to 0
  combined_data[colnames(combined_data)[grepl('AVG_CLAIM_CNT', colnames(combined_data))]][is.na(combined_data[colnames(combined_data)[grepl('AVG_CLAIM_CNT', colnames(combined_data))]])] <- 0
  
  #  ------------------------------------------------------------------------
  # remove subset and patient IDs to define modelling data:
  #matched_ids <- as.factor(combined_data$test_patient_id)
  combined_model <- select(combined_data,  -PATIENT_ID, -test_patient_id, -lookback_date, -subset, -index_date)
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
  res <- run_cross_validation(combined_model, full_results_dir, 'freq_dd', 
                              test_indices = CV_indices$test_indices, 
                              train_indices = CV_indices$train_indices)
  #Run CV including freq ONLY
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('AVG_CLAIM_CNT|label|AGE|GENDER')),
                                   full_results_dir, 'freq', 
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
  #Run CV including DD ONLY
  res_dd <- run_cross_validation(dplyr::select(combined_model, matches('DT_DIFF|label|AGE|GENDER')),
                                 full_results_dir, 'dd', 
                                 test_indices = CV_indices$test_indices, 
                                 train_indices = CV_indices$train_indices)
  
  #Run CV including LAST DD ONLY
  res_dd <- run_cross_validation(dplyr::select(combined_model, matches('LAST|label|AGE|GENDER')),
                                 full_results_dir, 'last_exp_dt_dd', 
                                 test_indices = CV_indices$test_indices, 
                                 train_indices = CV_indices$train_indices)
  #Run CV including FIRST DD ONLY
  res_dd <- run_cross_validation(dplyr::select(combined_model, matches('FIRST|label|AGE|GENDER')),
                                 full_results_dir, 'last_exp_dt_dd', 
                                 test_indices = CV_indices$test_indices, 
                                 train_indices = CV_indices$train_indices)
  
  
}

#  ------------------------------------------------------------------------
#  ------------- Perform CV for 1:50 train, 1:50 test -------------------
#  ------------------------------------------------------------------------

if (standard_CV) {
  #TRY RUNNING STANDARD 5 FOLD CV
  if (unmatched_1_1000) {
    combined_data$subset[is.na(combined_data$subset)] <- 'train'
    combined_data <- combined_data[combined_data$subset != 'test_neg',]
    combined_model <- select(combined_data,  -PATIENT_ID, -test_patient_id, -lookback_date, -subset, -index_date)
  } else {
    combined_data$label <- as.factor(combined_data$label)
    combined_model <- select(combined_data,  -PATIENT_ID, -test_patient_id, -lookback_date,  -index_date)
    character_cols <- which(sapply(combined_model, class) == "character")
    combined_model[,character_cols] <- sapply(combined_model[,character_cols], as.numeric)
  }
  
  combined_model$label <- as.factor(combined_model$label)
  
  test_strat <- 'standard_5_fold_CV/'
  full_results_dir <- paste0(results_dir, test_strat, cohort_dir)
  modelling_data_dir <- paste0('F:/Projects/Strongbridge/data/matching_experiments/02_modelling/', test_strat)
  
  # read in pre-created ids
  CV_ids <- read_rds(paste0(modelling_data_dir, "1_50_matched_train_1_50_matched_test_CV_ids.rds"))
  CV_indices <- convert_ids_to_indices(combined_data$PATIENT_ID, CV_ids$train_ids, 
                                       CV_ids$test_ids)
  
  #Quick final QC
  pos_val_check <- palab::positive_values_check(combined_model, suffix='AVG_CLAIM_CNT')
  pos_val_check_dd <- palab::positive_values_check(combined_model, suffix='DIFF')
  palab::time_units_check(combined_model[combined_model$label==0,], combined_model[combined_model$label==1,], prefix1='AVG_CLAIM_CNT', prefix2='AVG_CLAIM_CNT', str_function=ends_with)
  
  
  #Run CV including freq and DD
  res <- run_cross_validation(combined_model, full_results_dir, paste0('freq_dd', suffix),
                              test_indices = CV_indices$test_indices, 
                              train_indices = CV_indices$train_indices)
  #Run CV including freq ONLY
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('AVG_CLAIM|label|AGE|GENDER')),
                                   full_results_dir, paste0('freq', suffix),
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
  #Run CV including DD ONLY
  res_dd <- run_cross_validation(dplyr::select(combined_model, matches('DT_DIFF|label|AGE|GENDER')),
                                 full_results_dir, paste0('dd', suffix),
                                 test_indices = CV_indices$test_indices, 
                                 train_indices = CV_indices$train_indices)
  #Run CV including dd/freq ONLY
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('AVG_CLAIM|DT_DIFF|label')),
                                   full_results_dir, paste0('freq_dd_only', suffix),
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('DT_DIFF|label')),
                                   full_results_dir, paste0('dd_only', suffix),
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
  
  res_freq <- run_cross_validation(dplyr::select(combined_model, matches('AVG_CLAIM|label')),
                                   full_results_dir, paste0('freq_only', suffix),
                                   test_indices = CV_indices$test_indices, 
                                   train_indices = CV_indices$train_indices)
}
