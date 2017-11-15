
#  ------------------------------------------------------------------------
# Cross validate model on unmatched test set
#  ------------------------------------------------------------------------

library(mlr)
library(tidyverse)
library(palabmod)
source('F:/Shaun/Strongbridge/Code/strongbridge_ppp/matching_experiments/02_modelling/funcs_run_CV.R')
source('F:/Shaun/Strongbridge/Code/strongbridge_ppp/matching_experiments/02_modelling/funcs_make_CV_indices.R')



adv_data_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/results/matching_experiments/modelling/"
cohort_dir <- '00_matched_train_unmatched_test/'
test_strat <- '1_1000_unmatched_test/'
full_results_dir <- paste0(results_dir, test_strat, cohort_dir)
full_data_dir <- paste0('F:/Projects/Strongbridge/data/matching_experiments/02_modelling/', test_strat)

# data in -----------------------------------------------------------------

combined <- read_rds(paste0(adv_data_dir,
                            "05_combined_train_unmatched_test_capped_freq_datediff.rds"))

#LOAD IDs GENERATED IN 00_create_unmatched_CV_ids
CV_ids <- read_rds(paste0(full_data_dir, 
                          "1_50_matched_train_1_1000_unmatched_test_CV_ids.rds"))

#Convert IDs to indices using function from funcs_CV 
CV_indices <- convert_ids_to_indices(combined$PATIENT_ID, CV_ids$train_ids, 
                                     CV_ids$test_ids)


# read in resampling indices (created in preliminary modelling scripts):
train_indices <- read_rds(paste0(data_dir, "train_indices.rds"))
test_indices <- read_rds(paste0(data_dir, "test_indices.rds"))

#Double check indices from conversion in 00_create_unmatched_CV_ids match
#with what was previously used
for (i in 1:5) {
  print(all(train_indices[[i]] == CV_indices$train_indices[[i]]))
  print(all(test_indices[[i]] == CV_indices$test_indices[[i]]))
}


# preprocess --------------------------------------------------------------

combined_model <- combined %>% select(-PATIENT_ID, -index_date, -lookback_date,
                                      -test_patient_id, -subset)

char_cols <- grep("character", sapply(combined_model, class))

combined_model[,char_cols] <- sapply(combined_model[,char_cols], as.numeric)

combined_model$label <- as.factor(combined_model$label)


#RUN CV including DD AND FREQ
res <- run_cross_validation(combined_model, full_results_dir, 'Lachlan_freq_dd', 
                            test_indices = CV_indices$test_indices, 
                            train_indices = CV_indices$train_indices)

#Run CV including freq ONLY
res_freq <- run_cross_validation(dplyr::select(combined_model, matches('CNT|label|AGE|GENDER')),
                                 full_results_dir, 'Lachlan_freq', 
                                 test_indices = CV_indices$test_indices, 
                                 train_indices = CV_indices$train_indices)

#Run CV including DD ONLY
res_dd <- run_cross_validation(dplyr::select(combined_model, matches('EXP|label|AGE|GENDER')),
                               full_results_dir, 'Lachlan_dd', 
                               test_indices = CV_indices$test_indices, 
                               train_indices = CV_indices$train_indices)
