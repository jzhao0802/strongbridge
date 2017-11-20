library('tidyverse')
library('dplyr')
rm(list=ls())
gc()
base_dir <- 'F:/Projects/Strongbridge/data/raw_data_cohorts/'
output_dir <- 'F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/'

#LOAD IN DATASET WITH UNMATCHED NEGATIVES FOR TESTING
df <- readRDS ('F:/Projects/Strongbridge/data/modelling/Advanced_model_data/05_combined_train_unmatched_test_capped_freq_datediff.rds')
#LOAD IN DATASET WITH MATCHED NEGATIVES FOR TESTING
#df <- readRDS ('F:/Projects/Strongbridge/data/modelling/Advanced_model_data/04_combined_train_matched_test_capped_freq_datediff.rds')

df <- df %>%
  setNames(gsub('EXP_DT$',"EXP_DT_DIFF", names(.))) %>%
  setNames(gsub('EXP_$',"EXP_DT_DIFF", names(.))) %>%
  setNames(gsub('EXP$',"EXP_DT_DIFF", names(.)))


#Select only training data as this is all we have in the other cohorts... - ISSUE IS THAT CANNOT FIX INDEX DATES FOR TEST COHORT....
df_test_negs <- df[df$subset == 'test_neg',] %>%
  dplyr::mutate(PATIENT_ID = as.numeric(PATIENT_ID))
df_test_negs$index_date <- lubridate::mdy(df_test_negs$index_date)


df <- df[df$subset == 'train_neg' | df$subset == 'pos',]
df$index_date <- lubridate::ymd(df$index_date)
#df$subset <- NULL

df <- df %>%
  dplyr::mutate(PATIENT_ID = as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)

df_neg <- df[df$label==0,]
#For original study, index dates were moved to be date of most recent last_expd (or kept the same if this was > 1 month away from original index date)
#Need to reset to original index dates and re-calculate date differences
#Do this ONLY FOR NEGATIVES!
df_12_dates <- readRDS(paste(base_dir, '03_Cohorts_by_variable_type_12_months', 'Neg_dates_MOD_12.rds', sep='/')) %>% 
  dplyr::mutate(PATIENT_ID = as.numeric(PATIENT_ID)) %>% 
  dplyr::arrange(PATIENT_ID)

df_12_dates$index_date <- lubridate::mdy(df_12_dates$index_date)
df_neg[colnames(df_neg)[grepl('EXP_DT', colnames(df_neg))]] <- df_neg[colnames(df_neg)[grepl('EXP_DT', colnames(df_neg))]] + as.numeric(df_12_dates$index_date - df_neg$index_date)
df_neg$index_date <- df_12_dates$index_date

pos_val_check_dd <- palab::positive_values_check(df_neg, suffix='DIFF')
#Recombined with testing negatives and positives
df <- dplyr::bind_rows(df[df$label==1,], df_neg, df_test_negs)

#Save output dataset

#test_negs <- df[df$subset == 'test_neg',]
#test_negs$subset <- NULL

#df <- df[df$subset == 'train_neg' | df$subset == 'pos',]
#df$subset <- NULL


saveRDS(df, paste0(output_dir, '00_matched_train_unmatched_test/01_combined_freq_datediff_topcoded_unmatched_test_original_index.rds'))

#Create testing ONLY dataset
#df <- readr::read_rds(paste0(output_dir, '00_matched_train_unmatched_test/01_combined_freq_datediff_topcoded_unmatched_test_original_index.rds'))
results_dir <- "F:/Projects/Strongbridge/results/matching_experiments/modelling/"
data_dir <- 'F:/Projects/Strongbridge/data/matching_experiments/02_modelling/'
cohort_dir <- '00_matched_train_unmatched_test/'
test_strat <- '1_1000_unmatched_test/'
full_data_dir <- paste0(data_dir, test_strat)
readr::write_rds(df[df$subset %in% c('pos', 'test_neg'),], 
          paste0(full_data_dir, "1_1000_unmatched_test.rds"))

#saveRDS(df, paste0(output_dir, '00_matched_train_unmatched_test/01_combined_freq_datediff_topcoded_unmatched_test_original_index.rds'))



#USE THIS FILENAME FOR UNMATCHED TESTING NEGATIVES
#saveRDS(test_negs, paste0(output_dir, '00_matched_train_unmatched_test/01_combined_freq_datediff_topcoded_unmatched_testing_negs_only.rds'))
#USE THIS FILENAME FOR MATCHED TESTING NEGATIVES
#saveRDS(test_negs, paste0(output_dir, '00_matched_train_unmatched_test/01_combined_freq_datediff_topcoded_matched_testing_negs_only.rds'))
