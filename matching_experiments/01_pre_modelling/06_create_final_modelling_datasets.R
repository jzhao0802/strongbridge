library(tidyverse)

#This script extracts only features used for modelling in initial Strongbridge experiment by matching features in 12/13 month cohorts to original dataset
df_original <- readRDS ('F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/00_matched_train_unmatched_test/01_combined_freq_datediff_topcoded.rds') %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)

#Select features for 13 month + cohort
data_dir <-"F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/"
cohort_dir <- '02_gt_13_months_train/'

df_freqs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_common_freq_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_date_differences_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)%>%
  setNames(gsub('EXPDT',"EXP_DT", names(.))) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_freqs_datediffs <- data.frame(df_freqs, df_date_diffs)

saveRDS(df_freqs_datediffs, paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds'))

#Select features for <= 12 month 
cohort_dir <- '01_lte_12_months_train/'

df_freqs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_common_freq_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_date_differences_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)%>%
  setNames(gsub('EXPDT',"EXP_DT", names(.))) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_freqs_datediffs <- data.frame(df_freqs, df_date_diffs)

saveRDS(df_freqs_datediffs, paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds'))

