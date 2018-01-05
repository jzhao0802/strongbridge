rm(list=ls())
gc()
library(tidyverse)

#This script extracts only features used for modelling in initial Strongbridge experiment by matching features in 12/13 month cohorts to original dataset
df_original <- readRDS ('F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/00_matched_train_unmatched_test/01_combined_freq_datediff_topcoded.rds') %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)

#Select features for 13 month + cohort
data_dir <-"F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/"
cohort_dir <- '02_gt_13_months_train/'

df_13_freqs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_common_freq_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_13_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_date_differences_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)%>%
  setNames(gsub('EXPDT',"EXP_DT", names(.))) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_13_freqs_datediffs <- data.frame(df_13_freqs, df_13_date_diffs)

saveRDS(df_13_freqs_datediffs, paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds'))

#Select features for <= 12 month 
cohort_dir <- '01_lte_12_months_train/'

df_12_freqs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_common_freq_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_12_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_date_differences_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)%>%
  setNames(gsub('EXPDT',"EXP_DT", names(.))) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_12_freqs_datediffs <- data.frame(df_12_freqs, df_12_date_diffs)

saveRDS(df_12_freqs_datediffs, paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds'))


#Create combined feature set
cohort_dir <- '01_lte_12_months_train/'
df_12 <- readRDS(paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)

cohort_dir <- '02_gt_13_months_train/'
df_13 <- readRDS(paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)

df_12_suffixed <- df_12 %>%
  dplyr::select(matches('AVG|DIFF')) %>%
  setNames(paste0(names(.), '_FIXED_12_MONTHS'))
df_13_suffixed <- df_13 %>%
  dplyr::select(matches('AVG|DIFF')) %>%
  setNames(paste0(names(.), '_13_PLUS_MONTHS'))

df_12_13 <- data.frame(df_12_suffixed, df_13_suffixed, dplyr::select(df_12,- one_of(colnames(df_12[grep('AVG|DIFF', colnames(df_12))]))))
cohort_dir <- '03_lt_12_gt_13_months_train/'
dir.create(paste0(data_dir, cohort_dir), recursive = TRUE, showWarnings = FALSE)
saveRDS(df_12_13, paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds'))
