rm(list=ls())
gc()

library('palab')

#This script performs QC tests on final datasets


#Check cohort 13 month+
data_dir <-"F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/"
cohort_dir <- '02_gt_13_months_train/'

#cohort_dir <- '01_lte_12_months_train/'

df_freqs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_common_freq_topcoded.rds'))
df_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_date_differences_topcoded.rds'))
df_dates <- readRDS(paste0(data_dir, cohort_dir, '01_combined_dates_unformatted.rds'))

#ENSURE NO DATE DIFFS <= 365
print (!any(df_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(contains('D_')) < 365, na.rm=TRUE))
print (!any(df_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(matches('^P_')) < 365, na.rm=TRUE))
#G variables have some dd with 365..
print (!any(df_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(matches('^G_')) < 365, na.rm=TRUE))
#NOTE - S dates are in units of months!!
print (!any(df_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(contains('S_')) < 12, na.rm=TRUE))


pos_val_check <- positive_values_check(df_freqs, suffix='AVG_CLAIM_CNT')
pos_val_check_dd <- positive_values_check(df_date_diffs, suffix='DIFF')
time_units_check(df_freqs[df_freqs$label==0,], df_freqs[df_freqs$label==1,], prefix1='AVG_CLAIM_CNT', prefix2='AVG_CLAIM_CNT', str_function=ends_with)
missing_values_check(df_freqs, all=TRUE)
index_date_check(df_dates, index_date_field='index_date', suffix_expdt='FIRST_EXP_DT')
index_date_check(df_dates, index_date_field='index_date', suffix_expdt='LAST_EXP_DT')

#CHECK <= 12 MONTH COHORT
cohort_dir <- '01_lte_12_months_train/'

df_freqs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_common_freq_topcoded.rds'))
df_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_date_differences_topcoded.rds'))

#ENSURE NO DATE DIFFS > 365
print (all(df_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(contains('D_')) <= 365, na.rm=TRUE))
print (all(df_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(matches('^P_')) <= 365, na.rm=TRUE))
print (all(df_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(matches('^G_')) <= 365, na.rm=TRUE))
#NOTE - S dates are in units of months!!
print (all(df_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(contains('S_')) <= 12, na.rm=TRUE))


df_dates <- readRDS(paste0(data_dir, cohort_dir, '01_combined_dates_unformatted.rds'))

pos_val_check <- positive_values_check(df_freqs, suffix='AVG_CLAIM_CNT')
pos_val_check_dd <- positive_values_check(df_date_diffs, suffix='DIFF')
time_units_check(df_freqs[df_freqs$label==0,], df_freqs[df_freqs$label==1,], prefix1='AVG_CLAIM_CNT', prefix2='AVG_CLAIM_CNT', str_function=ends_with)
missing_values_check(df_freqs, all=TRUE)
index_date_check(df_dates, index_date_field='index_date', suffix_expdt='FIRST_EXP_DT')


#CHECK MATCHED COHORT
cohort_dir <- '00_matched_train_unmatched_test/'

df <- readRDS(paste0(data_dir, cohort_dir, '01_combined_freq_datediff_topcoded.rds'))

pos_val_check <- positive_values_check(df, suffix='AVG_CLAIM_CNT')
pos_val_check_dd <- positive_values_check(df, suffix='DIFF')
df[colnames(df)[grep('AVG_CLAIM_CNT', colnames(df))]] <- sapply(df[colnames(df)[grep('AVG_CLAIM_CNT', colnames(df))]], as.numeric)
time_units_check(df[df$label==0,], df[df$label==1,], prefix1='AVG_CLAIM_CNT', prefix2='AVG_CLAIM_CNT', str_function=ends_with)
#missing_values_check(df, all=TRUE)

