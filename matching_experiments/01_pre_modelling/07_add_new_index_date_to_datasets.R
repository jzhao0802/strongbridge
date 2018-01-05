rm(list=ls())
gc()
library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
source('F:/Shaun/Strongbridge/Code/strongbridge_ppp/matching_experiments/01_pre_modelling/helper_functions.R')
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

#This script extracts only features used for modelling in initial Strongbridge experiment by matching features in 12/13 month cohorts to original dataset
df_original <- df <- readRDS ('F:/Projects/Strongbridge/data/modelling/Advanced_model_data/05_combined_train_unmatched_test_capped_freq_datediff.rds') %>%
  dplyr::filter(subset!='test_neg') %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  setNames(gsub('EXP_DT$',"EXP_DT_DIFF", names(.))) %>%
  setNames(gsub('EXP_$',"EXP_DT_DIFF", names(.))) %>%
  setNames(gsub('EXP$',"EXP_DT_DIFF", names(.)))

df_original$index_date <- lubridate::ymd(df_original$index_date)


#New index dates as yearmon to allow recalc of S dates - keep positives for this
index_date <- as.yearmon(df_original$index_date)

df_original <- df_original[df_original$subset == 'train_neg',]

data_dir <-"F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/"

#Select features for 13 month + cohort
cohort_dir <- '02_gt_13_months_train/'

df_13_freqs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_common_freq_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

#Load pre-topcoded date diffs - need to re-topcode afterwards
df_13_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_date_differences.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)%>%
  setNames(gsub('EXPDT',"EXP_DT", names(.))) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

S_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_dates_unformatted.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  dplyr::select(dplyr::starts_with("S_"))%>%
  calculate_s_date_diffs(index_date) %>%
  setNames(gsub('EXPDT', 'EXP_DT', paste0(names(.),"DT_DIFF")))%>%
  dplyr::select(intersect(colnames(.), colnames(df_original))) 

df_13_freqs$index_date <- lubridate::mdy(df_13_freqs$index_date)

#Get rid of S variables
df_13_date_diffs <- df_13_date_diffs %>%
  dplyr::select(-starts_with('S_'))
df_13_date_diffs$index_date <- lubridate::mdy(df_13_date_diffs$index_date)
df_13_date_diffs[df_13_date_diffs$label == 0,][colnames(df_13_date_diffs)[grepl('EXP_DT', colnames(df_13_date_diffs))]] <- df_13_date_diffs[df_13_date_diffs$label == 0,][colnames(df_13_date_diffs)[grepl('EXP_DT', colnames(df_13_date_diffs))]]  + as.numeric(df_original$index_date - df_13_date_diffs$index_date[df_13_date_diffs$label == 0])
df_13_date_diffs$index_date[df_13_date_diffs$label == 0] <- df_original$index_date
df_13_freqs$index_date[df_13_freqs$label == 0] <- df_original$index_date
#Add S variables back in
df_13_date_diffs <- data.frame(df_13_date_diffs, S_date_diffs)

#topcode 
topcoded_dates <- dplyr::select(df_13_date_diffs, label, dplyr::contains("EXP")) %>%
  topcode_date_diffs(cap = 0.99, label = "label") %>%
  round()
df_13_date_diffs <- data.frame(df_13_date_diffs[, !grepl('EXP', colnames(df_13_date_diffs))],
                                   topcoded_dates)

#QC checks for DD vars (more lenient as after modifying index date will have some dates within 1 year)
print (!any(df_13_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(contains('D_')) < 300, na.rm=TRUE))
print (!any(df_13_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(matches('^P_')) < 300, na.rm=TRUE))
#G variables have some dd with 365.
print (!any(df_13_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(matches('^G_')) < 300, na.rm=TRUE))
#NOTE - S dates are in units of months!!
print (!any(df_13_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(contains('S_')) < 12, na.rm=TRUE))

pos_val_check_dd <- palab::positive_values_check(df_13_date_diffs, suffix='DIFF')

#Combined back with freqs
df_13 <- data.frame(df_13_freqs, df_13_date_diffs)

saveRDS(df_13, paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded_new_indexes.rds'))

#Select features for <= 12 month 
cohort_dir <- '01_lte_12_months_train/'

df_12_freqs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_common_freq_topcoded.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

df_12_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_date_differences.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID)%>%
  setNames(gsub('EXPDT',"EXP_DT", names(.))) %>%
  dplyr::select(intersect(colnames(.), colnames(df_original)))

S_date_diffs <- readRDS(paste0(data_dir, cohort_dir, '01_combined_dates_unformatted.rds')) %>%
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
  dplyr::arrange(PATIENT_ID) %>%
  dplyr::select(dplyr::starts_with("S_"))%>%
  calculate_s_date_diffs(index_date) %>%
  setNames(gsub('EXPDT', 'EXP_DT', paste0(names(.),"DT_DIFF")))%>%
  dplyr::select(intersect(colnames(.), colnames(df_original))) 

df_12_freqs$index_date <- lubridate::mdy(df_12_freqs$index_date)

#Get rid of S variables
df_12_date_diffs <- df_12_date_diffs %>%
  dplyr::select(-starts_with('S_'))
df_12_date_diffs$index_date <- lubridate::mdy(df_12_date_diffs$index_date)
df_12_date_diffs[df_12_date_diffs$label == 0,][colnames(df_12_date_diffs)[grepl('EXP_DT', colnames(df_12_date_diffs))]] <- df_12_date_diffs[df_12_date_diffs$label == 0,][colnames(df_12_date_diffs)[grepl('EXP_DT', colnames(df_12_date_diffs))]]  + as.numeric(df_original$index_date - df_12_date_diffs$index_date[df_12_date_diffs$label == 0])
df_12_date_diffs$index_date[df_12_date_diffs$label == 0] <- df_original$index_date
df_12_freqs$index_date[df_12_freqs$label == 0] <- df_original$index_date
#Add S variables back in
df_12_date_diffs <- data.frame(df_12_date_diffs, S_date_diffs)

#topcode 
topcoded_dates <- dplyr::select(df_12_date_diffs, label, dplyr::contains("EXP")) %>%
  topcode_date_diffs(cap = 0.99, label = "label") %>%
  round()
df_12_date_diffs <- data.frame(df_12_date_diffs[, !grepl('EXP', colnames(df_12_date_diffs))],
                               topcoded_dates)

#QC checks for DD vars (more lenient as after modifying index date will have some dates within 1 year)
print (all(df_12_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(contains('D_')) <= 365, na.rm=TRUE))
print (all(df_12_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(matches('^P_')) <= 365, na.rm=TRUE))
#G variables have some dd with 365.
print (all(df_12_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(matches('^G_')) <= 365, na.rm=TRUE))
#NOTE - S dates are in units of months!!
print (all(df_12_date_diffs %>% dplyr::select(contains('DIFF')) %>% dplyr::select(contains('S_')) <= 12, na.rm=TRUE))


pos_val_check_dd <- palab::positive_values_check(df_12_date_diffs, suffix='DIFF')

df_12 <- data.frame(df_12_freqs, df_12_date_diffs)

saveRDS(df_12, paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded_new_indexes.rds'))


#Create combined feature set
#cohort_dir <- '01_lte_12_months_train/'
#df_12 <- readRDS(paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds')) %>%
#  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
#  dplyr::arrange(PATIENT_ID)###

#cohort_dir <- '02_gt_13_months_train/'
#df_13 <- readRDS(paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded.rds')) %>%
#  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>%
#  dplyr::arrange(PATIENT_ID)

df_12_suffixed <- df_12 %>%
  dplyr::select(matches('AVG|DIFF')) %>%
  setNames(paste0(names(.), '_FIXED_12_MONTHS'))
df_13_suffixed <- df_13 %>%
  dplyr::select(matches('AVG|DIFF')) %>%
  setNames(paste0(names(.), '_13_PLUS_MONTHS'))

df_12_13 <- data.frame(df_12_suffixed, df_13_suffixed, dplyr::select(df_12,- one_of(colnames(df_12[grep('AVG|DIFF', colnames(df_12))]))))
cohort_dir <- '03_lt_12_gt_13_months_train/'
dir.create(paste0(data_dir, cohort_dir), recursive = TRUE, showWarnings = FALSE)
saveRDS(df_12_13, paste0(data_dir, cohort_dir, '02_combined_freq_datediff_topcoded_new_indexes.rds'))
