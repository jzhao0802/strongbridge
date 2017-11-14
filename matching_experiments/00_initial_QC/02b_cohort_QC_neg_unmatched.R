library('tidyverse')
library('lubridate')
base_dir <- 'F:/Projects/Strongbridge/data/raw_data_cohorts/'

#Load df of <= 12 months lookback
df_12_dates <- readRDS(paste(base_dir, '03_Cohorts_by_variable_type_12_months', 'Neg_dates_MOD_12.rds', sep='/'))
df_12_counts <- readRDS(paste(base_dir, '03_Cohorts_by_variable_type_12_months', 'Neg_claims_MOD_12.rds', sep='/'))
df_12_freqs <- readRDS(paste(base_dir, '03_Cohorts_by_variable_type_12_months', 'Neg_common_frequencies_MOD_12.rds', sep='/'))

#Load df of <= 13 months lookback

df_13_dates <- readRDS(paste(base_dir, '05_Cohorts_by_variable_type_13_months', 'Neg_dates_MOD_13.rds', sep='/'))
df_13_counts <- readRDS(paste(base_dir, '05_Cohorts_by_variable_type_13_months', 'Neg_claims_MOD_13.rds', sep='/'))
df_13_freqs<- readRDS(paste(base_dir, '05_Cohorts_by_variable_type_13_months', 'Neg_common_frequencies_MOD_13.rds', sep='/'))

#Load df with unmatched negatives with full lookback
df_all <- readRDS ('F:/Projects/Strongbridge/data/modelling/05_unmatched_neg_capped_freq_datediff.rds')
df_original <- as.data.frame(readr::read_csv('F:/Projects/Strongbridge/data/random_sample_scoring/Scoring_Final_Sample_C000_UP.csv'))
df_original <- as.data.frame(df_original)
#Need to find overlap...
#df_all <- df_all[df_all$subset == 'test_neg',]
overlap <- intersect(df_all$PATIENT_ID, df_12_dates$PATIENT_ID)

df_all <- df_all[df_all$PATIENT_ID %in% overlap,] %>% dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% dplyr::arrange(PATIENT_ID)
df_original <- df_original[df_original$PATIENT_ID %in% overlap,] %>% dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% dplyr::arrange(PATIENT_ID)

df_13_dates <- df_13_dates[df_13_dates$PATIENT_ID %in% overlap,] %>% dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% dplyr::arrange(PATIENT_ID)
df_13_freqs <- df_13_freqs[df_13_freqs$PATIENT_ID %in% overlap,] %>% dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% dplyr::arrange(PATIENT_ID)
df_13_counts <- df_13_counts[df_13_counts$PATIENT_ID %in% overlap,] %>% dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% dplyr::arrange(PATIENT_ID)

df_12_dates <- df_12_dates[df_12_dates$PATIENT_ID %in% overlap,] %>% dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% dplyr::arrange(PATIENT_ID)
df_12_freqs <- df_12_freqs[df_12_freqs$PATIENT_ID %in% overlap,] %>% dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% dplyr::arrange(PATIENT_ID)
df_12_counts <- df_12_counts[df_12_counts$PATIENT_ID %in% overlap,] %>% dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% dplyr::arrange(PATIENT_ID)

#DO CHECKS OVER ALL VALID VARIABLES
all_codes <- sub('_AVG_CLAIM_CNT', '', colnames(df_all))
exclude <- c("test_patient_id","index_date","lookback_date", "AGE","GENDER", 'label', 'PATIENT_ID')
all_codes <- all_codes[!(all_codes%in% exclude)]
for (code in all_codes){
  #CONSTRUCT NAMES FOR EXP_DT, FREQ AND COUNT COLUMNS
  last <- paste0(code, '_LAST_EXP_DT')
  first <- paste0(code, '_FIRST_EXP_DT')
  freq <- paste0(code, '_AVG_CLAIM_CNT')
  count <- paste0(code, '_CLAIM_CNT')
  
  #SKIP IF ONE OF THESE VARIABLES DOES NOT EXIST FOR A COHORT
  if (!((freq %in% colnames(df_12_freqs) & freq%in%colnames(df_13_freqs) & freq%in%colnames(df_all)) &
  (first %in% colnames(df_12_dates) & first%in%colnames(df_13_dates) & first%in%colnames(df_all)) &
  (last %in% colnames(df_12_dates) & last%in%colnames(df_13_dates) & last%in%colnames(df_all)) &
  (count %in% colnames(df_12_counts) & count%in%colnames(df_13_counts) & count%in%colnames(df_original)))) next
  
  #CHECK WHICH TEST TO RUN FOR VARIABLE
  #VARIABLE HAS FIRST AND LAST EXP > 365 DAYS FROM INDEX?
  both_gt_12 <- df_all[last] > 365 & df_all[first] > 365
  #VARIABLE HAS FIRST EXP > 365, BUT LAST EXP <= 365 DAYS FROM INDEX?
  first_gt_12 <- df_all[last] <= 365 & df_all[first] > 365
  #VARIABLE HAS FIRST AND LAST EXP <= 365 DAYS FROM INDEX?
  first_lt_12 <- df_all[last] <= 365 & df_all[first] <= 365
  
  #ASSIGN NAs TO F
  both_gt_12[is.na(both_gt_12)] <- F
  first_gt_12[is.na(first_gt_12)] <- F
  first_lt_12[is.na(first_lt_12)] <- F
  
  #RUN TESTS
  
# CHECK 1 -- ---------------------------------------------------
  if (any(both_gt_12)){
    #SELECT PATIENTS THAT FALL INTO GROUPS FROM EACH DF
    patients_both <- df_all$PATIENT_ID[both_gt_12]
    df_all_sel_patient <- df_all[df_all$PATIENT_ID %in% patients_both,]
    df_original_sel_patient <- df_original[df_original$PATIENT_ID %in% patients_both,]
    
    df_13_dates_sel_patient <- df_13_dates[df_13_dates$PATIENT_ID %in% patients_both,]
    df_13_freqs_sel_patient <- df_13_freqs[df_13_freqs$PATIENT_ID %in% patients_both,]
    df_13_counts_sel_patient <- df_13_counts[df_13_counts$PATIENT_ID %in% patients_both,]
    
    df_12_dates_sel_patient <- df_12_dates[df_12_dates$PATIENT_ID %in% patients_both,]
    df_12_freqs_sel_patient <- df_12_freqs[df_12_freqs$PATIENT_ID %in% patients_both,]
    df_12_counts_sel_patient <- df_12_counts[df_12_counts$PATIENT_ID %in% patients_both,]
    #Expect frequencies to be larger for df_13, and 0 for df_12
    freq_passed <- all(df_all_sel_patient[freq] < df_13_freqs_sel_patient[freq] & df_12_freqs_sel_patient[freq] == 0)
    #Expect dates to be same for df_13 and NA for df_12
    dates_passed <- all(df_original_sel_patient[first] == df_13_dates_sel_patient[first] & df_original_sel_patient[last] == df_13_dates_sel_patient[last] & is.na(df_12_dates_sel_patient[last]) & is.na(df_12_dates_sel_patient[last]))
    #Expect 0 count for df_12 and same counts for df_13
    counts_passed <- all((df_original_sel_patient[count] == df_13_counts_sel_patient[count]) & df_12_counts_sel_patient[count] == 0)
    print (paste('CHECK 1 PASSED?', (freq_passed & dates_passed & counts_passed)))
    if (!(freq_passed & dates_passed & counts_passed)) break
  } 
  
# CHECK 2 -- ---------------------------------------------------
  if (any(first_gt_12)){
    #SELECT PATIENTS THAT FALL INTO GROUPS FROM EACH DF
    patients_first <- df_all$PATIENT_ID[first_gt_12]
    df_all_sel_patient <- df_all[df_all$PATIENT_ID %in% patients_first,]
    df_original_sel_patient <- df_original[df_original$PATIENT_ID %in% patients_first,]
    
    df_13_dates_sel_patient <- df_13_dates[df_13_dates$PATIENT_ID %in% patients_first,]
    df_13_freqs_sel_patient <- df_13_freqs[df_13_freqs$PATIENT_ID %in% patients_first,]
    df_13_counts_sel_patient <- df_13_counts[df_13_counts$PATIENT_ID %in% patients_first,]
    
    df_12_dates_sel_patient <- df_12_dates[df_12_dates$PATIENT_ID %in% patients_first,]
    df_12_freqs_sel_patient <- df_12_freqs[df_12_freqs$PATIENT_ID %in% patients_first,]
    df_12_counts_sel_patient <- df_12_counts[df_12_counts$PATIENT_ID %in% patients_first,]
    #WARNING - FOUND CASES WHERE SUM OF COUNTS FOR PATIENTS IN COOHORTS >= 13 AND <= 12 IS OFF BY 1 - DUE TO NOT INCLUDING FIRST_ACTIVITY_LRX_DX_DATE??
    #Expect sum of counts to be same
    count_passed <- all(df_original_sel_patient[count] > df_13_counts_sel_patient[count] & df_original_sel_patient[count] > df_12_counts_sel_patient[count] & ((df_13_counts_sel_patient[count] + df_12_counts_sel_patient[count]) == df_original_sel_patient[count]) || ((df_13_counts_sel_patient[count] + df_12_counts_sel_patient[count] + 1) == df_original_sel_patient[count])) 
    #Expect first date same for df_13, last date same for df_12
    dates_passed <- all(df_original_sel_patient[first] == df_13_dates_sel_patient[first] & df_original_sel_patient[last] == df_12_dates_sel_patient[last] )
    
    print (paste('CHECK 2 PASSED?', (dates_passed & count_passed)))
    if (!(dates_passed & count_passed)) break
  }  
 
# CHECK 3 -- ---------------------------------------------------
  if (any(first_lt_12)){
    #SELECT PATIENTS THAT FALL INTO GROUPS FROM EACH DF
    patients_first <- df_all$PATIENT_ID[first_lt_12]
    df_all_sel_patient <- df_all[df_all$PATIENT_ID %in% patients_first,]
    df_original_sel_patient <- df_original[df_original$PATIENT_ID %in% patients_first,]
    
    df_13_dates_sel_patient <- df_13_dates[df_13_dates$PATIENT_ID %in% patients_first,]
    df_13_freqs_sel_patient <- df_13_freqs[df_13_freqs$PATIENT_ID %in% patients_first,]
    df_13_counts_sel_patient <- df_13_counts[df_13_counts$PATIENT_ID %in% patients_first,]
    
    df_12_dates_sel_patient <- df_12_dates[df_12_dates$PATIENT_ID %in% patients_first,]
    df_12_freqs_sel_patient <- df_12_freqs[df_12_freqs$PATIENT_ID %in% patients_first,]
    df_12_counts_sel_patient <- df_12_counts[df_12_counts$PATIENT_ID %in% patients_first,]
    #Expect larger frequency for df_12, 0 for df_13
    freq_passed <- all(df_all_sel_patient[freq] < df_12_freqs_sel_patient[freq] & df_13_freqs_sel_patient[freq] == 0)
    #expect first and last same for df_12, NA for df_13
    dates_passed <- all(df_original_sel_patient[first] == df_12_dates_sel_patient[first] & df_original_sel_patient[last] == df_12_dates_sel_patient[last] & is.na(df_13_dates_sel_patient[last]) & is.na(df_13_dates_sel_patient[first]))
    #Expect same counts for df_12, 0 for df_13
    counts_passed <- all((df_original_sel_patient[count] == df_12_counts_sel_patient[count]) & df_13_counts_sel_patient[count] == 0)
    print (paste('CHECK 3 PASSED?', (freq_passed & dates_passed & counts_passed)))
    
    if (!(dates_passed & count_passed & counts_passed)) break
  }  
   
}
