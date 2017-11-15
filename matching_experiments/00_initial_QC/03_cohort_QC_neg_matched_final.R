library('tidyverse')
library('lubridate')
library('dplyr')
base_dir <- 'F:/Projects/Strongbridge/data/raw_data_cohorts/'
base_dir_2 <- 'F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/'

#Load df of <= 12 months lookback
df_12_dates <- readRDS(paste(base_dir, '03_Cohorts_by_variable_type_12_months', 'Neg_dates_MOD_12.rds', sep='/')) %>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)
df_12_counts <- readRDS(paste(base_dir, '03_Cohorts_by_variable_type_12_months', 'Neg_claims_MOD_12.rds', sep='/'))%>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)
df_12_freqs <- readRDS(paste(base_dir, '03_Cohorts_by_variable_type_12_months', 'Neg_common_frequencies_MOD_12.rds', sep='/'))%>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)
df_12_dd <- readRDS( paste(base_dir_2, '01_lte_12_months_train', '01_combined_date_differences.rds', sep='/'))%>% 
  dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
  dplyr::arrange(PATIENT_ID)
df_12_dd <- df_12_dd[df_12_dd$label == 0,]
#Load df of <= 13 months lookback

df_13_dates <- readRDS(paste(base_dir, '05_Cohorts_by_variable_type_13_months', 'Neg_dates_MOD_13.rds', sep='/'))%>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)
df_13_counts <- readRDS(paste(base_dir, '05_Cohorts_by_variable_type_13_months', 'Neg_claims_MOD_13.rds', sep='/'))%>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)
df_13_freqs<- readRDS(paste(base_dir, '05_Cohorts_by_variable_type_13_months', 'Neg_common_frequencies_MOD_13.rds', sep='/'))%>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)

#Load df with positives with full lookback
#df_all <- readRDS ('F:/Projects/Strongbridge/data/modelling/Advanced_model_data/05_combined_train_unmatched_test_capped_freq_datediff.rds')
df_all <- readRDS ('F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/00_matched_train_unmatched_test/01_combined_freq_datediff_topcoded.rds')
#Need to get the test negatives as training sample is matched, test is unmatched
df_all <- df_all[df_all$label==0,]%>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)
df_all_counts <- readRDS('F:/Projects/Strongbridge/data/raw_data_cohorts/01_Cohorts_by_variable_type/Modelling/Neg_claims_MOD.rds')%>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)
df_all_dates <- readRDS('F:/Projects/Strongbridge/data/raw_data_cohorts/01_Cohorts_by_variable_type/Modelling/Neg_dates_MOD.rds')%>% 
          dplyr::mutate(PATIENT_ID=as.numeric(PATIENT_ID)) %>% 
          dplyr::arrange(PATIENT_ID)

#CHECK ALL PATIENT IDS MATCH:
print (paste('IDs match with 12 month cohort?', all(df_all$PATIENT_ID == df_12_dates$PATIENT_ID)))
print (paste('IDs match with 13 month cohort?', all(df_all$PATIENT_ID == df_13_dates$PATIENT_ID)))

#For original study, index dates were moved to be date of most recent last_expd (or kept the same if this was > 1 month away from original index date)
#Need to reset to original index dates and re-calculate date differences

#df_all$index_date_2 <- lubridate::mdy(df_12_dates$index_date)
#df_all$index_date <- lubridate::ymd(df_all$index_date)
#df_all[colnames(df_all)[grepl('EXP_DT', colnames(df_all))]] <- df_all[colnames(df_all)[grepl('EXP_DT', colnames(df_all))]] + as.numeric(df_all$index_date_2 - df_all$index_date)
#df_all$index_date <- df_all$index_date_2
#df_all$index_date_2 <- NULL

#DO CHECKS OVER ALL VALID VARIABLES
all_codes <- sub('_AVG_CLAIM_CNT', '', colnames(df_all))
exclude <- c("test_patient_id","index_date","lookback_date", "AGE","GENDER", 'label', 'PATIENT_ID', 'subset')
all_codes <- all_codes[!(all_codes%in% exclude)]
matched_13_check <- c()
for (code in all_codes){
  #CONSTRUCT NAMES FOR EXP_DT, FREQ AND COUNT COLUMNS
  last <- paste0(code, '_LAST_EXP_DT')
  last_dd <- paste0(code, '_LAST_EXP_DT_DIFF')
  first <- paste0(code, '_FIRST_EXP_DT')
  first_dd <- paste0(code, '_FIRST_EXP_DT_DIFF')
  freq <- paste0(code, '_AVG_CLAIM_CNT')
  count <- paste0(code, '_CLAIM_CNT')
  
  #SKIP IF ONE OF THESE VARIABLES DOES NOT EXIST FOR A COHORT
  if (!((freq %in% colnames(df_12_freqs) & freq%in%colnames(df_13_freqs) & freq%in%colnames(df_all)) &
  (first %in% colnames(df_12_dates) & first%in%colnames(df_13_dates) & first_dd%in%colnames(df_all)) &
  (last %in% colnames(df_12_dates) & last%in%colnames(df_13_dates) & last_dd%in%colnames(df_all)) &
  (count %in% colnames(df_12_counts) & count%in%colnames(df_13_counts) & count%in%colnames(df_all_counts)))) next
  
  #CHECK WHICH TEST TO RUN FOR VARIABLE
  #VARIABLE HAS FIRST AND LAST EXP > 365 DAYS FROM INDEX?
  both_gt_12 <- df_all[last_dd] > 365 & df_all[first_dd] > 365
  #VARIABLE HAS FIRST EXP > 365, BUT LAST EXP <= 365 DAYS FROM INDEX?
  first_gt_12 <- df_all[last_dd] < 365 & df_all[first_dd] > 365
  #VARIABLE HAS FIRST AND LAST EXP <= 365 DAYS FROM INDEX?
  first_lt_12 <- df_all[last_dd] < 365 & df_all[first_dd] < 365

  #THESE FAIL:
  #first_gt_12 <- df_all[last] <= 365 & df_all[first] > 365
  #VARIABLE HAS FIRST AND LAST EXP <= 365 DAYS FROM INDEX?
  #first_lt_12 <- df_all[last] <= 365 & df_all[first] <= 365
  #ASSIGN NAs TO F
  both_gt_12[is.na(both_gt_12)] <- F
  first_gt_12[is.na(first_gt_12)] <- F
  first_lt_12[is.na(first_lt_12)] <- F
  
  #RUN TESTS
  
# CHECK 1 -- ---------------------------------------------------
  if (any(both_gt_12)){
    #SELECT PATIENTS THAT FALL INTO GROUPS FROM EACH DF
    patients_both <- df_all$PATIENT_ID[both_gt_12]
    df_all_sel_patient <-  as.numeric(df_all[freq][df_all$PATIENT_ID %in% patients_both,])
    df_all_counts_sel_patient <- as.numeric(df_all_counts[count][df_all_counts$PATIENT_ID %in% patients_both,])
    df_all_dates_sel_patient <- df_all_dates[c(first, last)][df_all_dates$PATIENT_ID %in% patients_both,]
    
    df_13_dates_sel_patient <- df_13_dates[c(first, last)][df_13_dates$PATIENT_ID %in% patients_both,]
    df_13_freqs_sel_patient <-  as.numeric(df_13_freqs[freq][df_13_freqs$PATIENT_ID %in% patients_both,])
    df_13_counts_sel_patient <-  as.numeric(df_13_counts[count][df_13_counts$PATIENT_ID %in% patients_both,])
    
    df_12_dates_sel_patient <- df_12_dates[c(first, last)][df_12_dates$PATIENT_ID %in% patients_both,]
    df_12_freqs_sel_patient <-  as.numeric(df_12_freqs[freq][df_12_freqs$PATIENT_ID %in% patients_both,])
    df_12_counts_sel_patient <-  as.numeric(df_12_counts[count][df_12_counts$PATIENT_ID %in% patients_both,])
    #Expect frequencies to be larger for df_13, and 0 for df_12
    #freq_passed <- all(as.numeric(df_12_freqs_sel_patient) == 0)
    #Expect dates to be same for df_13 and NA for df_12
    #dates_passed <- all(is.na(df_12_dates_sel_patient[last]) & is.na(df_12_dates_sel_patient[last]))
    #Expect 0 count for df_12 and same counts for df_13
    #counts_passed <- all( as.numeric(df_12_counts_sel_patient) == 0)
    
    freq_passed <- all(df_all_sel_patient < df_13_freqs_sel_patient & df_12_freqs_sel_patient == 0)
    #Expect dates to be same for df_13 and NA for df_12
    dates_passed <- all(df_all_dates_sel_patient[first] == df_13_dates_sel_patient[first] & df_all_dates_sel_patient[last] == df_13_dates_sel_patient[last] & is.na(df_12_dates_sel_patient[last]) & is.na(df_12_dates_sel_patient[last]))
    #Expect 0 count for df_12 and same counts for df_13
    counts_passed <- all((df_all_counts_sel_patient == df_13_counts_sel_patient) & df_12_counts_sel_patient == 0)
    matched_13 <- all(df_13_dates_sel_patient[first]==df_all_dates_sel_patient[first])
    matched_13_check <- c(matched_13_check, matched_13)
    print (paste('CHECK 1 PASSED?', (freq_passed & dates_passed & counts_passed)))
    if (!(freq_passed & dates_passed & counts_passed)) break
    print (paste('13 month first same?', matched_13 ))
  } 
  
# CHECK 2 -- ---------------------------------------------------
  if (any(first_gt_12)){
    #SELECT PATIENTS THAT FALL INTO GROUPS FROM EACH DF
    patients_first <- df_all$PATIENT_ID[first_gt_12]
    df_all_sel_patient <-  as.numeric(df_all[freq][df_all$PATIENT_ID %in% patients_first,])
    df_all_counts_sel_patient <-  as.numeric(df_all_counts[count][df_all_counts$PATIENT_ID %in% patients_first,])
    df_all_dates_sel_patient <- df_all_dates[c(first, last)][df_all_dates$PATIENT_ID %in% patients_first,]
    
    df_13_dates_sel_patient <- df_13_dates[c(first, last)][df_13_dates$PATIENT_ID %in% patients_first,]
    df_13_freqs_sel_patient <-  as.numeric(df_13_freqs[freq][df_13_freqs$PATIENT_ID %in% patients_first,])
    df_13_counts_sel_patient <- as.numeric( df_13_counts[count][df_13_counts$PATIENT_ID %in% patients_first,])
    
    df_12_dates_sel_patient <- df_12_dates[c(first, last)][df_12_dates$PATIENT_ID %in% patients_first,]
    df_12_freqs_sel_patient <-  as.numeric(df_12_freqs[freq][df_12_freqs$PATIENT_ID %in% patients_first,])
    df_12_counts_sel_patient <-  as.numeric(df_12_counts[count][df_12_counts$PATIENT_ID %in% patients_first,])
    #WARNING - FOUND CASES WHERE SUM OF COUNTS FOR PATIENTS IN COOHORTS >= 13 AND <= 12 IS OFF BY 1 - DUE TO NOT INCLUDING FIRST_ACTIVITY_LRX_DX_DATE??
    #Expect sum of counts to be same
    #count_passed <- all(as.numeric(df_all_counts_sel_patient) > as.numeric(df_12_counts_sel_patient) )
    #Expect first date same for df_13, last date same for df_12
    #dates_passed <- all(df_all_dates_sel_patient[last] == df_12_dates_sel_patient[last] )
    
    count_passed <- all(df_all_counts_sel_patient > df_13_counts_sel_patient & df_all_counts_sel_patient > df_12_counts_sel_patient & ((df_13_counts_sel_patient + df_12_counts_sel_patient) == df_all_counts_sel_patient) || ((df_13_counts_sel_patient + df_12_counts_sel_patient + 1) == df_all_counts_sel_patient)) 
    #Expect first date same for df_13, last date same for df_12
    dates_passed <- all(df_all_dates_sel_patient[first] == df_13_dates_sel_patient[first] & df_all_dates_sel_patient[last] == df_12_dates_sel_patient[last] )
    matched_13 <- all(df_13_dates_sel_patient[first]==df_all_dates_sel_patient[first])
    matched_13_check <- c(matched_13_check, matched_13)
    passed <- (dates_passed & count_passed)
    print (paste('CHECK 2 PASSED?', (dates_passed & count_passed)))
    if (is.na(passed) | !passed) break
    
    print (paste('13 month first same?', matched_13 ))
  }  
 
# CHECK 3 -- ---------------------------------------------------
  if (any(first_lt_12)){
    #SELECT PATIENTS THAT FALL INTO GROUPS FROM EACH DF
    patients_first <- df_all$PATIENT_ID[first_lt_12]
    df_all_sel_patient_dd <-  df_all[c(freq, first_dd, last_dd)][df_all$PATIENT_ID %in% patients_first,]
    df_all_sel_patient <- as.numeric(df_all_sel_patient_dd[[freq]])
    df_all_counts_sel_patient <-  as.numeric(df_all_counts[count][df_all_counts$PATIENT_ID %in% patients_first,])
    df_all_dates_sel_patient <- df_all_dates[c(first, last)][df_all_dates$PATIENT_ID %in% patients_first,]
    
    df_13_dates_sel_patient <- df_13_dates[c(first, last)][df_13_dates$PATIENT_ID %in% patients_first,]
    df_13_freqs_sel_patient <-  as.numeric(df_13_freqs[freq][df_13_freqs$PATIENT_ID %in% patients_first,])
    df_13_counts_sel_patient <-  as.numeric(df_13_counts[count][df_13_counts$PATIENT_ID %in% patients_first,])
    
    df_12_dates_sel_patient <- df_12_dates[c(first, last)][df_12_dates$PATIENT_ID %in% patients_first,]
    df_12_dd_sel_patient <- df_12_dd[c(first_dd, last_dd)][df_12_dd$PATIENT_ID %in% patients_first,]
    df_12_freqs_sel_patient <-  as.numeric(df_12_freqs[freq][df_12_freqs$PATIENT_ID %in% patients_first,])
    df_12_counts_sel_patient <-  as.numeric(df_12_counts[count][df_12_counts$PATIENT_ID %in% patients_first,])
    #Expect larger frequency for df_12, 0 for df_13
    #freq_passed <- all(as.numeric(df_all_sel_patient) < as.numeric(df_12_freqs_sel_patient))
    #expect first and last same for df_12, NA for df_13
    #dates_passed <- all(df_all_dates_sel_patient[first] == df_12_dates_sel_patient[first] & df_all_dates_sel_patient[last] == df_12_dates_sel_patient[last])
    #Expect same counts for df_12, 0 for df_13
    #counts_passed <- all((as.numeric(df_all_counts_sel_patient) == as.numeric(df_12_counts_sel_patient)))
    dd_passed <- all(df_12_dd_sel_patient[first_dd] == df_all_sel_patient_dd[first_dd] & df_12_dd_sel_patient[last_dd] == df_all_sel_patient_dd[last_dd])
    #Expect larger frequency for df_12, 0 for df_13
    freq_passed <- all(df_all_sel_patient < df_12_freqs_sel_patient & df_13_freqs_sel_patient == 0)
    ##expect first and last same for df_12, NA for df_13
    dates_passed <- all(df_all_dates_sel_patient[first] == df_12_dates_sel_patient[first] & df_all_dates_sel_patient[last] == df_12_dates_sel_patient[last] & is.na(df_13_dates_sel_patient[last]) & is.na(df_13_dates_sel_patient[first]))
    ##Expect same counts for df_12, 0 for df_13
    counts_passed <- all((df_all_counts_sel_patient == df_12_counts_sel_patient) & df_13_counts_sel_patient == 0)
    passed <- (dates_passed & count_passed & counts_passed) 
    print (paste('CHECK 3 PASSED?', (freq_passed & dates_passed & counts_passed & dd_passed)))
    if (is.na(passed) | !passed) break
  }
  
  #Check if any first exposure dates in cohort 13+ are before first exposure dates from original dataset - will tell us if matched data was used as the basis to create the 13+month neg cohort
  neg_matched_for_13 <- lubridate::mdy(df_13_dates[[first]]) < lubridate::mdy(df_all_dates[[first]])
  neg_matched_for_13[is.na(neg_matched_for_13)] <- F
                                                                            
  print (paste('13 unmatched??', any(neg_matched_for_13)))
}
#SUMMARY


# SEEMS LIKE EVENTS WERE REMOVED FOR COHORT 12 IF THEY HAD THEIR LAST EVENT EXACTLY 365 DAYS BEFORE INDEX - DIFFERENT TO POSITIVES
# SEEMS LIKE 13+ WERE MOST LIKELY TAKEN FROM MATCHED NEGATIVES....
#Investigation into whether all events on 365 date are excluded... seems not??
df_12_dd[first_dd][!is.na(df_12_dd[first_dd])][df_12_dd[first_dd][!is.na(df_12_dd[first_dd])] == 365 ]
#[1] 365 365 365 365 365 365 365 365 365
df_all[first][!is.na(df_12_dd[first_dd])][df_12_dd[first_dd][!is.na(df_12_dd[first_dd])] == 365 ]
#[1]  365  365  457  685  849  486  567 2496 1088
df_all$PATIENT_ID[!is.na(df_12_dd[first_dd])][df_12_dd[first_dd][!is.na(df_12_dd[first_dd])] == 365 ]
#[1]   84358364  132982411  322449962  670813799  682174015
#[6]  909011473  977230078 1003869486 1239198545
df_12_dd$PATIENT_ID[!is.na(df_12_dd[first_dd])][df_12_dd[first_dd][!is.na(df_12_dd[first_dd])] == 365 ]
#[1]   84358364  132982411  322449962  670813799  682174015
#[6]  909011473  977230078 1003869486 1239198545
first_dd
#[1] "D_7809_FIRST_EXP_DT_DIFF"
df_12_dates$index_date[df_12_dates$PATIENT_ID==523585154]
#[1] "05/31/2014"
df_all_dates_sel_patient[first][df_12_dd_sel_patient[first_dd] != df_all_sel_patient_dd[first]]
#[1] "05/31/2013" NA          
df_12_dates_sel_patient[first][df_12_dd_sel_patient[first_dd] != df_all_sel_patient_dd[first]]
#[1] "06/01/2013" NA       -> this event had the timeline truncated at 365 days, hence the DD change to 364... why??    
df_12_dd_sel_patient[first_dd][df_12_dd_sel_patient[first_dd] != df_all_sel_patient_dd[first]]
#[1] 364  NA
df_all_sel_patient_dd[first][df_12_dd_sel_patient[first_dd] != df_all_sel_patient_dd[first]]
#[1] 365  NA