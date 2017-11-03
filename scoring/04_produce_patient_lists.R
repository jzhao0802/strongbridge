library('dplyr')
library('parallel')
library('foreach')
library('stringr')
#Set base path for scoring chunks
base <- 'F:/Projects/Strongbridge/data/scoring_cohort_chunks/'
#Set path for model scores
score_path <- 'F:/Projects/Strongbridge/results/scoring/'

#Set prefix for scored chunks
scored_prefix <- paste0(score_path, 'C')
#Set path for summaries for outputs
summary_path <- paste0(score_path, 'scoring_summaries/')
#Set path for PR curve
PR_curve_path <- 'F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/PR_curve_opt_HP_unmatched.csv'
PR_curve <- readr::read_csv(PR_curve_path)

#Set initial patient type ("Predicted" if not using chunk 128, else "Diagnosed")
patient_type <- 'Predicted'
#Set requested recall and model thresholds
recall_threshold <- 35
threshold <- PR_curve$thresh[[35]] 
print(threshold)
#Set empty patient list
full_patient_list <- list()
#duplicate_ppp_ids <- readRDS(paste0(score_path, 'ppp_duplicate_ids.rds'))

scoring_file_prefix <- paste(base, 'Scoring_Final_Sample_C', sep='/')
#Load in PPP patient info (need this to check lookbacks)
i<-128
scoring_filename <- paste0(scoring_file_prefix, stringr::str_pad(i, 3, pad='0'), '.csv')
clean_dirty_patients <- read.csv(scoring_filename)
clean_dirty_patients$PATIENT_ID <- as.double(clean_dirty_patients$PATIENT_ID )
#Calculate lookback period for PPP patients
clean_dirty_patients$lookback_period <- lubridate::mdy(clean_dirty_patients$index_dt2) - lubridate::mdy(clean_dirty_patients$lookback_dt2)

#Load in RX information for PPP patients
additional_PPP_information <- read.csv(paste(base, 'Strongbridge_Cohort_128_Additional Specs_20171025.csv', sep='/'))
additional_PPP_information$PATIENT_ID <- as.double(additional_PPP_information$PATIENT_ID)
duplicate_ids <- c()
#Start loop
for (i in 1:128){
  #Load scored chunk
  scoring_filename <- paste0(scored_prefix, str_pad(i, 3, pad='0'), '_score_sample_pred.rds')
  print(paste0('Using file ', scoring_filename))
  scored <- readRDS(scoring_filename) 
  scored$PATIENT_ID <- as.double(scored$PATIENT_ID)
  if(i == 128) {
    #Set type to "Diagnosed" if using chunk 128
    #Include all patients for chunk 128, but set those with lookback < 24 months to NA precision and model score
    patient_type <- 'Diagnosed'
    #patient_list <- scored[scored$PATIENT_ID %in% additional_PPP_information$PATIENT_ID[additional_PPP_information$rx_flag==1], ] %>% dplyr::select(-one_of('prob.0', 'response'))
    patient_list <- scored %>% dplyr::select(-one_of('prob.0', 'response'))
    
  } else {
    #Remove PPP duplicates
    duplicates <- scored$PATIENT_ID %in% clean_dirty_patients$PATIENT_ID 
    duplicate_ids <- c(duplicate_ids, scored$PATIENT_ID[duplicates])
    scored <- scored[!(duplicates),]
    #Get patients above threshold
    patient_list <- scored[scored$prob.1 >= threshold,] %>% dplyr::select(-one_of('prob.0', 'response'))
  }
  #Set patient type
  patient_list['Diagnosed or Predicted'] <- patient_type
  #Save to list
  full_patient_list[i] <- list(patient_list)

}



#Combine outputs and rename score and id columns
full_patient_df <- full_patient_list %>% 
  dplyr::bind_rows() %>% 
  dplyr::arrange(desc(prob.1)) %>%
  dplyr::rename('Model Score'='prob.1', 'Patient Id'='PATIENT_ID')
full_patient_df$`Patient Id` <- as.double(full_patient_df$`Patient Id`)
prec_1_per_buckets <- readr::read_csv(paste0(summary_path, 'precision_1_percent_buckets.csv'))
#Set precision for each patient based on precision in 1% recall buckets
full_patient_df$Precision <- 0
for (j in 100:1){
  print(PR_curve$prec[[j]])
  matched <- full_patient_df['Model Score'] >= PR_curve$thresh[[j]]
  print(any(matched))
  if (sum(matched))
    full_patient_df[matched,]$Precision <- prec_1_per_buckets$prec[[j]]
}
full_patient_df <- full_patient_df %>% dplyr::rename('Precision 1% Bucket'='Precision')

#Add patient ids that have lookback < 24 months and set precision/score to NA (all of these will come from PPP patients)
#Note: these patients were not scored, so no need to set as NAs
patient_ids <- clean_dirty_patients$PATIENT_ID[clean_dirty_patients$lookback_period < 730]
patient_ids_df_lookback_lt_24 <- data.frame(`Patient Id` = patient_ids, check.names = FALSE)
patient_ids_df_lookback_lt_24$`Precision 1% Bucket` = NA
patient_ids_df_lookback_lt_24$`Model Score` = NA
patient_ids_df_lookback_lt_24$`Diagnosed or Predicted` = 'Diagnosed'

#Double check counts for each threshold make sense (counts are made in recount_without_ppp_duplicates.R).
full_counts <- readRDS(paste0(summary_path, 'total_counts_minus_ppp_duplicates.rds'))
all_pass <- c()
for (i in 1:35){
  test_sum <- sum(full_patient_df$`Model Score`[full_patient_df$`Diagnosed or Predicted`=='Predicted']>=full_counts$Threshold[[i]])
  pass <- full_counts$`Cumulative Count (without PPP, without duplicates)`[[i]] == test_sum
  
  print (pass)
  all_pass <- c(all_pass, pass)
}
print(all(all_pass))
#Add patients with lookback < 24 months
full_patient_df <- full_patient_df %>% 
  dplyr::bind_rows(patient_ids_df_lookback_lt_24) %>%
  dplyr::arrange(desc(`Model Score`)) %>%
  dplyr::select(`Model Score`, `Precision 1% Bucket`, `Patient Id`, `Diagnosed or Predicted`)
#Save final output (still has patients without Rx )
saveRDS(full_patient_df, paste(summary_path, 'patient_list_recall_35_no_duplicates.rds', sep='/'))
readr::write_csv(full_patient_df, paste(summary_path, 'patient_list_recall_35_no_duplicates.csv', sep='/'))

#Create patient list for those <= 20/25% threshold (and filter out no Rx patients)
full_patient_df <- readRDS(paste(summary_path, 'patient_list_recall_35_no_duplicates.rds', sep='/'))
patients_without_rx <- additional_PPP_information$PATIENT_ID[additional_PPP_information$rx_flag==0]
full_patient_df <- full_patient_df[!(full_patient_df$`Patient Id` %in% patients_without_rx), ]
saveRDS(full_patient_df, paste(summary_path, 'patient_list_recall_35_no_duplicates_no_rx.rds', sep='/'))
readr::write_csv(full_patient_df, paste(summary_path, 'patient_list_recall_35_no_duplicates_no_rx.csv', sep='/'))
full_patient_df <- readRDS(paste(summary_path, 'patient_list_recall_35_no_duplicates_no_rx.rds', sep='/'))

 full_patient_df_20 <- full_patient_df[full_patient_df$`Diagnosed or Predicted`=='Predicted' & full_patient_df$`Model Score` >=  PR_curve$thresh[[20]],] %>%
  dplyr::bind_rows(full_patient_df[full_patient_df$`Diagnosed or Predicted`=='Diagnosed',]) %>%
  dplyr::arrange(desc(`Model Score`))
# saveRDS(full_patient_df_20, paste(summary_path, 'patient_list_recall_20_no_duplicates_no_rx.rds', sep='/'))
readr::write_csv(full_patient_df_20, paste(summary_path, 'patient_list_recall_20_no_duplicates_no_rx.csv', sep='/'))
 
full_patient_df_25 <- full_patient_df[full_patient_df$`Diagnosed or Predicted`=='Predicted' & full_patient_df$`Model Score` >=  PR_curve$thresh[[25]],] %>%
  dplyr::bind_rows(full_patient_df[full_patient_df$`Diagnosed or Predicted`=='Diagnosed',]) %>%
  dplyr::arrange(desc(`Model Score`))
# saveRDS(full_patient_df_25, paste(summary_path, 'patient_list_recall_25_no_duplicates_no_rx.rds', sep='/'))
readr::write_csv(full_patient_df_25, paste(summary_path, 'patient_list_recall_25_no_duplicates_no_rx.csv', sep='/'))
