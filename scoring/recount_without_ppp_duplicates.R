library('dplyr')
library('foreach')
library('stringr')
base <- 'F:/Projects/Strongbridge/data/scoring_cohort_chunks/'
log_base <- 'F:/Shaun/Strongbridge/'
#Set path for model scores
score_path <- 'F:/Projects/Strongbridge/results/scoring/'
output_path <- 'F:/Projects/Strongbridge/results/scoring/patient_predictor_counts/'
output_prefix <- paste0(output_path, 'patient_predictor_count_')
scored_prefix <- paste0(score_path, 'C')
summary_path <- paste0(score_path, 'scoring_summaries/')
lachlan_counts <- readr::read_csv(paste(score_path, 'scoring_summaries/all_counts_127_exluding_ppp_codes.csv', sep='/'))

#Set path for PR curve
PR_curve_path <- 'F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/PR_curve_opt_HP_unmatched.csv'
PR_curve <- readr::read_csv(PR_curve_path)
PR_curve$tot_num_patients <- 0
PR_curve$tot_num_patients_no_dup <- 0
PR_curve$tot_num_patients_no_ppp <- 0
PR_curve$tot_num_patients_no_dup_no_ppp <- 0
tot_below <- 0
tot_below_no_dup <- 0
duplicate_ppp_ids <- readRDS(paste0(score_path, 'ppp_duplicate_ids.rds'))
additional_PPP_information <- read.csv(paste(base, 'Strongbridge_Cohort_128_Additional Specs_20171025.csv', sep='/'))
additional_PPP_information$PATIENT_ID <- as.double(additional_PPP_information$PATIENT_ID)
for (i in 1:128){
  print(paste0(scored_prefix, stringr::str_pad(i, 3, pad='0'), '_score_sample_pred.rds'))
  scored <- readRDS(paste0(scored_prefix, stringr::str_pad(i, 3, pad='0'), '_score_sample_pred.rds'))
  scored$PATIENT_ID <- as.double(scored$PATIENT_ID)
  if (i!=128) {
    #Remove duplicated for chunks 1:127
    scored_no_dup <- scored[!(scored$PATIENT_ID %in% duplicate_ppp_ids$PATIENT_ID),]
  } else {
    #Need to remove patients without Rx data
    scored <- scored[scored$PATIENT_ID %in% additional_PPP_information$PATIENT_ID[additional_PPP_information$rx_flag==1], ]
    scored_no_dup <- scored
  }
  for (j in 1:100){
    if (j==1) {
      PR_curve$tot_num_patients[[j]] <- PR_curve$tot_num_patients[[j]] + sum(scored$prob.1 >= PR_curve$thresh[[j]])
      PR_curve$tot_num_patients_no_dup[[j]] <- PR_curve$tot_num_patients_no_dup[[j]] + sum(scored_no_dup$prob.1 >= PR_curve$thresh[[j]])
      if (i!=128){
        PR_curve$tot_num_patients_no_ppp[[j]] <- PR_curve$tot_num_patients_no_ppp[[j]] + sum(scored$prob.1 >= PR_curve$thresh[[j]])
        PR_curve$tot_num_patients_no_dup_no_ppp[[j]] <- PR_curve$tot_num_patients_no_dup_no_ppp[[j]] + sum(scored_no_dup$prob.1 >= PR_curve$thresh[[j]])
      }
      #print (paste('CHECK 2',  j, cur_check_2))
    }
    else {
      PR_curve$tot_num_patients[[j]] <- PR_curve$tot_num_patients[[j]] + sum(scored$prob.1 >= PR_curve$thresh[[j]] & scored$prob.1 < PR_curve$thresh[[j-1]])
      PR_curve$tot_num_patients_no_dup[[j]] <- PR_curve$tot_num_patients_no_dup[[j]] + sum(scored_no_dup$prob.1 >= PR_curve$thresh[[j]] & scored_no_dup$prob.1 < PR_curve$thresh[[j-1]])
      if (i!=128){
        PR_curve$tot_num_patients_no_ppp[[j]] <- PR_curve$tot_num_patients_no_ppp[[j]] + sum(scored$prob.1 >= PR_curve$thresh[[j]] & scored$prob.1 < PR_curve$thresh[[j-1]])
        PR_curve$tot_num_patients_no_dup_no_ppp[[j]] <- PR_curve$tot_num_patients_no_dup_no_ppp[[j]] + sum(scored_no_dup$prob.1 >= PR_curve$thresh[[j]] & scored_no_dup$prob.1 < PR_curve$thresh[[j-1]])
      }
      #print (paste('CHECK 2', j,  cur_check_2))
    }
    #cur_check_2_i <- scored_counts$counts_sample_scoring_cohort[[j]] == output$tot_num_patients[[j]]
    #check_2 <- c(check_2, cur_check_2&cur_check_2_i)
  }
  
  tot_below <-  tot_below + sum(scored$prob.1 < PR_curve$thresh[[j]])
  tot_below_no_dup <-  tot_below_no_dup + sum( scored_no_dup$prob.1 < PR_curve$thresh[[j]])
  

}
tot_bel <- 27791902
tot_bel_n_dup <- 27791599

PR_curve$cum_num_patients <- cumsum(PR_curve$tot_num_patients)
PR_curve$cum_num_patients_no_dup <- cumsum(PR_curve$tot_num_patients_no_dup)
PR_curve$cum_num_patients_no_ppp <- cumsum(PR_curve$tot_num_patients_no_ppp)
PR_curve$cum_num_patients_no_dup_no_ppp <- cumsum(PR_curve$tot_num_patients_no_dup_no_ppp)
PR_curve <- PR_curve %>% 
  dplyr::rename('Recall Bin'='rec_binned', 'Threshold'='thresh', 'Cumulative Precision'='prec', 'Total Count (with PPP, with duplicates)'=tot_num_patients, 'Total Count (with PPP, without duplicates)'=tot_num_patients_no_dup, 'Total Count (without PPP, with duplicates)'=tot_num_patients_no_ppp, 'Total Count (without PPP, without duplicates)'=tot_num_patients_no_dup_no_ppp) %>% 
  dplyr::rename('Cumulative Count (with PPP, with duplicates)'=cum_num_patients, 'Cumulative Count (with PPP, without duplicates)'=cum_num_patients_no_dup, 'Cumulative Count (without PPP, with duplicates)'=cum_num_patients_no_ppp, 'Cumulative Count (without PPP, without duplicates)'=cum_num_patients_no_dup_no_ppp) 
saveRDS(PR_curve, paste0(summary_path, 'total_counts_minus_ppp_duplicates_no_rx.rds'))
readr::write_csv(PR_curve, paste0(summary_path, 'total_counts_minus_ppp_duplicates_no_rx.csv'))


#PR_curve <- readRDS(paste0(summary_path, 'total_counts_minus_ppp_duplicates.rds')) %>%
#  dplyr::select('Recall Bin', 'Threshold', 'Cumulative Precision', 'Total Count (without PPP, with duplicates)', 'Total Count (without PPP, without duplicates)') %>%
#  dplyr::rename('rec_binned'='Recall Bin', 'thresh'='Threshold', 'tot_num_patients_no_ppp'='Total Count (without PPP, with duplicates)', 'tot_num_patients_no_dup_no_ppp'='Total Count (without PPP, without duplicates)', 'prec'='Cumulative Precision')
  
#PR_curve$tot_num_patients <- PR_curve$tot_num_patients_no_ppp
#PR_curve$tot_num_patients_no_dup <- PR_curve$tot_num_patients_no_dup_no_ppp
