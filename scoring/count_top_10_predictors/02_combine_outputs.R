library('dplyr')
library('parallel')
library('foreach')
library('readr')
library('stringr')
base <- 'F:/Projects/Strongbridge/data/scoring_cohort_chunks/'
log_base <- 'F:/Shaun/Strongbridge/'
#Set path for model scores
score_path <- 'F:/Projects/Strongbridge/results/scoring/'
output_path <- 'F:/Projects/Strongbridge/results/scoring/patient_predictor_counts/'
lachlan_counts <- readr::read_csv(paste(score_path, 'scoring_summaries/all_counts_127_exluding_ppp_codes.csv', sep='/'))

output_prefix <- paste0(output_path, 'patient_predictor_count_')
scored_prefix <- paste0(score_path, 'C')

#LOAD PR CURVE AND SET COUNTS TO 0
PR_curve_path <- 'F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/PR_curve_opt_HP_unmatched.csv'
PR_curve <- readr::read_csv(PR_curve_path)
PR_curve <- cbind(num_patients_with_predictors=0,PR_curve)
PR_curve <- cbind(tot_num_patients=0,PR_curve)
#PR_curve <- cbind(validation_cum_tot_num_patients=0,PR_curve)


#LOOP OVER OUTPUTS AND SUM TO PR CURVE
for (i in 1:127){
  output <- readRDS(paste0(output_prefix, i, '.rds'))
  scored_counts <- readr::read_csv(paste0(scored_prefix, str_pad(i, 3, pad='0'), '_score_sample_counts.csv'))
  PR_curve$tot_num_patients <- PR_curve$tot_num_patients + output$tot_num_patients
  PR_curve$num_patients_with_predictors <- PR_curve$num_patients_with_predictors + output$num_patients_with_predictors
  #PR_curve$validation_cum_tot_num_patients <- PR_curve$validation_cum_tot_num_patients + scored_counts$counts_sample_scoring_cohort
}



#Set total number of patients below lowest threshold (calculated in test_output.R)
tot_below <- 27791838
#Add total number below threshold (may need to go back and get total number with predictors below threshold...)
#tot_below_row <- data.frame(tot_num_patients=tot_below, rec_binned=0, prec=0, thresh=0, num_patients_with_predictors=0, validation_cum_tot_num_patients=0)
tot_below_row <- data.frame(tot_num_patients=tot_below, rec_binned=0, prec=0, thresh=0, num_patients_with_predictors=0)
PR_curve <- rbind(PR_curve, tot_below_row)

#CALCULATE PERCENTAGE IN EACH BIN (will be 0 for below lowest threshold row as don't currently have number of patients with predictors in this region)
PR_curve$percentage_patients_with_predictors <- as.numeric(PR_curve$num_patients_with_predictors)/as.numeric(PR_curve$tot_num_patients)
PR_curve$tot_cum_sum <- cumsum(PR_curve$tot_num_patients)
PR_curve$validated <- PR_curve$tot_cum_sum == lachlan_counts$count_pats

merged_output_path <- paste0(output_prefix, 'merged')
saveRDS(PR_curve, paste0(merged_output_path, '.rds'))
PR_curve_renamed <- PR_curve %>% dplyr::rename('Total Number of Patients'=tot_num_patients, 'Number of Patients with Predictors'=num_patients_with_predictors, 'Recall Bin'=rec_binned, 'Precision'=prec, 'Threshold'=thresh, 'Percentage of Patients with Predictors'=percentage_patients_with_predictors, 'Validated'=validated, 'Total Cumulative Sum'=tot_cum_sum)
readr::write_csv(PR_curve_renamed, paste0(merged_output_path, '.csv'))
