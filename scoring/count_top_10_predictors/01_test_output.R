library('dplyr')
library('parallel')
library('foreach')
library('stringr')
base <- 'F:/Projects/Strongbridge/data/scoring_cohort_chunks/'
log_base <- 'F:/Shaun/Strongbridge/'
#Set path for model scores
score_path <- 'F:/Projects/Strongbridge/results/scoring/'
output_path <- 'F:/Projects/Strongbridge/results/scoring/patient_predictor_counts/'
output_prefix <- paste0(output_path, 'patient_predictor_count_')
scored_prefix <- paste0(score_path, 'C')
#Set path for PR curve
PR_curve_path <- 'F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/PR_curve_opt_HP_unmatched.csv'
PR_curve <- readr::read_csv(PR_curve_path)
PR_curve <- cbind(num_patients_with_predictors=0,PR_curve)
PR_curve <- cbind(tot_num_patients=0,PR_curve)
tot_below <- 0
for (i in 1:127){
  output <- readRDS(paste0(output_prefix, i, '.rds'))
  scored <- readRDS(paste0(scored_prefix, str_pad(i, 3, pad='0'), '_score_sample_pred.rds'))
  scored_counts <- readr::read_csv(paste0(scored_prefix, str_pad(i, 3, pad='0'), '_score_sample_counts.csv'))
          
  
  
  #CHECK 1 - SUM OF ALL PATIENTS SHOULD BE THE SAME
  sum_output <- sum(output$tot_num_patients)
  sum_output <- sum_output + sum(scored$prob.1 < PR_curve$thresh[[100]])
  tot_below <- tot_below + sum(scored$prob.1 < PR_curve$thresh[[100]])
  sum_scored <- nrow(scored)
  #sum_counts <- sum(scored_counts$counts_sample_scoring_cohort)
  check_1_pass <- sum_output==sum_scored
  #print (paste('CHECK 1', check_1_pass))
  
  #CHECK 2 - CHECK TOTALS IN EACH BIN ARE OK
  check_2 <- c() 
  for (j in 1:100){
    if (j==1) {
      cur_check_2 <- sum(scored$prob.1 >= PR_curve$thresh[[j]]) == output$tot_num_patients[[j]]
      PR_curve$tot_num_patients[[j]] <- PR_curve$tot_num_patients[[j]] + sum(scored$prob.1 > PR_curve$thresh[[j]])
      
      #print (paste('CHECK 2',  j, cur_check_2))
    }
    else {
      cur_check_2 <- sum(scored$prob.1 >= PR_curve$thresh[[j]] & scored$prob.1 < PR_curve$thresh[[j-1]]) == output$tot_num_patients[[j]]
      PR_curve$tot_num_patients[[j]] <- PR_curve$tot_num_patients[[j]] + sum(scored$prob.1 > PR_curve$thresh[[j]] & scored$prob.1 < PR_curve$thresh[[j-1]])
      
      #print (paste('CHECK 2', j,  cur_check_2))
    }
    #cur_check_2_i <- scored_counts$counts_sample_scoring_cohort[[j]] == output$tot_num_patients[[j]]
    #check_2 <- c(check_2, cur_check_2&cur_check_2_i)
    check_2 <- c(check_2, cur_check_2)
  }
  
  #LACHLAN USES SAPPLY OVER PR CURVE THRESHOLDS TO GET CUM SUM, DO THIS AND COMPARE...
  counts <- sapply(PR_curve$thresh, function(x) { length(scored$prob.1[scored$prob.1 >= x]) })
  check_2_ii <- counts == cumsum(output$tot_num_patients)
  check_2_iii <- scored_counts$counts_sample_scoring_cohort == cumsum(output$tot_num_patients)
  #CHECK IF ALL SECONDARY CHECKS PASSED
  check_2_pass <- all(check_2) & all(check_2_ii)
  print(all(check_2_ii))
  if (!(check_1_pass && check_2_pass)) {
    print (paste('ERROR: FILE ', i, 'DID NOT PASS!!'))
    print (paste('CHECK 2 ii', check_2_ii))
    print (paste('CHECK 2', check_2_pass))
    print (paste('CHECK 1', check_1_pass))
    break
  }
  
  print (paste(i, 'PASSED'))
}


