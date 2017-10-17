#Set base directory for scoring files
library('dplyr')
library('parallel')
library('foreach')
base <- 'F:/Projects/Strongbridge/data/scoring_cohort_chunks/'
log_base <- 'F:/Shaun/Strongbridge/'
#Set path for model scores
score_path <- 'F:/Projects/Strongbridge/results/scoring/'
output_path <- 'F:/Projects/Strongbridge/results/scoring/patient_predictor_counts/'

#Set path for PR curve
PR_curve_path <- 'F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/PR_curve_opt_HP_unmatched.csv'

#score_path <- 'F:/Projects/Strongbridge/results/scoring/PATIENT_ID_and_MODEL_SCORE_list.rds'
#Load model scores
#scores <- readRDS(score_path)
#score_1 <-readRDS('F:/Projects/Strongbridge/results/scoring/C001_score_sample_pred.rds')

#Double check split files are ok to use
#for (i in 1:127){
#  if(i<10) s <- readRDS(paste0('F:/Projects/Strongbridge/results/scoring/C00', i, '_score_sample_pred.rds'))
#  else if (i<100) s <- readRDS(paste0('F:/Projects/Strongbridge/results/scoring/C0', i, '_score_sample_pred.rds'))
#  else s <- readRDS(paste0('F:/Projects/Strongbridge/results/scoring/C', i, '_score_sample_pred.rds'))
#  g <- all(s$prob.1 == scores[[i]]$model_score)
#  if (!g){
#    print ('NOT GOOD')
#    break
#  }
#  print(i)
#  print(g)
#}



scoring_file_prefix <- paste(base, 'Scoring_Final_Sample_C', sep='/')
scores_file_prefix <- paste(score_path, 'C', sep='/')


#cl <- makeCluster(detectCores ())
library('doParallel')
#registerDoParallel(detectCores(), type='FORK')
registerDoParallel(3, type='FORK')

#Set features that are required for selection
subset <- c('CLAIM_CNT', 'FLAG', 'AVG_CLAIM_CNT', 'AVG_CLAIM')
features <- c('D_2768', 'G_797000', 'S_S44', 'G_371000', 'D_7807', 'D_7288', 'D_3449', 'G_372000', 'D_7802', 'D_3441')

start_file = 46
end_file = 127

#end_file = 47
#start_file = 1
#end_file = 100
#output <- foreach(i=start_file:end_file) %dopar% {scores[[i]]}

for (i in start_file:end_file){
#output<-foreach(i=start_file:end_file) %dopar% {
#foreach(i=start_file:end_file) %dopar% {
  #Get scores for current file
  #Start timing
  i=46
  gc()
  output_file <- paste0(paste(output_path, 'patient_predictor_count_', sep='/'), i, '.rds')
  #if (file_test("-f", output_file)) return(NULL)
  if (file_test("-f", output_file)) next
  
  logfile <- paste(log_base, Sys.getpid(), sep='/')
  write('Starting loop', file=logfile, append=TRUE)
  start <- Sys.time()

  #Load PR curve
  PR_curve <- readr::read_csv(PR_curve_path)
  PR_curve <- cbind(num_patients_with_predictors=0,PR_curve)
  PR_curve <- cbind(tot_num_patients=0,PR_curve)
  
  
  write('Setting score matrix', file=logfile, append=TRUE)
  #cur_scores<-scores[[i]]
  #Set path for current scoring cohort file

  cur_scores_file_path <- paste0(scores_file_prefix, str_pad(i, 3, pad='0'), '_score_sample_pred.rds')
  file <- paste0(scoring_file_prefix, str_pad(i, 3, pad='0'), '.csv')
  
  
  write(paste('Loading CSV file', file), file=logfile, append=TRUE)
  rm(df)
  rm(cur_scores)
  gc()
  #Read in patients from current chunk
  df <- readr::read_csv(file)
  cur_scores <- readRDS(cur_scores_file_path)
  cur_scores$model_score <- cur_scores$prob.1 
  #If on first file, set features to check
  #if (i == start_file) {
    #Set features that are available in the dataset (only need to do this for first file...)
  write('Setting features', file=logfile, append=TRUE)
    all_features <- c()
    
    all_variables <- names(df)
    for (feature in features){
      for (i in subset){
        new_feature <- paste(feature, i, sep='_')
        if (new_feature %in% all_variables) all_features <- c(all_features, new_feature)
        #if (new_feature %in% all_variables) print(paste(new_feature))
        
      }
    }
  #}
  

  
  write('Setting Scores', file=logfile, append=TRUE)
  #Select subset of predictors that we require a patient to have
  df_f<-df %>% dplyr::select(all_features)
  #Set all na's to 0 to stop NA issues during summations later
  df_f[is.na(df_f)] <- 0
  df_f <- as.data.frame(lapply(df_f, as.numeric))
  #create truth table of predictors that have values (i.e. > 0)
  #selected_patients_truth_table <- df_f > 0
  #Perform sum across columns and check if sum is > 0 - this is effectively doing an OR operation across columns
  selected_patients <- rowSums(df_f) >0
  #Get patient ids for patients that have these indicators
  patients_with_indicators <- df$PATIENT_ID[selected_patients]
  #Get scores for patients with these indicators
  filtered_patient_scores <- cur_scores[cur_scores$PATIENT_ID %in% patients_with_indicators,]
  #Loop over all recall bins in the PR curve and count the number of patients within each bin
  for (j in 1:100){
    lower <- PR_curve$thresh[[j]]
    if (j == 1) {
      PR_curve$num_patients_with_predictors[[j]] <- PR_curve$num_patients_with_predictors[[j]] + sum(filtered_patient_scores$model_score >= lower)
      PR_curve$tot_num_patients[[j]] <- PR_curve$tot_num_patients[[j]] + sum(cur_scores$model_score >= lower)
    }
    else {
      upper <- PR_curve$thresh[[j-1]]
      PR_curve$num_patients_with_predictors[[j]] <- PR_curve$num_patients_with_predictors[[j]] + sum(filtered_patient_scores$model_score >= lower & filtered_patient_scores$model_score < upper)
      PR_curve$tot_num_patients[[j]] <- PR_curve$tot_num_patients[[j]] + sum(cur_scores$model_score >= lower & cur_scores$model_score < upper)
    }
    
  }
  
  #End timing
  end <- Sys.time()
  write(paste('Loop took', end-start, 'seconds'), file=logfile, append=TRUE)
  write(paste('Saving RDS file', output_file), file=logfile, append=TRUE)
  
  saveRDS(PR_curve, output_file)
  #PR_curve
  
  #break
}



