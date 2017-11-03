library('dplyr')
library('foreach')
library('stringr')
#Set base path for scoring chunks
base <- 'F:/Projects/Strongbridge/data/scoring_cohort_chunks/'
#Set path for model scores
score_path <- 'F:/Projects/Strongbridge/results/scoring/'
scoring_file_prefix <- paste(base, 'Scoring_Final_Sample_C', sep='/')

#Set prefix for scored chunks
scored_prefix <- paste0(score_path, 'C')
#Set path for summaries for outputs
summary_path <- paste0(score_path, 'scoring_summaries/')

#Set empty patient list
full_patient_list <- list()
#Load in PPP chunk (all patients)
i<-128
scoring_filename <- paste0(scoring_file_prefix, stringr::str_pad(i, 3, pad='0'), '.csv')
clean_dirty_patients <- read.csv(scoring_filename)
clean_dirty_patients$PATIENT_ID <- as.double(clean_dirty_patients$PATIENT_ID )
all_duplicate_ids <- c()
chunks <- c()
all_ids <- c()
#Loop over other chunks to look for duplocates
for (i in 1:127){
  #Load scored chunk
  scoring_filename <- paste0(scored_prefix, str_pad(i, 3, pad='0'), '_score_sample_pred.rds')
  scored <- readRDS(scoring_filename)
  scored$PATIENT_ID  <- as.double(scored$PATIENT_ID)
  print(paste0('Using file ', scoring_filename))
  all_ids <- c(scored$PATIENT_ID)
  ids <- scored$PATIENT_ID[scored$PATIENT_ID %in% clean_dirty_patients$PATIENT_ID]
  for (id in ids){
    all_duplicate_ids <- c(all_duplicate_ids, id)
    chunks <- c(chunks, i)
  }
  
}

#create df and save
duplicate_ids_df <- data.frame(chunk=chunks, PATIENT_ID=all_duplicate_ids)
saveRDS(duplicate_ids_df, paste0(score_path, 'ppp_duplicate_ids.rds'))

#investigate for single chunk (127 for now)
i <- 127
file <- paste0(scoring_file_prefix, stringr::str_pad(i, 3, pad='0'), '.csv')
df <- readr::read_csv(file)
chunk_127_duplicate <- duplicate_ids_df$PATIENT_ID[duplicate_ids_df$chunk == 127]
chunk_127_duplicate_patients <- df[df$PATIENT_ID %in% chunk_127_duplicate,] %>% dplyr::arrange(PATIENT_ID)
ppp_chunk_127_patients <- clean_dirty_patients[clean_dirty_patients$PATIENT_ID %in% chunk_127_duplicate,] %>% dplyr::arrange(PATIENT_ID)

#calculate lookbacks to compare...
chunk_127_duplicate_patients$lookback_period <- lubridate::mdy(chunk_127_duplicate_patients$index_dt2) - lubridate::mdy(chunk_127_duplicate_patients$lookback_dt2)
ppp_chunk_127_patients$lookback_period <- lubridate::mdy(ppp_chunk_127_patients$index_dt2) - lubridate::mdy(ppp_chunk_127_patients$lookback_dt2)
#do genders match?
genders_matched <- all(chunk_127_duplicate_patients$gender == ppp_chunk_127_patients$gender)
#YES... do ages match?
ages_matched <- all(chunk_127_duplicate_patients$age == ppp_chunk_127_patients$age)
#NOPE...
