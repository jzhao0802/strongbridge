#  ------------------------------------------------------------------------
#   Sum all counts from all chunks
#  ------------------------------------------------------------------------

num_chunks = c(1:28)
counts <- vector("list") 

counts[[1]]<- read_csv(paste0(results_dir, "C", str_pad(1, 3, pad = "0"), "_score_sample_counts.csv"))
all_counts <- data.frame(
  counts[[1]][,1:3], 
  count_pats = counts[[1]]$counts_sample_scoring_cohort,
  count_nobrain = counts[[1]]$patients_with_HYPP_and_CAIs
)

for (i in num_chunks[-1]) {
  
  counts[[i]]<- read_csv(paste0(results_dir, "C", str_pad(i, 3, pad = "0"), "_score_sample_counts.csv"))

  all_counts$count_pats = counts[[i]]$counts_sample_scoring_cohort + all_counts$count_pats
  all_counts$count_nobrain = counts[[i]]$patients_with_HYPP_and_CAIs + all_counts$count_nobrain
  
}

write.csv(all_counts, 
          paste0(results_dir, "all_counts.csv")
)

#  ------------------------------------------------------------------------
#   Count of dirty/clean ppp 
#  ------------------------------------------------------------------------
score_dir <- "F:/Projects/Strongbridge/data/scoring_cohort_chunks/"
pr_curve <- read_csv(paste0(model_dir, "PR_curve_opt_HP_unmatched.csv"))
counts_ppp <- vector("list")

num_chunks = c(1:28)
 
# Count per chunk 
for (i in num_chunks) {

  pred_merge <- merge(
    read_rds(paste0(results_dir, "C", str_pad(i, 3, pad = "0"), "_score_sample_pred.rds")), 
    
    read_csv(paste0(score_dir, "Scoring_Final_Sample_", chunk, ".csv"),
             col_types = (cols(PATIENT_ID = col_character(), .default = col_guess()))) %>% 
      select(PATIENT_ID, clean_ppp_clm_cnt, dirty_ppp_clm_cnt), 
    
    by = "PATIENT_ID")
  
  clean_count <- sapply( pr_curve$thresh, function(x) { 
      length(pred_merge$prob.1[pred_merge$prob.1 >= x & pred_merge$clean_ppp_clm_cnt > 0])
      })
  dirty_count <- sapply( pr_curve$thresh, function(x) { 
    length(pred_merge$prob.1[pred_merge$prob.1 >= x & pred_merge$dirty_ppp_clm_cnt > 0])
      })
  
  counts_ppp[[i]] <- data.frame(pr_curve, 
                          clean_counts= clean_count,
                          dirty_counts = dirty_count
         )
}

# Sum all counts together

ppp_all_counts <- data.frame(
  counts_ppp[[1]][,1:3], 
  clean_counts = counts_ppp[[1]]$clean_counts,
  dirty_counts = counts_ppp[[1]]$dirty_counts
)

for (i in num_chunks[-1]) {
  
  ppp_all_counts$clean_counts = counts_ppp[[i]]$clean_counts + ppp_all_counts$clean_counts
  ppp_all_counts$dirty_counts = counts_ppp[[i]]$dirty_counts + ppp_all_counts$dirty_counts
  
}

#  ------------------------------------------------------------------------
#   Patient profiles
#  ------------------------------------------------------------------------
profiles <- vector("list")

num_chunks = c(1:8, 13:18)

for (i in num_chunks) {
  profiles[[i]] <- read_rds(paste0(results_dir, "C", str_pad(i, 3, pad = "0"), "_score_sample_patient_profiles.rds"))
}

profiles_all <- profiles[[1]]
for (i in num_chunks[-1]) {
  profiles_all <- rbind(profiles_all, profiles[[i]])
}

profiles_all <- arrange(profiles_all, desc(prob.1))
write.csv(profiles_all, paste0(results_dir, "top_10_patient_profiles_1_8_13_18.csv"))
