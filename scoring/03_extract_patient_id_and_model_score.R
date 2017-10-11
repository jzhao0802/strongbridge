
#  ------------------------------------------------------------------------
# Create a dataframe with PATIENT_ID and model score
#  ------------------------------------------------------------------------

library(tidyverse)
library(stringr)

pred_dir <- "F:/Projects/Strongbridge/results/scoring/"

# define empty list
pred_list <- list() 

# Loop through all prediction objects, extracting patient id and model score.
# Stack in list
for(i in 1:128) {
  
  file_name <- paste0("C", str_pad(i, 3, pad = "0"), "_score_sample_pred.rds")
  
  pred <- read_rds(paste0(pred_dir, file_name))
  
  pred_list[[i]] <- data.frame(PATIENT_ID = pred$PATIENT_ID,
                               model_score = pred$prob.1)
  
  print(i)
  
}

write_rds(pred_list, paste0(pred_dir, "PATIENT_ID_and_MODEL_SCORE_list.rds"))

# This is a big operation and I'm not sure whether R can take it:
pred_all <- do.call("rbind", pred_list)
