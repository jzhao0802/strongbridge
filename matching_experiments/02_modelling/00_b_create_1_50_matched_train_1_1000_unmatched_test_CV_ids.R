#  ------------------------------------------------------------------------
# SPLIT PATIENT IDS INTO TRAINING/TESTING FOR EACH CV FOLD
#  ------------------------------------------------------------------------

library(tidyverse)
library(mlr)
library(xgboost)
library(palabmod)
# library(PRROC)

# globals -----------------------------------------------------------------

results_dir <- "F:/Projects/Strongbridge/results/matching_experiments/modelling/"
data_dir <- 'F:/Projects/Strongbridge/data/matching_experiments/02_modelling/'
cohort_dir <- '00_matched_train_unmatched_test/'
test_strat <- '1_1000_unmatched_test/'
full_data_dir <- paste0(data_dir, test_strat)


# NOTE: FOR NOW JUST CONVERT THE INDICES LACHLAN USED BACK INTO IDS TO ENSURE
# SAME INDICES WERE USED FOR BOTH PROJECT AND EXPERIMENT

#Load dataset used for project
adv_data_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
combined <- read_rds(paste0(adv_data_dir, "05_combined_train_unmatched_test_capped_freq_datediff.rds"))

#Load indices used for train/test
train_indices <- read_rds(paste0(data_dir, "train_indices.rds"))
test_indices <- read_rds(paste0(data_dir, "test_indices.rds"))

#Convert Patient ids to factor
combined$PATIENT_ID <- as.factor(combined$PATIENT_ID)

#Get patient ids for indices
train_combined <- list()
test_combined <- list()
for (i in 1:length(train_indices)){
  train_combined[[i]] <- combined$PATIENT_ID[train_indices[[i]]]
  test_combined[[i]] <- combined$PATIENT_ID[test_indices[[i]]]
  
  
}

#train_combined <- combined$PATIENT_IDS[train_indices]
#test_combined <- combined$PATIENT_IDS[test_indices]


#Save 
write_rds(list(train_ids = train_combined, test_ids = test_combined), 
          paste0(full_data_dir, "1_50_matched_train_1_1000_unmatched_test_CV_ids.rds"))

#Save dataset with training negatives removed to make it easier to load
#testing dataset in
#write_rds(combined[combined$subset %in% c('pos', 'test_neg'),], 
#          paste0(full_data_dir, "1_1000_unmatched_test.rds"))


#  ------------------------------------------------------------------------

#USE PROCEEDING CODE TO GENERATE IDS FROM SCRATCH - NOTDONE FOR MAIN EXPERIMENT
# HENCE ENCAPSULATED CODE IN if(FALSE) - NEEDS TO BE REMOVED TO GENERATE
# IDs FROM SCRATCH!!!

#  ------------------------------------------------------------------------

if (FALSE){

combined_data <- read_rds(paste0(data_dir, cohort_dir, 
                                 '01_combined_freq_datediff_topcoded_unmatched_test_original_index.rds'))
# remove subset and patient IDs to define modelling data:
combined_model <- select(combined_data, -subset, -PATIENT_ID, -test_patient_id, -lookback_date, -index_date)
character_cols <- which(sapply(combined_model, class) == "character")
combined_model[,character_cols] <- sapply(combined_model[,character_cols], as.numeric)
#combined_model[,c(69,125,126,127)] <- sapply(combined_model[,c(69,125,126,127)], as.numeric)
combined_model$label <- as.factor(combined_model$label)


# CREATE RESAMPLING INDICES -----------------------------------------------

# feed the correct row numbers to rin to give a 1:50 train ratio and
# 1:1000 test ratio:

# get dataframe of patient_ids and subset:
patients <- select(combined_data, PATIENT_ID, test_patient_id, subset)

# 1: extract three cohorts:
pos_patients <- filter(patients, subset == "pos")
neg_train <- filter(patients, subset == "train_neg")
neg_test <- filter(patients, subset == "test_neg")

# 2: split positives into K groups (one for each CV fold):
K <- 5
set.seed(123)

pos_randomise <- sample(nrow(pos_patients), nrow(pos_patients), replace = FALSE)

neg_test_randomise <- sample(nrow(neg_test), nrow(neg_test), replace = FALSE)

# create a set of K bins:
# remainder <- nrow(pos_patients) - nrow(pos_patients) %% K
bins <- rep(1:K, nrow(pos_patients)/K)
pos_groups <- split(pos_patients[pos_randomise,], bins)
neg_test_groups <- split(neg_test[neg_test_randomise,], bins)

# create a series of training and testing indices:
train_neg <- list()
test_neg <- list()
train_combined <- list()
test_combined <- list()
train_indices <- list()
test_indices <- list()

for(i in 1:length(pos_groups)) {
  # train on patients NOT in the set i:
  train_neg[[i]] <- neg_train$PATIENT_ID[!(neg_train$test_patient_id %in% pos_groups[[i]]$PATIENT_ID)]
  # test on everything in the set i:
  #test_neg[[i]] <- neg_test$PATIENT_ID[neg_test$test_patient_id %in% pos_groups[[i]]$PATIENT_ID]
  #test_neg[[i]] <- neg_test$PATIENT_ID[neg_test$PATIENT_ID %in% pos_groups[[i]]$PATIENT_ID]
  
  # extract positive patients NOT in set i for training:
  train_pos <- do.call("rbind", pos_groups[-i])
  
  # combine positives and negatives:
  train_combined[[i]] <- c(train_pos$PATIENT_ID, train_neg[[i]])
  test_combined[[i]] <- c(pos_groups[[i]]$PATIENT_ID, neg_test_groups[[i]]$PATIENT_ID)
  
  # extract indices in original dataset for traininig and testing:
  train_indices[[i]] <- which(combined_data$PATIENT_ID %in% train_combined[[i]])
  test_indices[[i]] <- which(combined_data$PATIENT_ID %in% test_combined[[i]])
}

# checks
length(train_indices[[1]]) + length(train_indices[[2]]) + length(train_indices[[3]]) + length(train_indices[[4]]) + length(train_indices[[5]]) == (nrow(pos_patients) + nrow(neg_train)) * 4

length(test_indices[[1]]) + length(test_indices[[2]]) + length(test_indices[[3]]) + length(test_indices[[4]]) + length(test_indices[[5]]) == (nrow(neg_test) + nrow(pos_patients))

#indices <- create_cv_indices(pos_patients, neg_train, neg_test = neg_test, K = 5, seed = 123, matched_train_col = 'test_patient_id')

#Save IDs and indices as indices can vary if data is not organised in same way....
write_rds(list(train_ids = train_combined, test_ids = test_combined), paste0(results_dir, "1_50_mathced_train_1_1000_unmatched_test_CV_indices.rds"))
}