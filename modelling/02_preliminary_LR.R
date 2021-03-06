
#  ------------------------------------------------------------------------
# PRELIMINARY MODELLING - LOGISTIC REGRESSION FREQUENCIES ONLY
#  ------------------------------------------------------------------------

library(tidyverse)
library(mlr)
library(palabmod)

# globals -----------------------------------------------------------------

data_dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/results/modelling/LR_preliminary/"

# Data in -----------------------------------------------------------------

train_raw <- read_rds(paste0(data_dir, "01_train_combined_common_freq_topcoded.rds"))
test_neg_raw <- read_rds(paste0(data_dir, "02_Neg_frequencies_1_to_1000_topcoded.rds")) 

# need to exclude duplicate patient IDs from test_neg_raw:
dupes <- which(test_neg_raw$PATIENT_ID %in% train_raw$PATIENT_ID)
test_neg_no_dupes <- test_neg_raw[-dupes,]


#  ------------------------------------------------------------------------
# PREPROCESS
#  ------------------------------------------------------------------------

# exclude columns not included in modelling, leaving patient IDs in for now:
exclude_train <- c("-index_date", "-lookback_date")
exclude_test <- c("-index_date", "-lookback_date",
                  "-lookback_days")

# remove duplicates
train_model <- train_raw %>% select_(.dots = exclude_train)
test_model <- test_neg_no_dupes %>% select_(.dots = exclude_test)

# add subset column for indexing the cross validation
train_model$subset <- ifelse(train_model$label == 1, "pos", "train_neg")
test_model$subset <- "test_neg"

# order columns so they are the same:
test_model <- test_model[order(match(colnames(test_model), colnames(train_model)))]

# check column names are in the same order:
all.equal(colnames(train_model), colnames(test_model))

# combine data
combined_data <- bind_rows(train_model, test_model)

# change label to a factor:
combined_data$label <- as.factor(combined_data$label)

# remove subset and patient IDs to define modelling data:
combined_model <- select(combined_data, -subset, -PATIENT_ID, -test_patient_id)


#  ------------------------------------------------------------------------
# MODELLING
#  ------------------------------------------------------------------------

# create mlr dataset:
dataset <- makeClassifTask(id = "Prelim_strongbridge",
                           data = combined_model,
                           target = "label",
                           positive = 1)

# make learner
lrn_lr <- makeLearner(cl = "classif.logreg", predict.type = "prob")



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

# create a set of K bins:
# remainder <- nrow(pos_patients) - nrow(pos_patients) %% K
bins <- rep(1:K, nrow(pos_patients)/K)
pos_groups <- split(pos_patients[pos_randomise,], bins)

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
  test_neg[[i]] <- neg_test$PATIENT_ID[neg_test$test_patient_id %in% pos_groups[[i]]$PATIENT_ID]
  
  # extract positive patients NOT in set i for training:
  train_pos <- do.call("rbind", pos_groups[-i])
  
  # combine positives and negatives:
  train_combined[[i]] <- c(train_pos$PATIENT_ID, train_neg[[i]])
  test_combined[[i]] <- c(pos_groups[[i]]$PATIENT_ID, test_neg[[i]])
  
  # extract indices in original dataset for traininig and testing:
  train_indices[[i]] <- which(combined_data$PATIENT_ID %in% train_combined[[i]])
  test_indices[[i]] <- which(combined_data$PATIENT_ID %in% test_combined[[i]])
}

# checks
length(train_indices[[1]]) + length(train_indices[[2]]) + length(train_indices[[3]]) + length(train_indices[[4]]) + length(train_indices[[5]]) == nrow(train_model) * 4

length(test_indices[[1]]) + length(test_indices[[2]]) + length(test_indices[[3]]) + length(test_indices[[4]]) + length(test_indices[[5]]) - nrow(pos_patients) == nrow(test_model)



# RESAMPLE ----------------------------------------------------------------

# read in pre-created indices
train_indices <- read_rds(paste0(data_dir, "train_indices.rds"))
test_indices <- read_rds(paste0(data_dir, "test_indices.rds"))

rdesc <- makeResampleDesc(method = "CV", iters = 5, predict = "both")

rin <- makeResampleInstance(desc = rdesc,
                            task = dataset)

# add custom indices
rin$train.inds <- train_indices
rin$test.inds <- test_indices

pr10 <- perf_make_pr_measure(10, "pr10")

res <- resample(learner = lrn_lr,
                task = dataset,
                resampling = rin,
                measures = pr10, 
                models = TRUE)

# train single model:
train_model$label <- as.factor(train_model$label)
training_dataset <- makeClassifTask(id = "training model on all 1:50 training data",
                                    data = select(train_model, -PATIENT_ID, -test_patient_id, -subset),
                                    target = "label",
                                    positive = 1)
lr_model <- train(learner = lrn_lr,
                   task = training_dataset)

# ANALYSIS ----------------------------------------------------------------
pr_curve <- perf_binned_perf_curve(res$pred, bin_num = 100)

# write out:
write_csv(pr_curve$curve, paste0(results_dir, "PRCurve_LR_5_fold_freq_100_bins.csv"))

# extract coefficients and p values:
LR_summary <- summary(lr_model$learner.model)

LR_coeffs <- as.data.frame(LR_summary$coefficients)

write.csv(LR_coeffs, paste0(results_dir, "LR_Coefficients.csv"))
