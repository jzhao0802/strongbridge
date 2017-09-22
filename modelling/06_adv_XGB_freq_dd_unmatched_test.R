
#  ------------------------------------------------------------------------
# Cross validate model on unmatched test set
#  ------------------------------------------------------------------------

library(mlr)
library(tidyverse)
library(palabmod)

adv_data_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/01_XGB_with_date_differences/"

# data in -----------------------------------------------------------------

combined <- read_rds(paste0(adv_data_dir, "05_combined_train_unmatched_test_capped_freq_datediff.rds"))

# read in resampling indices (created in preliminary modelling scripts):
train_indices <- read_rds(paste0(data_dir, "train_indices.rds"))
test_indices <- read_rds(paste0(data_dir, "test_indices.rds"))


# preprocess --------------------------------------------------------------

combined_model <- combined %>% select(-PATIENT_ID, -index_date, -lookback_date,
                                      -test_patient_id, -subset)

char_cols <- grep("character", sapply(combined_model, class))

combined_model[,char_cols] <- sapply(combined_model[,char_cols], as.numeric)

combined_model$label <- as.factor(combined_model$label)

model <- read_rds("F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/01_XGB_with_date_differences/XGB_Model_adv_freq_datediffs.rds")

# MLR pipeline ------------------------------------------------------------

dataset <- makeClassifTask(id = "unmatched test set", 
                           data = combined_model,
                           target = "label", 
                           positive = 1)

# create resampling description and instance:

# make xgboost learner:
lrn_xgb <- makeLearner(cl = "classif.xgboost", predict.type = "prob")
lrn_xgb$par.vals <- list(
  nrounds = 100,
  verbose = TRUE,
  objective = "binary:logistic"
)

# create resampling instance:
rdesc <- makeResampleDesc(method = "CV", iters = 5, predict = "test")
rin <- makeResampleInstance(desc = rdesc, task = dataset)
rin$train.inds <- train_indices
rin$test.inds <- test_indices

# run CV
res <- resample(learner = lrn_xgb, 
                task = dataset, 
                resampling = rin)

# PR Curve:

pr_curve <- perf_binned_perf_curve(res$pred, bin_num = 100)

write_csv(pr_curve$curve, paste0(results_dir, "PRCurve_XGB_adv_5_fold_freq_unmatched_test_100_bins.csv"))



