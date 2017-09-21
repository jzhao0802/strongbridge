
#  ------------------------------------------------------------------------
# TRAIN AND TEST A MODEL ON A 1:1 RATIO
#  ------------------------------------------------------------------------



library(mlr)
library(tidyverse)
library(xgboost)
library(palabmod)

input_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/results/modelling/XGBoost_1_to_1_ratio/"

# data in -----------------------------------------------------------------

combined <- read_rds(paste0(input_dir, "05_combined_train_unmatched_test_capped_freq_datediff.rds"))

combined_model <- combined %>% select(-PATIENT_ID, -index_date, -lookback_date,
                                      -test_patient_id, -subset)

char_cols <- grep("character", sapply(combined_model, class))

combined_model[,char_cols] <- sapply(combined_model[,char_cols], as.numeric)

combined_model$label <- as.factor(combined_model$label)

# MLR pipeline ------------------------------------------------------------

dataset <- makeClassifTask(id = "unmatched test set", 
                           data = combined_model,
                           target = "label", 
                           positive = 1)

# make learner:
lrn_xgb <- makeLearner(cl = "classif.xgboost", predict.type = "prob")

lrn_xgb$par.vals <- list(nrounds = 100,
                         objective = "binary:logistic")

# make parameter set:
ps <- makeParamSet(
  makeNumericParam("eta", lower=0.01, upper=0.3),
  makeIntegerParam("max_depth", lower=2, upper=6),
  makeIntegerParam("min_child_weight", lower=1, upper=5),
  makeNumericParam("colsample_bytree", lower=.5, upper=1),
  makeNumericParam("subsample", lower=.5, upper=1)
)

# make resampling desc
rdesc <- makeResampleDesc(method = "CV", iters = 5, predict = "test")