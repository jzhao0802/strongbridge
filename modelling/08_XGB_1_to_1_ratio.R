
#  ------------------------------------------------------------------------
# TRAIN AND TEST A MODEL ON A 1:1 RATIO
#  ------------------------------------------------------------------------





#  ------------------------------------------------------------------------
# TRAINING AND TESTING ON A 1:1 RATIO
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

neg_index <- sample(1:nrow(combined[combined$subset == "train_neg",]), 1553, replace = FALSE)

negs <- combined[combined$subset == "train_neg", ] 

balanced <- rbind(combined[combined$subset == "pos", ], negs[neg_index, ] )

balanced_model <- balanced %>% select(-PATIENT_ID, -index_date, -lookback_date,
                                      -test_patient_id, -subset)

char_cols <- grep("character", sapply(balanced_model, class))

balanced_model[,char_cols] <- sapply(balanced_model[,char_cols], as.numeric)

balanced_model$label <- as.factor(balanced_model$label)

# MLR pipeline ------------------------------------------------------------

dataset <- makeClassifTask(id = "1:1 ratio", 
                           data = balanced_model,
                           target = "label", 
                           positive = 1)

# make learner:
lrn_xgb <- makeLearner(cl = "classif.xgboost", predict.type = "prob")

lrn_xgb$par.vals <- list(nrounds = 100,
                         objective = "binary:logistic")


# make resampling desc
rdesc <- makeResampleDesc(method = "CV", iters = 5, predict = "test")


res <- resample(learner = lrn_xgb, task = dataset, resampling = rdesc)


# pr curve:

pr_balanced <- perf_binned_perf_curve(pred = res$pred, bin_num = 100)

write_csv(pr_balanced$curve, paste0(results_dir, "PR_curve_1_to_1_ratio.csv"))




