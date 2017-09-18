
#  ------------------------------------------------------------------------
# ADVANCED MODELLING - XGBOOST
#  ------------------------------------------------------------------------

library(tidyverse)
library(mlr)
library(xgboost)
library(palabmod)
# library(PRROC)

# globals -----------------------------------------------------------------

data_dir <- "F:/Projects/Strongbridge/data/modelling/"
output_data_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
results_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/"


#  ------------------------------------------------------------------------
# CREATING COMBINED DATASETS FOR TRAIN AND TEST
#  ------------------------------------------------------------------------

# Data in -----------------------------------------------------------------

train_freq <- read_rds(paste0(data_dir, "01_train_combined_common_freq_new_index_topcoded.rds"))
test_neg_freq <- read_rds(paste0(data_dir, "02_Neg_frequencies_1_to_1000_new_index_topcoded.rds"))

train_dates <- read_rds(paste0(data_dir, "01_train_combined_date_differences_new_index.rds"))
test_neg_dates <- read_rds(paste0(data_dir, "02_Neg_date_differences_1_to_1000_new_index.rds"))

# remove new index date column (accidentally left in):
train_freq$index_date <- train_freq$new_index_date
train_freq$new_index_date <- NULL

# check equality of patient IDs:
all.equal(train_freq$PATIENT_ID, train_dates$PATIENT_ID)
all.equal(test_neg_dates$PATIENT_ID, test_neg_freq$PATIENT_ID)
all.equal(test_neg_freq$lookback_date, test_neg_dates$lookback_date)

# extract common variables:
train_common <- train_freq %>% select(PATIENT_ID, test_patient_id, index_date, 
                                      lookback_date, label, AGE, GENDER)
test_common <- test_neg_freq %>% select(PATIENT_ID, test_patient_id, index_date, 
                                        lookback_date, label, AGE, GENDER)


# create combined datasets:
train_combined <- data.frame(train_common,
                             train_freq[, 8:ncol(train_freq)],
                             train_dates[, 6:ncol(train_dates)])
test_combined <- data.frame(test_common, 
                            test_neg_freq[,9:ncol(test_neg_freq)],
                            test_neg_dates[,7:ncol(test_neg_dates)])

# order columns so they are the same:
test_combined <- test_combined[order(match(colnames(test_combined), colnames(train_combined)))]
# check:
all.equal(colnames(train_combined), colnames(test_combined))

# need to exclude duplicate patient IDs from test_neg_raw:
dupes <- which(test_combined$PATIENT_ID %in% train_combined$PATIENT_ID)
test_combined <- test_combined[-dupes,]

# write out these datasets to rds:
write_rds(train_combined, paste0(output_data_dir, "03_train_freq_datediffs.rds"))
write_rds(test_combined, paste0(output_data_dir, "03_test_neg_freq_datediffs_1_1000_rm_dupes.rds"))

#  ------------------------------------------------------------------------
# MODELLING
#  ------------------------------------------------------------------------

# datasets
train_combined <- read_rds(paste0(data_dir, "03_train_freq_datediffs.rds"))
test_combined <- read_rds(paste0(data_dir, "03_test_neg_freq_datediffs_1_1000_rm_dupes.rds"))

# exclude non-modelling columns
exclude <- c("-PATIENT_ID", "-test_patient_id", "-index_date", "-lookback_date")

train_model  <- select_(train_combined, .dots = exclude)
test_model  <- select_(test_combined, .dots = exclude)

# combined dataset for modelling:
combined_model <- rbind(train_model, test_model)
combined_model$label <- as.factor(combined_model$label)

# create mlr dataset:
dataset <- makeClassifTask(id = "Strongbridge freq and datediffs",
                           data = combined_model,
                           target = "label",
                           positive = 1)

# make xgboost learner:
lrn_xgb <- makeLearner(cl = "classif.xgboost", predict.type = "prob")
lrn_xgb$par.vals <- list(
  nrounds = 100,
  verbose = TRUE,
  objective = "binary:logistic"
)

# read in resampling indices (created in preliminary modelling scripts):
train_indices <- read_rds(paste0(data_dir, "train_indices.rds"))
test_indices <- read_rds(paste0(data_dir, "test_indices.rds"))

# create resampling description and instance:

rdesc <- makeResampleDesc(method = "CV", iters = 5, predict = "test")
rin <- makeResampleInstance(desc = rdesc, task = dataset)
rin$train.inds <- train_indices
rin$test.inds <- test_indices

# create pr10 measure using palabmod:
pr10 <- perf_make_pr_measure(10, "pr10")

# perform cross validation:

res <- resample(learner = lrn_xgb, 
                task = dataset, 
                resampling = rin, 
                models = TRUE,
                measures = pr10)

write_rds(res$pred$data, paste0(results_dir, "XGB_adv_predictions_20170915.rds"))

# SINGLE MODEL ------------------------------------------------------------

train_model$label <- as.factor(train_model$label)
training_dataset <- makeClassifTask(id = "training model on all 1:50 training data",
                                    data = train_model,
                                    target = "label",
                                    positive = 1)
xgb_model <- train(learner = lrn_xgb,
                   task = training_dataset)

write_rds(xgb_model, paste0(results_dir, "XGB_Model_adv_freq_datediffs.rds"))

#  ------------------------------------------------------------------------
# MODELLING ANALYSIS
#  ------------------------------------------------------------------------

# PR CURVE ----------------------------------------------------------------

# generate pr curve from the resample:
pr_curve <- perf_binned_perf_curve(pred = res$pred, bin_num = 100)

write_csv(pr_curve$curve, paste0(results_dir, "PRCurve_XGB_adv_5_fold_freq_100_bins.csv"))

# VARIABLE IMPORTANCE -----------------------------------------------------

importance_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/variable_importance/"

# variable importance for the single model ----------------------------------
importance_model <- xgb.importance(feature_names = xgb_model$features,
                                   model = xgb_model$learner.model)
# convert to numeric in order to use in detailed xgb.importance:
train_numeric <- as.data.frame(sapply(training_dataset$env$data, function(x) { as.numeric(as.character(x)) }))

detailed_imp <- xgb.importance(feature_names = xgb_model$features,
                               model = xgb_model$learner.model, data = as.matrix(train_numeric),
                               label = train_numeric$label)

# generate variable importance for each fold of the CV:
for(i in 1:length(res$models)) {
  importance_fold <- xgb.importance(feature_names = res$models[[i]]$features,
                                    model = res$models[[i]]$learner.model)
  write_csv(importance_fold, paste0(importance_dir, "VI_XGB_freq_fold_", i, ".csv"))
}

# write out:
write_csv(importance_model, paste(importance_dir, "VI_XGB_freq_datediff_singlemodel.csv"))
write_csv(detailed_imp, paste(importance_dir, "Detailed_VI_XGB_adv_freq_datediff_singlemodel.csv"))

# MEASURES ----------------------------------------------------------------

write_csv(res$measures.test, paste0(results_dir, "PR10_XGB_adv_freq_datediffs_5foldCV.csv"))

# PLOTS -------------------------------------------------------------------
# 5 most important predictors by xgboost VI:
ggplot(train_combined, aes(x=D_2768_LAST_EXP_DT, ..density.., fill=as.factor(label))) + 
  geom_density(alpha=.3)
ggplot(train_combined, aes(x=P_99214_LAST_EXP_DT, ..density.., fill=as.factor(label))) + 
  geom_density(alpha=.3)
ggplot(train_combined, aes(x=G_797000_AVG_CLAIM_CNT, ..density.., fill=as.factor(label))) + 
  geom_density(alpha=.3)
ggplot(train_combined, aes(x=S_S44_LAST_EXP_, ..density.., fill=as.factor(label))) + 
  geom_density(alpha=.3)
ggplot(train_combined, aes(x=P_99213_LAST_EXP_DT, ..density.., fill=as.factor(label))) + 
  geom_density(alpha=.3)







