
#  ------------------------------------------------------------------------
# PRELIMINARY MODELLING - XGBOOST FREQUENCIES ONLY
#  ------------------------------------------------------------------------

library(dplyr)
library(mlr)
library(xgboost)
library(palabmod)
# library(PRROC)

# globals -----------------------------------------------------------------

data_dir <- "F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/"
model_data_dir <- "F:/Projects/Strongbridge/data/matching_experiments/02_modelling/"
results_dir <- "F:/Projects/Strongbridge/results/matching_experiments/modelling/"
#cohort_dir <- '02_Cohorts_by_variable_type_13_months/'
#cohort_dir <- '01_Cohorts_by_variable_type_12_months/'
cohort_dir <- '00_matched_train_unmatched_test/'

# Data in -----------------------------------------------------------------

combined_data <- readRDS(paste0(data_dir, cohort_dir, "01_combined_freq_datediff_topcoded_unmatched_test_original_index.rds")) %>%
  dplyr::filter(subset %in% c('pos', 'train_neg'))

#adv_data_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
#combined_data <- read_rds(paste0(adv_data_dir, "05_combined_train_unmatched_test_capped_freq_datediff.rds")) %>%
#  dplyr::filter(subset %in% c('pos', 'train_neg'))

#  ------------------------------------------------------------------------
# PREPROCESS
#  ------------------------------------------------------------------------


# change label to a factor:
combined_data$label <- as.factor(combined_data$label)
combined_data$PATIENT_ID <- as.factor(combined_data$PATIENT_ID)
combined_data$test_patient_id[is.na(combined_data$test_patient_id)] <- combined_data$PATIENT_ID[is.na(combined_data$test_patient_id)]

#  ------------------------------------------------------------------------
# remove subset and patient IDs to define modelling data:
matched_ids <- as.factor(combined_data$test_patient_id)
combined_model <- select(combined_data,  -PATIENT_ID, -test_patient_id, -subset, -lookback_date, -index_date)
character_cols <- which(sapply(combined_model, class) == "character")
combined_model[,character_cols] <- sapply(combined_model[,character_cols], as.numeric)
#combined_model[,c(69,125,126,127)] <- sapply(combined_model[,c(69,125,126,127)], as.numeric)
combined_model$label <- as.factor(combined_model$label)

# #### REMOVE SPECIALITY VARIABLES:
# combined_model <- combined_model %>% select(-starts_with("S"))
# train_model <- train_model %>% select(-starts_with("S"))
# ################################

#  ------------------------------------------------------------------------
# MODELLING
#  ------------------------------------------------------------------------

# create mlr dataset:
dataset <- makeClassifTask(id = "Prelim_strongbridge_matching_orig",
                           data = combined_model,
                           target = "label",
                           positive = 1,
                           blocking = matched_ids)

# make learner
lrn_xgb <- makeLearner("classif.xgboost", predict.type = "prob")
lrn_xgb$par.vals <- list(
  nrounds = 100,
  verbose = TRUE,
  objective = "binary:logistic"
)


# RESAMPLE ----------------------------------------------------------------

# read in pre-created indices
rdesc <- makeResampleDesc(method = "CV", iters = 5, predict = "test", stratify = F)
#rdesc <- makeResampleDesc(method = "CV", iters = 5, predict = "test")
rin <- makeResampleInstance(desc = rdesc,
                            task = dataset)

pr10 <- perf_make_pr_measure(10, "pr10")

# add custom indices
#rin$train.inds <- train_indices
#rin$test.inds <- test_indices



# cross validate
res <- resample(learner = lrn_xgb, 
                task = dataset,
                resampling = rin,
                measures = pr10,
                models = TRUE)

# extract test predictions and add on patient id column:
#test_pred <- res$pred$data[res$pred$data$set == "test",]
#id <- test_pred$id
#test_data <- combined_data[id,]
#all.equal(test_data$label, test_pred$truth)
# add patient id:
#test_pred$PATIENT_ID <- test_data$PATIENT_ID
# write out:
#write_rds(test_pred, paste0(results_dir, "XGB_prelim_test_predictions_unmatched_1_1000.rds"))

write_rds(res, paste0(results_dir, cohort_dir, 'XGB_adv_5_fold_freq_dd_CV.rds' ))
# SINGLE MODEL ------------------------------------------------------------
xgb_model <- train(learner = lrn_xgb,
                   task = dataset)

# write out model:
write_rds(xgb_model, paste0(results_dir, cohort_dir, "XGB_adv_5_fold_freq_dd.rds"))

#Check PR curve for unmatched negatives...
#test_data <- readRDS(paste0(data_dir, cohort_dir, "01_combined_freq_datediff_topcoded_unmatched_test_original_index.rds")) %>%
#  dplyr::filter(subset %in% c('pos', 'test_neg'))
#test_data$PATIENT_ID <- as.factor(test_data$PATIENT_ID)
#test_data$test_patient_id[is.na(test_data$test_patient_id)] <- test_data$PATIENT_ID[is.na(test_data$test_patient_id)]
#test_data$test_patient_id <- as.factor(test_data$test_patient_id)
#test_matched_ids <- test_data$test_patient_id
#character_cols <- which(sapply(test_data, class) == "character")
#test_data[,character_cols] <- sapply(test_data[,character_cols], as.numeric)
#test_data$label <- as.factor(test_data$label)
#test_dataset <-makeClassifTask(id = "test_strongbridge_matching_orig",
#                                           data = select(test_data,  -PATIENT_ID, -test_patient_id, -subset, -lookback_date, -index_date),
#                                           target = "label",
#                                           positive = 1,
#                                           blocking=test_matched_ids)
#predictions <- predict(xgb_model, task=test_dataset)


#  ------------------------------------------------------------------------
# MODELLING ANALYSIS
#  ------------------------------------------------------------------------

# PR CURVE ----------------------------------------------------------------

# generate pr curve from the resample:
pr_curve <- perf_binned_perf_curve(pred = res$pred, bin_num = 100)

# write out:
write_csv(pr_curve$curve, paste0(results_dir, cohort_dir, "PRCurve_XGB_unmatched_5_fold_freq_100_bins.csv"))

# ROCR pr curve:
#perf_vs_thresh <- generateThreshVsPerfData(res$pred, measures = list(tpr, ppv))
#plotROCCurves(perf_vs_thresh)

#write_csv(perf_vs_thresh$data, paste0(results_dir, "ROCR_PRCurve_XGB_5_fold_freq.csv"))

# MEASURES ----------------------------------------------------------------

#write_csv(res$measures.test, paste0(results_dir, "PR10_XGB_freq_5foldCV.csv"))

# VARIABLE IMPORTANCE -----------------------------------------------------

importance_dir <- paste0(results_dir, cohort_dir, 'variable_importance/')

# variable importance for the single model ----------------------------------
importance_model <- xgb.importance(feature_names = xgb_model$features,
                             model = xgb_model$learner.model)
# convert to numeric in order to use in detailed xgb.importance:
#train_numeric <- as.data.frame(sapply(training_dataset$env$data, function(x) { as.numeric(as.character(x)) }))

#detailed_imp <- xgb.importance(feature_names = xgb_model$features,
#                               model = xgb_model$learner.model, data = as.matrix(train_numeric),
#                               label = train_numeric$label)

# write out:
write_csv(importance_model, paste0(importance_dir, "VI_XGB_freq_singlemodel.csv"))
#write_csv(detailed_imp, paste(importance_dir, "Detailed_VI_XGB_freq_singlemodel.csv"))

# generate variable importance for each fold of the CV:
for(i in 1:length(res$models)) {
  importance_fold <- xgb.importance(feature_names = res$models[[i]]$features,
                                    model = res$models[[i]]$learner.model)
  write_csv(importance_fold, paste0(importance_dir, "VI_XGB_freq_dd_CV_fold_", i, ".csv"))
}


#  ------------------------------------------------------------------------


