
#  ------------------------------------------------------------------------
#  Hyperparameter grid search with date differences
#  ------------------------------------------------------------------------

library(mlr)
library(tidyverse)
library(xgboost)
library(palabmod)

input_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/"

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
rin <- makeResampleInstance(desc = rdesc, task = dataset)
# # read in pre-created indices
train_indices <- read_rds(paste0(dir, "train_indices.rds"))
test_indices <- read_rds(paste0(dir, "test_indices.rds"))
# 
rin$train.inds <- train_indices
rin$test.inds <- test_indices

# make random search iteration:
ctrl <- makeTuneControlRandom(maxit = 200)

# Define performane metrics
pr20 <- perf_make_pr_measure(recall_perc = 20, "pr20")
m2 <- auc

# tune parameters:
#res <- tuneParams(learner = lrn_xgb, task = dataset, resampling = rin,
                 # par.set = ps, control = ctrl, measures = list(pr20, m2))

res_unmatched <- tuneParams(learner = lrn_xgb, task = dataset, resampling = rin,
                  par.set = ps, control = ctrl, measures = list(pr20, m2))

# Then extract best parameters and setHyperParams. Then resample
write_rds(res_unmatched, paste0(input_dir, "resample_unmatched_HP_random_search_object.rds"))

write_rds(res, paste0(input_dir, "resample_matched_HP_random_search_object.rds"))


# Cross validate on optimal hyperparameters -------------------------------

lrn_xgb_opt <- setHyperPars(lrn_xgb, par.vals = c(res$x, nrounds = 100))

# run CV
res_opt <- resample(learner = lrn_xgb_opt, 
                task = dataset, 
                resampling = rin)

pr_curve_opt <- perf_binned_perf_curve(pred = res_opt$pred, bin_num = 100)

write_csv(pr_curve_opt$curve, paste0(results_dir, "PR_curve_opt_HP_unmatched.csv"))

# Train single model on unmatched data ------------------------------------

train_data <- combined_model[combined$subset != "test_neg", ]

train_dataset <- makeClassifTask(id = "training HP optimal model", 
                                 data = train_data,
                                 target = "label", 
                                 positive = 1)

xgb_model <- train(learner = lrn_xgb_opt, task = train_dataset)

write_rds(xgb_model, paste0(results_dir, "xgb_model_optimal_HP.rds"))

# VARIABLE IMPORTANCE -----------------------------------------------------

# variable importance for the single model ----------------------------------
importance_model <- xgb.importance(feature_names = xgb_model$features,
                                   model = xgb_model$learner.model)
# convert to numeric in order to use in detailed xgb.importance:
train_numeric <- as.data.frame(sapply(train_dataset$env$data, function(x) { as.numeric(as.character(x)) }))

detailed_imp <- xgb.importance(feature_names = xgb_model$features,
                               model = xgb_model$learner.model, data = as.matrix(train_numeric),
                               label = train_numeric$label)

# write out:
write_csv(importance_model, paste(results_dir, "VI_XGB_optimal_HP_singlemodel.csv"))
write_csv(detailed_imp, paste(results_dir, "Detailed_VI_XGB_optimal_HP__singlemodel.csv"))


