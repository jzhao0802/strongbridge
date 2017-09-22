
#  ------------------------------------------------------------------------
# XGBoost with hyperparameter search for FS
#  ------------------------------------------------------------------------

library(mlr)
library(xgboost)
library(tidyverse)
library(palabmod)
library(parallel)
library(parallelMap)
library(doParallel)

# data in:
# combined_data <- read_rds("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/combined_common_frequencies.rds")

cluster_tr_data <- read_rds("F:/Projects/Strongbridge/data/feature_selection/train_clustering_rm_3592_capped_p99.rds")
cluster_ts_data <- read_rds("F:/Projects/Strongbridge/data/feature_selection/test_clustering_rm_3592_capped_p99.rds")

all.equal(colnames(cluster_tr_data), colnames(cluster_ts_data))
combined_data <- bind_rows(cluster_ts_data, cluster_tr_data)

# remove non-modelling variables:
modelling_data <- combined_data
modelling_data$test_patient_id <- NULL
modelling_data$PATIENT_ID <- NULL
modelling_data$index_date <- NULL
modelling_data$lookback_date <- NULL
# D_3592 is the dirty PPP code. Therefore it shouldn't be included in the
# modelling data. Nor should lookback in days
modelling_data$D_3592_AVG_CLAIM_CNT <- NULL
modelling_data$lookback_days <- NULL
modelling_data$test_patient_id <- NULL
modelling_data$label <- as.factor(modelling_data$label)



# MODELLING ---------------------------------------------------------------

table(modelling_data$label)
# Make modelling dataset
model_data <- makeClassifTask(id = "Feature_selection", 
                              data = modelling_data, 
                              target = "label",
                              positive = 1)

lrn_xgb <- makeLearner("classif.xgboost",
                       predict.type = "prob")

lrn_xgb$par.vals = list(
  nrounds = 100,
  verbose = TRUE,
  objective = "binary:logistic"
)

# Set up hyperparameter grid:

# making a set of parameters:
recall_thrs <- 10
random_search_iter = 50L
set.seed(123, "L'Ecuyer")

ps <- makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.3),
  makeIntegerParam("max_depth", lower = 2, upper = 6),
  makeIntegerParam("min_child_weight", lower = 1, upper = 5),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  makeNumericParam("subsample", lower = 0.5, upper = 1)
  # makeNumericParam("usw.rate", lower = 0.5, upper = 1)
)

# define a random search:
ctrl <- makeTuneControlRandom(maxit = random_search_iter,
                              tune.threshold = FALSE)

# define performance measaures (apparently only the first is used for
# assessing hyperparams but we need at least 2 for DH's get_result
# function to work.)
pr10 <- perf_make_pr_measure(recall_perc = recall_thrs,
                             "pr_10")
m2 <- auc
m_all <- list(pr10, m2) 

# resample model:
outer <- makeResampleDesc(method = "CV", iters = 3L, predict = "both")
inner <- makeResampleDesc(method = "CV", iters = 3L)

# define a wrapped learner:
lrn_wrap <- makeTuneWrapper(lrn_xgb, resampling = inner, par.set = ps,
                            control = ctrl, show.info = F,
                            measures = m_all)

# start parallelisation
parallelStartSocket(35L, level="mlr.tuneParams")
# resample
res <- resample(learner = lrn_wrap, task = model_data, resampling = outer,
                models = TRUE, extract = getTuneResult, show.info = F,
                measures = m_all)

parallelStop()


resam <- resample(lrn_xgb, model_data, resampling = rdesc)


pr_resam <- palabmod::perf_binned_perf_curve(resam$pred,
                                             bin_num = 20,
                                             x_metric = "rec",
                                             y_metric = "prec",
                                             agg_func = mean)

write_csv(pr_resam$curve, paste0(results_dir, "01_XGBoost_freq_top__predictors_ex_3592_clust.csv"))

xgb_model <- train(learner = lrn_xgb, task = model_data)

detailed_importance <- xgb.importance(model = xgb_model$learner.model, feature_names = colnames(model_data$env$data),
                                      data = as.matrix(model_data$env$data), label = model_data$env$data$label)

importance <- xgb.importance(model = xgb_model$learner.model, feature_names = xgb_model$features)

write_csv(importance, paste0(results_dir, "01_XGBoost_importance_ex_3592_clust.csv"))