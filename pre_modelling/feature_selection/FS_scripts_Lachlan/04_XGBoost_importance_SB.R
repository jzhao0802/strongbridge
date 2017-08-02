
#  ------------------------------------------------------------------------
# RUNNING XGBOOST ON FREQUENCY DATA TO EXTRACT VARIABLE IMPORTANCE
#  ------------------------------------------------------------------------
library(mrl)
library(tidyverse)
library(stringr)
library(palabmod)
library(xgboost)


# DATA IN -----------------------------------------------------------------

Pos_freq <- read_rds(paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/",
                                                  "Pos_common_frequencies_FS",
                                                  ".rds"))
Neg_freq <- read_rds(paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/",
                            "Neg_common_frequencies_FS",
                            ".rds"))

# PREMODELLING ------------------------------------------------------------
# MANIPULATION 
Neg_freq$test_patient_id <- NULL
# Order variables so the datasets can be bound
setdiff(colnames(Pos_freq), colnames(Neg_freq))
Pos_order <- Pos_freq[order(match(colnames(Pos_freq), colnames(Neg_freq)))]
all.equal(colnames(Pos_order), colnames(Neg_freq))

# convert all freq variables to numeric:
Neg_freq[,8:ncol(Neg_freq)] <- sapply(Neg_freq[,8:ncol(Neg_freq)], as.numeric)
Pos_order[,8:ncol(Pos_order)] <- sapply(Pos_order[,8:ncol(Pos_order)], as.numeric)
combined_data <- bind_rows(Neg_freq, Pos_order)

# create gender dummy
combined_data$gender_dum <- ifelse(combined_data$GENDER == "F", 1, 0)
# remove non-modelling variables:
modelling_data <- combined_data
modelling_data$PATIENT_ID <- NULL
modelling_data$GENDER <- NULL
modelling_data$index_date <- NULL
modelling_data$lookback_date <- NULL
# D_3592 is the dirty PPP code. Therefore it shouldn't be included in the
# modelling data. Nor should lookback in days
modelling_data$D_3592_AVG_CLAIM_CNT <- NULL
modelling_data$lookback_days <- NULL
modelling_data$label <- as.factor(modelling_data$label)

# Other variable manipulation (e.g. subsetting by variable importance)

# SELECTING ONLY THE TOP 300 VARIABLES BY XGBOOST IMPORTANCE :
features_index <- c(which(colnames(modelling_data) == "label"), 
              which(colnames(modelling_data) %in% importance$Feature[1:1000]))
modelling_data_subset <- modelling_data
colnames(modelling_data_subset)
# MODELLING ---------------------------------------------------------------

table(modelling_data_subset$label)
# Make modelling dataset
model_data <- makeClassifTask(id = "Feature_selection", 
                              data = modelling_data_subset, 
                              target = "label",
                              positive = 1)

lrn_xgb <- makeLearner("classif.xgboost",
                       predict.type = "prob")

lrn_xgb$par.vals = list(
  nrounds = 100,
  verbose = TRUE,
  objective = "binary:logistic"
)

rdesc <- makeResampleDesc(method = "CV", iters = 5)

resam <- resample(lrn_xgb, model_data, resampling = rdesc)


pr_resam <- palabmod::perf_binned_perf_curve(resam$pred,
                                             bin_num = 20,
                                             x_metric = "rec",
                                             y_metric = "prec",
                                             agg_func = mean)

write_csv(pr_resam$curve, "F:/Projects/Strongbridge/results/feature_selection/01_XGBoost_freq_top__predictors.csv")

xgb_model <- train(learner = lrn_xgb, task = model_data)

detailed_importance <- xgb.importance(model = xgb_model$learner.model, feature_names = colnames(model_data$env$data),
                             data = as.matrix(model_data$env$data), label = model_data$env$data$label)

write_csv(importance, "F:/Projects/Strongbridge/results/feature_selection/01_XGBoost_importance.csv")




