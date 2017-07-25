
#  ------------------------------------------------------------------------
# RUNNING XGBOOST ON FREQUENCY DATA TO EXTRACT VARIABLE IMPORTANCE
#  ------------------------------------------------------------------------
library(mrl)
library(tidyverse)
library(stringr)
library(palabmod)


# DATA IN -----------------------------------------------------------------

Pos_freq <- read_rds(paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/",
                                                  "Pos_common_frequencies_FS",
                                                  ".rds")))
Neg_freq <- read_rds(paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/",
                            "Neg_common_frequencies_FS",
                            ".rds")))


# MANIPULATION ------------------------------------------------------------

# Order variables so the datasets can be bound
all.equal(colnames(Pos_freq), colnames(Neg_freq))


# MODELLING ---------------------------------------------------------------

# Make modelling dataset
model_data <- makeClassifTask(id = "Feature_selection", 
                              data = combined_data, 
                              target = "label",
                              positive = 1)

lrn_xgb <- makeLearner("classif.xgboost"
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







