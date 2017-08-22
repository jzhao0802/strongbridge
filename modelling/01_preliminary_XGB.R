
#  ------------------------------------------------------------------------
# MODELLING - XGBOOST FREQUENCIES ONLY
#  ------------------------------------------------------------------------

library(tidyverse)
library(mlr)
library(xgboost)


# globals -----------------------------------------------------------------

data_dir <- "F:/Projects/Strongbridge/data/modelling/"


# Data in -----------------------------------------------------------------

train_raw <- read_rds(paste0(data_dir, "01_train_combined_common_freq_topcoded.rds"))
test_neg_raw <- read_rds(paste0(data_dir, "02_Neg_frequencies_1_to_1000_topcoded.rds")) 


# Preprocess --------------------------------------------------------------

# exclude columns nt included in modelling, leaving patient IDs in for now:
exclude_train <- c("-index_date", "-lookback_date")
exclude_test <- c("-index_date", "-lookback_date",
             "-lookback_days")

train_model <- train_raw %>% select_(.dots = exclude_train)
test_model <- test_neg_raw %>% select_(.dots = exclude_test)

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

# remove subset and define modelling data:
combined_model <- select(combined_data, -subset, -PATIENT_ID, -test_patient_id)

# MODELLING ---------------------------------------------------------------

# create mlr dataset:

dataset <- makeClassifTask(id = "Prelim_strongbridge",
                           data = combined_model,
                           target = "label",
                           positive = 1)

# make learner
lrn_xgb <- makeLearner("classif.xgboost", predict.type = "prob")
lrn_xgb$par.vals <- list(
  nrounds = 100,
  verbose = TRUE,
  objective = "binary:logistic"
)

# create resampling instance:

rdesc <- makeResampleDesc(method = "CV", iters = 3)

rin <- makeResampleInstance(desc = rdesc,
                            task = dataset)

# feed the correct row numbers to rin to give a 1:50 train ratio and
# 1:1000 test ratio:

# get dataframe of patient_ids and subset:
patients <- select(combined_data, PATIENT_ID, test_patient_id, subset)
# 1: extract three cohorts:
pos_patients <- filter(patients, subset == "pos")
neg_train <- filter(patients, subset == "train_neg")
neg_test <- filter(patients, subset == "test_neg")

# 2: split positives into K groups (one for each CV fold):
pos_groups <- split()





rin$train.inds[[1]]





