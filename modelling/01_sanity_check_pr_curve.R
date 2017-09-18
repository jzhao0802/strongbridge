
#  ------------------------------------------------------------------------
# QUICK HOLDOUT SET SANITY CHECK OF CV MODEL RESULTS:
# - result: appears consistent with results from CV
#  ------------------------------------------------------------------------

library(mlr)
library(xgboost)
library(tidyverse)
library(palabmod)

data_dir <- "F:/Projects/Strongbridge/data/modelling/"

combined_data <- read_rds(paste0(data_dir, "preliminary_model_data/", "03_prelim_unmatched_combined_train_test.rds"))


# remove subset and patient IDs to define modelling data:
combined_model <- select(combined_data, -subset, -PATIENT_ID, -test_patient_id)
character_cols <- which(sapply(combined_model, class) == "character")
combined_model[,character_cols] <- sapply(combined_model[,character_cols], as.numeric)
#combined_model[,c(69,125,126,127)] <- sapply(combined_model[,c(69,125,126,127)], as.numeric)
combined_model$label <- as.factor(combined_model$label)

pos <- combined_model[combined_data$subset == "pos",]
train_neg <- combined_model[combined_data$subset == "train_neg",]
test_neg <- combined_model[combined_data$subset == "test_neg",]

train_pos_index <- sample(1:nrow(pos), 1000)
test_pos_index <- setdiff(1:nrow(pos), train_pos_index)[1:500]

train_neg_index <- sample(1:nrow(train_neg), 50000)
test_neg_index <- sample(1:nrow(test_neg), 500000)

pos_train <- pos[train_pos_index,]
pos_test <- pos[test_pos_index,]

neg_train <- train_neg[train_neg_index,]
neg_test <- test_neg[test_neg_index,]

rm(combined_model, combined_data, pos, train_neg, test_neg)

train_set <- rbind(pos_train, neg_train)
test_set <- rbind(pos_test, neg_test)

train_dataset <- makeClassifTask(data = train_set,
                                 target = "label",
                                 positive = 1)

lrn_xgb <- makeLearner(cl = "classif.xgboost", predict.type = "prob")
lrn_xgb$par.vals <- list(
  nrounds = 100,
  verbose = TRUE,
  objective = "binary:logistic"
)

xgb_model <- train(learner = lrn_xgb, task = train_dataset)

pred <- predict(object = xgb_model, newdata = test_set)

pr_curve_match <- perf_binned_perf_curve(pred)

pr_curve_match

write_csv(pr_curve_unmatch$curve, "F:/Projects/Strongbridge/results/modelling/XGBOOST_preliminary/unmatched_holdout_pr_sanity_check.csv")

write_csv(pr_curve_match$curve, "F:/Projects/Strongbridge/results/modelling/XGBOOST_preliminary/matched_holdout_pr_sanity_check.csv")
