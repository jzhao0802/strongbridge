library(palab)
library(readr)
library(dplyr)
library(mlr)
library(palabmod)
library(xgboost)

df <- read_rds('F:/Projects/Strongbridge/data/modelling/Advanced_model_data/05_combined_train_unmatched_test_capped_freq_datediff.rds')

drop <- c("-PATIENT_ID", "-test_patient_id", "-index_date", "-lookback_date")

df_num <- df %>% mutate_if(is.character, as.numeric) %>% mutate(label = as.factor(label)) %>% select_(.dots = drop)

which(sapply(df_num, class) == "character")

str(df_num)
dir <- "F:/Projects/Strongbridge/data/modelling/"
train_indices <- read_rds(paste0(dir, "train_indices.rds"))
test_indices <- read_rds(paste0(dir, "test_indices.rds"))
str(train_indices)
str(test_indices)                                                   

lapply(test_indices, function(x) { table(df_num$label[x]) })                         
lapply(train_indices, function(x) { table(df_num$label[x])})


dataset <- makeClassifTask(id = "Strongbridge adv model data", data = df_num, 
                           target = "label", positive = 1)

rdesc <- makeResampleDesc(method = "CV", iters = 5)

rin <- makeResampleInstance(desc = rdesc, task = dataset)

# supply custom indices
rin$train.inds <- train_indices
rin$test.inds <- test_indices

learner <- makeLearner(cl = "classif.xgboost", predict.type = "prob")

model_opt <- read_rds("F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/xgb_model_optimal_HP.rds") 

learner$par.vals <- model_opt$learner$par.vals

# Create list of measures ---------------------------------------------------------
ls_pr_measure <- list(perf_make_pr_measure(5, 'pr5'),
                      perf_make_pr_measure(15, 'pr15'),
                      perf_make_pr_measure(40, 'pr40'))

# Test the acc_vs_compl functionality -------------------------------------------
n_predictors <-  c(400, 200, 100, 50, 25, 10, 5)
ls_models <- plotting_acc_vs_comp(learner, dataset, rin, n_predictors, ls_pr_measure, linear_x_axis = FALSE, models = TRUE)

# extract variable stems
features <- ls_models$`100`$models[[1]]$features
ends <- paste0(c('_LAST_EXP_DT', '_FIRST_EXP_DT', '_AVG_CLAIM_CNT', "_FIRST_EXP", "_LAST_EXP_", "_AVG_CLAIM"), collapse = "|")
features <- unique(gsub(pattern = ends, replacement = "", x = features))

# write out features list
features_select <- colnames(df_num)[grep(paste(features, collapse = "|"), x = colnames(df_num))]

write_csv(as.data.frame(features_select), "F:/Projects/Strongbridge/results/post_modelling/feature_list_top_100.csv")

