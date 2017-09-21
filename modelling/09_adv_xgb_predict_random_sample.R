
#  ------------------------------------------------------------------------
# PREDICT SAMPLE FROM SCORING COHORT USING OPTIMAL HP MODEL
#  ------------------------------------------------------------------------

library(mlr)
library(tidyverse)
library(xgboost)
library(palabmod)

input_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"
dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/"

# data in -----------------------------------------------------------------

score_raw <- read_rds(paste0(dir, "05_unmatched_neg_capped_freq_datediff.rds"))

model <- read_rds("F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/xgb_model_optimal_HP.rds")

pr_curve <- read_csv(paste0(results_dir, "PR_curve_opt_HP_unmatched.csv"))

# extract modelling columns and put in correct order ----------------------

score_model <- select(score_raw, -PATIENT_ID, -index_date, -lookback_date, -label)

score_model <- score_model[order(match(colnames(score_model), model$features))]

all.equal(colnames(score_model), model$features)

char_cols <- grep("character", sapply(score_model, class))

score_model[,char_cols] <- sapply(score_model[,char_cols], as.numeric)


# feed data through model -------------------------------------------------

pred_ <- predict(object = model, newdata = score_model)

pred_data <- pred$data

pred_data$PATIENT_ID <- score_raw$PATIENT_ID

counts <- sapply(pr_curve$thresh, function(x) { length(pred_data$prob.1[pred_data$prob.1 >= x]) })
score_model$prob.1 <- pred$data$prob.1

no_brainers <- sapply(pr_curve$thresh, function(x) { length(score_model$prob.1[score_model$prob.1 >= x & score_model$D_2768_AVG_CLAIM_CNT > 0 & score_model$G_371000_AVG_CLAIM_CNT > 0])})

counts_df <- data.frame(counts_sample_scoring_cohort = counts,
                        patients_with_HYPP_and_CAIs = no_brainers,
                        prop_HYPP_and_CAI = no_brainers/counts
                        )

# GET TOP 10 HIGH RISK PATIENTS -------------------------------------------

pred_order <- BBmisc::sortByCol(pred_data, "prob.1", asc = FALSE)

top_10 <- pred_order[1:10,]

top_10_patients <- left_join(top_10, score_raw, by = "PATIENT_ID")

# Write out ---------------------------------------------------------------

write_csv(top_10_patients, paste0(results_dir, "XGB_adv_sample_scoring_top_10_patient_profiles.csv"))

write_csv(counts_df, paste0(results_dir, "predict_score_sample_counts_and_no_brainers.csv"))

write_rds(pred_data, paste0(results_dir, "score_sample_predict.rds"))





