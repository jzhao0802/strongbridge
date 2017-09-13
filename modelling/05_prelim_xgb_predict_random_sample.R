
#  ------------------------------------------------------------------------
# PRELIMINARY MODELLING: RUNNING MODEL ON RANDOM SAMPLE FROM SCORING SAMPLE
#  ------------------------------------------------------------------------

data_dir <- "F:/Projects/Strongbridge/data/modelling/"
model_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_preliminary/"
results_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_preliminary/"



# DATA IN -----------------------------------------------------------------

scoring <- read_rds(paste0(data_dir, "03_random_scoring_freq_topcoded.rds"))

# MODEL IN ----------------------------------------------------------------

xgb_model <- read_rds(paste0(model_dir, "XGB_preliminary_model.rds"))

# PR CURVE IN -------------------------------------------------------------

pr_curve <- read_csv(paste0(model_dir, "PRCurve_XGB_5_fold_freq_100_bins_scaled_1_50000.csv"))

# EXCLUDE FEATURES NOT IN MODEL -------------------------------------------

setdiff(colnames(scoring), xgb_model$features)

model_data <- scoring
model_data$PATIENT_ID <- NULL
model_data$index_date <- NULL
model_data$lookback_date <- NULL

all.equal(xgb_model$features, colnames(model_data))

# USE MODEL TO PREDICT ----------------------------------------------------

pred <- predict(xgb_model, newdata = model_data)

pred$data$PATIENT_ID <- scoring$PATIENT_ID

# MAP COUNTS OF PATIENTS ABOVE EACH RISK SCORE ----------------------------

counts <- sapply(pr_curve$thresh, function(x) { length(pred$data$prob.1[pred$data$prob.1 >= x]) })
counts_df <- data.frame(counts_sample_scoring_cohort = counts)

expected_counts <- data.frame( Expected_count_50_million = round(counts*(50000000/nrow(model_data))) )


# GET TOP 10 HIGH RISK PATIENTS -------------------------------------------

pred_order <- BBmisc::sortByCol(pred$data, "prob.1", asc = FALSE)

top_10 <- pred_order[1:10,]

top_10_patients <- left_join(top_10, scoring, by = "PATIENT_ID")

# Write out ---------------------------------------------------------------
write_rds(pred$data, paste0(results_dir, "XGB_prelim_sample_scoring_cohort_predictions.rds"))
write_csv(counts_df, paste0(results_dir, "XGB_prelim_scoring_counts_from_1886364_patients.csv"))
write_csv(expected_counts, paste0(results_dir, "XGB_prelim_expected_counts_50_million.csv"))
write_csv(top_10_patients, paste0(results_dir, "XGB_prelim_sample_scoring_top_10_patient_profiles.csv"))
