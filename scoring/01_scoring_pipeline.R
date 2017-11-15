
#  ------------------------------------------------------------------------
# SCORING STRONGBRIDGE
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# SCORING PIPELINE:
# steps:
#   1. Read in data, model, PR Curve (for patient counts), and topcoding configuration.
#   2. Ensure common variables are correctly assigned
#   3. Ensure all variables in model are present in data
#   4. Select modelling variables and split data into
#     a. Common variables (PATIENT_ID, AGE, GENDER)
#     b. Frequencies
#     c. Dates
#   5. Process frequencies:
#     a. Impute NAs with 0
#     b. Topcode
#   6. Process dates:
#     a. S_ and index date: create date difference in months
#     b. D_, P_, G_ and index date: create date difference in days
#   7. Combine common, frequencies and date differeces in a single dataframe.
#   8. Run data through model:
#     a. Remove Patient_ID
#     b. Order columns to match order of features in model.
#     c. Use predict to run data through model.
#   9. Patient counts:
#     a. Join 'prob.1' to the dataset created in step 7.
#     b. For each risk score in the pr curve, compute the count of 
#     patients above that risk score.
#     c. For each risk score in the pr curve, compute the count of
#     patients above that risk score who have both HYPOPOSSEMIA and CAIs.
#   10. Extract top 10 patient profiles
#   11. Write out all results
#     
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Globals
#  ------------------------------------------------------------------------
library(tidyverse)
library(mlr)
library(stringr)
library(lubridate)
library(zoo)


for (c in 128) {

timestamp()

  # These helper functions are in on the master branch:
source("F:/Lachlan/strongbridge_ppp/scoring/00_scoring_helper_functions.R") 
  
score_dir <- "F:/Projects/Strongbridge/data/scoring_cohort_chunks/"
model_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/"
topcode_dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/results/scoring/"
  
  
chunk <- paste0("C", str_pad(c, 3, pad = "0"))

#  ------------------------------------------------------------------------
# 1. Read in data, model, PR Curve (for patient counts), and topcoding config
#  ------------------------------------------------------------------------

# #<<<<<<< HEAD
# # I've set nmax to 1000 here so that this example can be run quickly:
# score_raw <- read_csv(paste0(score_dir, "Scoring_Final_Sample_C000_UP.csv"), n_max = 1000,
# =======
score_raw <- read_csv(paste0(score_dir, "Scoring_Final_Sample_", chunk, ".csv"),
                      col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

model <- read_rds(paste0(model_dir, "xgb_model_optimal_HP.rds"))

pr_curve <- read_csv(paste0(model_dir, "PR_curve_opt_HP_unmatched.csv"))

ex_val_thrsh <- read_csv(paste0(topcode_dir, "ex_val_caps_freq.csv"))


#  ------------------------------------------------------------------------
# 2. Ensure common variables are correctly assigned (and code gender)
#  ------------------------------------------------------------------------

score_raw <- score_raw %>% rename(index_date = index_dt2, 
                                  lookback_date = lookback_dt2,
                                  AGE = age,
                                  GENDER = gender)

score_raw$GENDER <- ifelse(score_raw$GENDER == "F", 1, 0)

# # Note: for group 128, I remove patients with less than 24 months lookback:
 score_raw$lookback_date <- mdy(score_raw$lookback_date)
 score_raw$index_date <- mdy(score_raw$index_date)
 score_raw$lookback_length <- score_raw$index_date - score_raw$lookback_date
 score_raw <- score_raw %>% filter(lookback_length >= 730)
 score_raw$lookback_length <-  NULL

#  ------------------------------------------------------------------------
# 3. Ensure all variables in model are present in data and check no 
# accidental frequencies or dates are in data but not in model.
#  ------------------------------------------------------------------------

# two_way_setdiff(x = model$features, y = colnames(score_raw))

#  ------------------------------------------------------------------------
#  4. Select modelling variables and split data into:
#     a. Common variables (PATIENT_ID, AGE, GENDER)
#     b. Frequencies
#     c. Dates
#  ------------------------------------------------------------------------

# extract columns to be used in variable creation and modelling:
model_features <- c("PATIENT_ID", "AGE", "GENDER", "index_date", model$features)
score_subset <- select_(score_raw, .dots = model_features)

# common variables
common <- score_raw %>% select(PATIENT_ID, AGE, GENDER)

# frequencies
frequencies <- score_subset %>% select(contains("AVG"))

# dates and index date
dates_unform <- score_subset %>% select(index_date, contains("EXP"))

rm(score_raw)

#  ------------------------------------------------------------------------
# 5. Process frequencies:
#     a. Impute NAs with 0
#     b. Topcode
#  ------------------------------------------------------------------------

# impute:
frequencies[is.na(frequencies)] <- 0

# topcode:
for( i in 1:ncol(frequencies)){
  
  # Find variable in thrsh file:
  Thrsh_index <- grep(colnames(frequencies)[i], ex_val_thrsh$Variable)
  # Threshold corresponding to variable:
  Thrsh <- ex_val_thrsh$Thrsh[Thrsh_index]
  # Cap:
  frequencies[,i][frequencies[,i] > Thrsh] <- Thrsh
  
}


#  ------------------------------------------------------------------------
#   6. Process dates:
#     a. S_ and index date: create date difference in months
#     b. D_, P_, G_ and index date: create date difference in days
#  ------------------------------------------------------------------------

# S variables: This is pure faff so please bear with it -------------------

# select S_ variables:
S_vars <- dplyr::select(dates_unform, dplyr::starts_with("S_"))
# add an '01' to the end of each entry so that R recognises the date format:
S_vars_format <- as.data.frame(sapply(S_vars, function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))
# convert into dates:
S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))
# add index date column:
S_vars_dates$index_date <- mdy(dates_unform$index_date)
# convert to yearmon format:
S_vars_yearmon <- as.data.frame(sapply(S_vars_dates, as.yearmon))
# create date differences in months:
S_date_diffs <- as.data.frame(sapply(select(S_vars_yearmon, -index_date),
                                     function(x) {round((S_vars_yearmon$index_date - x)*12)}))
# remove temporary variables from memory:
rm(S_vars, S_vars_format, S_vars_dates, S_vars_yearmon)
gc()

# You may at this point need to write_rds S_date_diffs in order to save
# memory

# D_, G_, and P_ variables ------------------------------------------------

dates_form <- date_format(input_data = select(dates_unform, starts_with("D"),
                                         starts_with("P"),
                                         starts_with("G")),  
                          date_pattern = "EXP")
# add index date column for creation of date diffs
dates_form$index_date <- mdy(dates_unform$index_date)
# create date differences:
date_differences <- create_date_diffs(input = dates_form,
                                      index_col = "index_date")

# You may need to do this process piece by piece if memory becomes an issue:
# For example: For a 3 million row dataset you could split it row-wise into 3,
# write out each segment to CSV, and then join them up afterward.
# See the script: "07_create_unmatched_date_diffs" lines 71-89 for a demo.


#  ------------------------------------------------------------------------
#   7. Combine common, frequencies and date differeces in a single dataframe.
#  ------------------------------------------------------------------------

score_combined <- data.frame(common,
                             frequencies,
                             date_differences,
                             S_date_diffs)

# at this point we can remove everything that isn't needed for modelling:
keep <- c("pr_curve", 
          "score_combined", 
          "model", 
          "model_features",
          "model_dir",
          "results_dir", 
          "chunk")
rm(list = setdiff(ls(), keep))
gc()

#  ------------------------------------------------------------------------
#   8. Run data through model:
#     a. Remove Patient_ID
#     b. Order columns to match order of features in model.
#     c. Use predict to run data through model.
#  ------------------------------------------------------------------------

# remove PATIENT_ID:
score_model <- score_combined[,-1]

# order columns to match model features:
score_model <- score_model[order(match(colnames(score_model), model$features))]
# check:
all.equal(colnames(score_model), model$features)

# run data through model:
pred <- predict(object = model, newdata = score_model)

#  ------------------------------------------------------------------------
#   9. Patient counts:
#     a. Join 'prob.1' to the dataset created in step 7.
#     b. For each risk score in the pr curve, compute the count of 
#     patients above that risk score.
#     c. For each risk score in the pr curve, compute the count of
#     patients above that risk score who have both HYPOPOSSEMIA and CAIs.
#  ------------------------------------------------------------------------

# assign predictions data to dataframe:
pred_data <- pred$data

# add PATIENT_ID to predictions and risk score to data:
pred_data$PATIENT_ID <- score_combined$PATIENT_ID
score_model$prob.1 <- pred_data$prob.1

# compute counts of patients above each risk score
# - (I have a bias towards base R when using the apply functions)
counts <- sapply(pr_curve$thresh, function(x) { length(pred_data$prob.1[pred_data$prob.1 >= x]) })

# compute number of patients with HYPPOTASSEMIA and CAI above each risk score:
no_brainers <- sapply(pr_curve$thresh, function(x) { 
  length(score_model$prob.1[score_model$prob.1 >= x & 
                              score_model$D_2768_AVG_CLAIM_CNT > 0 & 
                              score_model$G_371000_AVG_CLAIM_CNT > 0])})

# combine into dataframe:
counts_df <- data.frame(pr_curve, 
                        counts_sample_scoring_cohort = counts,
                        patients_with_HYPP_and_CAIs = no_brainers,
                        prop_HYPP_and_CAI = no_brainers/counts
)


#  ------------------------------------------------------------------------
#   10. Extract top 10 patient profiles
#  ------------------------------------------------------------------------

# order by risk score
pred_order <- BBmisc::sortByCol(pred_data, "prob.1", asc = FALSE)

# get top 10 patients
top_10 <- pred_order[1:10,]

# join to dataset
top_10_patients <- left_join(top_10, score_combined, by = "PATIENT_ID")


#  ------------------------------------------------------------------------
#   11. Write out all results
#  ------------------------------------------------------------------------

write_rds(pred_data, paste0(results_dir, chunk, "_score_sample_pred.rds"))
write_csv(counts_df, paste0(results_dir, chunk, "_score_sample_counts.csv"))
write_rds(top_10_patients, paste0(results_dir, chunk, "_score_sample_patient_profiles.rds"))


timestamp()

rm(list = ls())
gc()

}

