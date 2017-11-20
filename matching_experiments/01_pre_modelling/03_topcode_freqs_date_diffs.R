#  ------------------------------------------------------------------------
# Top coding data as part of pre-modelling pipeline
#  ------------------------------------------------------------------------
rm(list=ls())
gc()

library(tidyverse)
source('F:/Shaun/Strongbridge/Code/strongbridge_ppp/matching_experiments/01_pre_modelling/helper_functions.R')

# globals

data_dir <-"F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/"

#lte_12 is TRUE if capping fixed 12 month cohort (lte_12 - less_than_equal_12)
lte_12 <- F
if (lte_12){
  cohort_dir <- '01_lte_12_months_train/'
  #Cap DD at 365 (only an issue whith S vars where had to set day of month to 01)
  max_dd <- 365
} else {
  cohort_dir <- '02_gt_13_months_train/'
  max_dd <- NULL
}

# Training frequencies ----------------------------------------------------

training_freq_raw <- read_rds(paste0(data_dir, cohort_dir, "01_combined_common_frequencies.rds"))

# extract variable for topcoding and label:

training_freq <- dplyr::select(training_freq_raw, label, dplyr::contains("CLAIM"))
training_freq <- as.data.frame(sapply(training_freq, function(x) { as.numeric(as.character(x)) } ))
training_freq_topcoded <- topcode_frequencies(training_freq, 0.99, label = "label")
# bind with common variables:
training_freq_combined <- data.frame(training_freq_raw[,!grepl('CLAIM', colnames(training_freq_raw))],
                                     training_freq_topcoded)

# convert gender to dummy:
training_freq_combined$GENDER <- ifelse(training_freq_combined$GENDER == "F", 1, 0)

# write out:
write_rds(training_freq_combined, paste0(data_dir, cohort_dir, "01_combined_common_freq_topcoded.rds"))


# Training date differences -----------------------------------------------
train_diffs_raw <- read_rds(paste0(data_dir, cohort_dir, "01_combined_date_differences.rds"))

# extract variables for topcoding and label:
train_dates <- dplyr::select(train_diffs_raw, label, dplyr::contains("EXP"))
train_dates_topcode <- topcode_date_diffs(input = train_dates, cap = 0.99, label = "label", max_val=max_dd) %>%
  round()

# bind with common variables:
train_dates_combined <- data.frame(train_diffs_raw[, !grepl('EXP', colnames(train_diffs_raw))],
                                   train_dates_topcode)

# round topcoded date diffs to whole numbers:
#train_dates_combined[,6:ncol(train_dates_combined)] <- round(train_dates_combined[,6:ncol(train_dates_combined)])

# write out:
write_rds(train_dates_combined, paste0(data_dir, cohort_dir, "01_combined_date_differences_topcoded.rds"))


# CREATING EX_VAL_THRSH FILES ---------------------------------------------
# find maximums of each column in the topcoded dataset and save for future reference:

train_dates_combined <- read_rds(paste0(data_dir, cohort_dir, "01_combined_date_differences_topcoded.rds"))

training_freq_combined <- read_rds(paste0(data_dir, cohort_dir, "01_combined_common_freq_topcoded.rds"))

train_dates_max <- sapply(dplyr::select(train_dates_combined, dplyr::contains('EXP')), function(x) {max(x, na.rm = TRUE)})

training_freq_max <- sapply(dplyr::select(training_freq_combined, contains('CLAIM')), function(x) { max(x, na.rm = TRUE)})

# create extreme vals config files for each dataset 

ex_freq_config <- data.frame(Variable = names(training_freq_max), 
                             Thrsh = training_freq_max)

ex_date_config <- data.frame(Variable = names(train_dates_max), 
                             Thrsh = train_dates_max)
# write out
write_csv(ex_freq_config, paste0(data_dir, cohort_dir, "ex_val_caps_freq.csv"))
write_csv(ex_date_config, paste0(data_dir, cohort_dir, "ex_val_caps_dates.csv"))

