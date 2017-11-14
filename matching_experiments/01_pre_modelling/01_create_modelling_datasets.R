
#  ------------------------------------------------------------------------
# Creating the feature-selected top-coded modelling training data 
# (frequencies and dates)
#  ------------------------------------------------------------------------
source('F:/Shaun/Strongbridge/Code/strongbridge_ppp/matching_experiments/pre_modelling/helper_functions.R')
library(lubridate)
library(tidyverse)
library(stats)

# Globals
raw_dir <- "F:/Projects/Strongbridge/data/raw_data_cohorts/03_Cohorts_by_variable_type_12_months/"
output_dir <- "F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/01_lte_12_months_train/"

#raw_dir <- "F:/Projects/Strongbridge/data/raw_data_cohorts/05_Cohorts_by_variable_type_13_months/"
#output_dir <- "F:/Projects/Strongbridge/data/matching_experiments/pre_modelling/02_gt_13_months_train/"
# Raw modelling data in  --------------------------------------------------

raw_neg_freq <- read_rds(paste0(raw_dir, "Neg_common_frequencies_MOD_12.rds"))
raw_pos_freq <- read_rds(paste0(raw_dir, "Pos_common_frequencies_MOD_12.rds"))
raw_neg_dates <- read_rds(paste0(raw_dir, "Neg_dates_MOD_12.rds"))
raw_pos_dates <- read_rds(paste0(raw_dir, "Pos_dates_MOD_12.rds"))

#raw_neg_freq <- read_rds(paste0(raw_dir, "Neg_common_frequencies_MOD_13.rds"))
#raw_pos_freq <- read_rds(paste0(raw_dir, "Pos_common_frequencies_MOD_13.rds"))
#raw_neg_dates <- read_rds(paste0(raw_dir, "Neg_dates_MOD_13.rds"))
#raw_pos_dates <- read_rds(paste0(raw_dir, "Pos_dates_MOD_13.rds"))

# Create combined ---------------------------------------------------------
# FREQUENCIES

combined_freq <- combine_dataframes(raw_neg_freq, raw_pos_freq)
# DATES
combined_dates <- combine_dataframes(raw_neg_dates, raw_pos_dates)
# SELECT MODELLING FEATURES -----------------------------------------------

# READ IN DATA IF HAVEN'T ALREADY

# list of features:
features <- read_csv("F:/Projects/Strongbridge/data/modelling/list_of_features.csv")

# FREQUENCIES
# columns with the features we want:
features_index_freq <- grep(paste(features$Variable_Stem, collapse = "|"), colnames(combined_freq))
combined_freq_select <- combined_freq[, features_index_freq]

# DATES
# columns with the features we want:
features_index_dates <- grep(paste(features$Variable_Stem, collapse = "|"), colnames(combined_dates))
combined_dates_select <- combined_dates[, features_index_dates]

which(sapply(combined_freq_select, class) == "character")

# Write out results -------------------------------------------------------

write_rds(combined_freq_select, paste0(output_dir, "01_combined_common_frequencies.rds"))
write_rds(combined_dates_select, paste0(output_dir, "01_combined_dates_unformatted.rds"))

