
#  ------------------------------------------------------------------------
# Preprocess unmatched negative cohort
#  ------------------------------------------------------------------------

library(tidyverse)
library(mlr)
library(zoo)
library(stringr)
library(lubridate)

temp_dd_dir <- "F:/Projects/Strongbridge/data/modelling/intermediate_date_diffs/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# read in sample from scoring cohort and frequencies:

raw_data <- read_csv("F:/Projects/Strongbridge/data/Random_sample_scoring/Scoring_Final_Sample_C000_UP.csv",
                    col_types = cols(PATIENT_ID = col_character(), .default = col_guess()))
freq <- read_rds("F:/Projects/Strongbridge/data/modelling/03_random_scoring_freq_topcoded.rds")

# matched_dates <- read_rds("F:/Projects/Strongbridge/data/modelling/01_train_combined_date_differences_new_index.rds")

unmatched <- read_rds("F:/Projects/Strongbridge/data/modelling/preliminary_model_data/02_sample_scoring_cohort_unmatched_negative_rm_duplicates.rds")


# rename age and gender to AGE and GENDER:
colnames(raw_data)[c(2,3,4,5)] <- c("lookback_date", "index_date", "AGE", "GENDER")

# extract dates from scoring--------------------------------------------------

score_dates <- raw_data %>% select(PATIENT_ID, index_date, contains("EXP"))

write_rds(score_dates, paste0(temp_dd_dir, "score_dates.rds"))
score_dates <- read_rds(paste0(temp_dd_dir, "score_dates.rds"))

# S variables -------------------------------------------------------------

# deal with the 'S_' variables that are in a different format:
S_vars <- dplyr::select(score_dates, dplyr::starts_with("S_"))

S_vars_format <- as.data.frame(sapply(S_vars, function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))
 
S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))

# add index date column;
S_vars_dates$index_date <- mdy(score_dates$index_date)

# convert to yearmonths:
S_vars_yearmon <- as.data.frame(sapply(S_vars_dates, as.yearmon))

S_date_diffs <- as.data.frame(sapply(select(S_vars_yearmon, -index_date),
                                     function(x) {round((S_vars_yearmon$index_date - x)*12)}))

write_rds(S_date_diffs, paste0(temp_dd_dir, "S_unmatched_date_differences.rds"))


# Non S variables ---------------------------------------------------------

# convert 'D' G' and 'P' variables to correct format
dates_form <-  date_format(input_data = score_dates,
                           date_pattern = "_EXP_DT",
                           PATIENT_ID_col = "PATIENT_ID")

# add index date column for creation of date diffs
dates_form$index_date <- mdy(score_dates$index_date)

# write out and read back in to save memory:
write_rds(dates_form, paste0(temp_dd_dir, "dates_form.rds"))
dates_form <- read_rds(paste0(temp_dd_dir, "dates_form.rds"))

# create date difference columns

date_differences_776000 <- create_date_diffs(input = dates_form[1:776000, 2:ncol(dates_form)],
                                             index_col = "index_date")
write_rds(date_differences_776000, paste(temp_dd_dir, "date_diffs_unmatched_776000.rds"))
rm(date_differences_776000)
gc()

date_differences_1200000 <- create_date_diffs(input = dates_form[776001:1200000, 2:ncol(dates_form)],
                                          index_col = "index_date")
write_rds(date_differences_1200000, paste(temp_dd_dir, "date_diffs_unmatched_to_1200000.rds"))
rm(date_differences_1200000)
gc()

date_differences_end <- create_date_diffs(input = dates_form[1200001:nrow(dates_form), 2:ncol(dates_form)],
                                          index_col = "index_date")
write_rds(date_differences_end, paste(temp_dd_dir, "date_diffs_unmatched_to_end.rds"))
gc()

# JOIN THE DATA TOGETER TO MAKE A SINGLE SET OF VARIABLES -----------------
#
date_differences_776000 <- read_rds(paste(temp_dd_dir, "date_diffs_unmatched_776000.rds"))
date_difference_1200000 <- read_rds(paste(temp_dd_dir, "date_diffs_unmatched_to_1200000.rds"))
date_differences_to_end <- read_rds(paste(temp_dd_dir, "date_diffs_unmatched_to_end.rds"))
S_date_diffs <- read_rds(paste0(temp_dd_dir, "S_unmatched_date_differences.rds"))

all.equal(colnames(date_differences_776000), colnames(date_difference_1200000), colnames(date_differences_to_end))

date_diffs <- rbind(date_differences_776000, date_difference_1200000, date_differences_to_end)

# add necessary columns and S variables:
date_diffs_combined <- data.frame(raw_data[,1:5],
                                  label = 0,
                                  date_diffs,
                                  S_date_diffs)

write_rds(date_diffs_combined, paste0(output_dir, "04_unmatched_neg_date_diffs.rds"))


# COMBINE FREQS AND DATE DIFFS --------------------------------------------

combined <- data.frame(freq,
                       date_diffs_combined[,7:ncol(date_diffs_combined)])

# reorder columns according to matched dataset used to train model:
matched <- read_rds(paste0(output_dir, "Advanced_model_data/", "03_train_capped_freq_datediffs.rds"))
combined_order <- combined[order(match(colnames(combined), colnames(matched)))]

write_rds(combined, paste0(output_dir, "05_unmatched_neg_capped_freq_datediff.rds"))


# FUNCTIONS ---------------------------------------------------------------

# convert date format:
date_format <- function(input_data, date_pattern, PATIENT_ID_col) {
  date_data <- dplyr::select(input_data, dplyr::contains(date_pattern))
  formatted <- lapply(date_data, mdy)
  df_date <- as.data.frame(formatted)
  df <- data.frame(PATIENT_ID = input_data[PATIENT_ID_col],
                   df_date
  )
  return(df)
}


# please input only a dataframe full of dates into this function:
create_date_diffs <- function(input, index_col = "index_date") {
  
  date_cols <- input[, -which(colnames(input) == index_col)]
  
  date_diffs <- as.data.frame(sapply(date_cols, function(x) { 
    input[[index_col]] - x
    
  }))
  
  return(date_diffs)
}
