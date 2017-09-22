
#  ------------------------------------------------------------------------
# Create index date for the negatives using the maximum last exposure date.
#  ------------------------------------------------------------------------

library(tidyverse)
library(plyr)
library(lubridate)
library(zoo)

raw_dir <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/1_to_50_dataset/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# DATA IN -----------------------------------------------------------------

Neg_dates <- read_rds(paste0(raw_dir, "Neg_dates.rds"))

# select last exposure dates:
Neg_last <- select(Neg_dates, PATIENT_ID, contains("LAST"))

# select specialities:
Last_S <- select(Neg_last, PATIENT_ID, starts_with("S_"))

# select non-specialities:
Last_non_S <- select(Neg_last, PATIENT_ID, ends_with("DT"))

# add "01" to Speciality variables
Last_S[,-1] <- as.data.frame(sapply(Last_S[,-1], function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))

# FORMAT DATES:
Last_S_format <- date_format_S(Last_S, date_pattern = "EXP", PATIENT_ID_col = "PATIENT_ID")
Last_non_S_format <- date_format(Last_non_S, date_pattern = "EXP", PATIENT_ID_col = "PATIENT_ID")

# combine S and non_S together to give complete formatted dataset:
Last_combined <- data.frame(Last_non_S_format, Last_S_format[,-1])

# Find max of Last column:
Last_max <- apply(Last_combined[,-1], 1, function(x) { max(x, na.rm = TRUE)})

All_max_df <- data.frame(PATIENT_ID = Last_combined$PATIENT_ID, 
                          old_index_date = mdy(Neg_dates$index_date),
                          new_index_date = ymd(Last_max))

# Impute missing values for the new index date with
# the old index date.
missing_indexes <- which(is.na(All_max_df$new_index_date))
All_max_df$new_index_date[missing_indexes] <- All_max_df$old_index_date[missing_indexes]

# If new index date is not in same year and month as old index date, then use old index date.
different_yearmon <- which(!(as.yearmon(All_max_df$new_index_date) == as.yearmon(All_max_df$old_index_date)))
All_max_df$new_index_date[different_yearmon] <- All_max_df$old_index_date[different_yearmon]

All_max_df$difference <- as.numeric(All_max_df$old_index_date - All_max_df$new_index_date)

write_rds(All_max_df, paste0(output_dir, "New_index_date_1_50.rds"))

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

# convert S dates:
date_format_S <- function(input_data, date_pattern, PATIENT_ID_col) {
  date_data <- dplyr::select(input_data, dplyr::contains(date_pattern))
  formatted <- lapply(date_data, ymd)
  df_date <- as.data.frame(formatted)
  df <- data.frame(PATIENT_ID = input_data[PATIENT_ID_col],
                   df_date
  )
  return(df)
}
