
#  ------------------------------------------------------------------------
# 00_COHORT CREATION
#  ------------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(palab)
library(lubridate)
library(plyr)
# LOAD DATA ---------------------------------------------------------------

data_loc <- "F:/Projects/Strongbridge/data/fs_modelling_cohorts/Strongbridge_"

Neg_DX <- read_csv(paste0(data_loc, "Neg_DX.csv"),
                   col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_PA <- read_csv(paste0(data_loc, "Neg_PA.csv"),
                   col_types = (cols(patient_id = col_character(), .default = col_guess())))
Neg_PR <-  read_csv(paste0(data_loc, "Neg_PR.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_RX <-  read_csv(paste0(data_loc, "Neg_RX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_SP <-  read_csv(paste0(data_loc, "Neg_SP.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_DX <-  read_csv(paste0(data_loc, "Pos_DX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PA <-  read_csv(paste0(data_loc, "Pos_PA.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PR <-  read_csv(paste0(data_loc, "Pos_PR.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_RX <-  read_csv(paste0(data_loc, "Pos_RX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_SP <-  read_csv(paste0(data_loc, "Pos_SP.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))


# Left join cohorts together:
Pos_all <- join_all(list(Pos_PA, Pos_DX, Pos_RX, Pos_PR, Pos_SP), type = "left")
Neg_all <- join_all(list(Neg_PA, Neg_DX, Neg_RX, Neg_PR, Neg_SP), type = "left")

# Convert all dates to correct format:
Pos_dates <- data.frame(PATIENT_ID = Pos_all$PATIENT_ID,
                        select(Pos_all, ends_with("EXP_DT")))
Pos_dates$PATIENT_ID <- as.character(Pos_dates$PATIENT_ID)
Pos_date_format <- date_format(input_data = Pos_dates, date_pattern = "EXP_DT",
                               PATIENT_ID_col = "PATIENT_ID")

Pos_date_format$looback_days <- mdy(Pos_all$final_lookback)
Pos_date_format$index_date <- mdy(Pos_all$final_index)

# summary of lookback and index:
summary(Pos_date_format$looback_days)
summary(Pos_date_format$index_date)

# check that none of these dates occur before the lookback date:
# These are actually the date difference variables:
Date_diffs_pos_df <- as.data.frame(sapply(Pos_date_format[,-1], function(x) { 
  Pos_date_format$index_date - x
  }))
max(Date_diffs_pos_df, na.rm = TRUE)

# compare the lookback date to the first and last exposure dates:
pos_lookback_diff <- as.data.frame(sapply(Pos_date_format[,-1], function(x) {
  x - Pos_date_format$looback_days
}))
max(pos_lookback_diff, na.rm = TRUE)
# FUNCTIONS ---------------------------------------------------------------

# convert date format:
#
date_format <- function(input_data, date_pattern, PATIENT_ID_col) {
  date_data <- select(input_data, contains(date_pattern))
  formatted <- lapply(date_data, mdy)
  df_date <- as.data.frame(formatted)
  df <- data.frame(PATIENT_ID = as.character(input_data[PATIENT_ID_col]),
                   df_date
  )
  return(df)
}




