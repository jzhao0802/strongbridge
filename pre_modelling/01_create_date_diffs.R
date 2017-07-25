# CREATE DATE DIFF VARIABLES ----------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
# POSITIVES ---------------------------------------------------------------
# Convert all dates to correct format:
Pos_dates <- read_rds(paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/",
                             "Pos_dates",
                             ".rds"))
# missing values:
Miss_Pos_dates <- prop_missing(Pos_dates)
colnames(Pos_dates)[Miss_Pos_dates == 1]
table(str_sub(colnames(Pos_dates), -5, -1))
Pos_dt_exp <-  select(Pos_dates, ends_with("EXP_"))


Pos_date_format <- date_format(input_data = Pos_dates, date_pattern = "EXP_DT",
                               PATIENT_ID_col = "PATIENT_ID")

Pos_date_format$looback_days <- mdy(Pos_all$final_lookback)
Pos_date_format$index_date <- mdy(Pos_all$final_index)

# check that none of these dates occur before the lookback date:
# These are actually the date difference variables:
Date_diffs_pos_df <- as.data.frame(sapply(Pos_date_format[,-1], function(x) { 
  Pos_date_format$index_date - x
}))

# NEGATIVES ---------------------------------------------------------------

Neg_dates <- read_rds(paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/",
                             "Neg_dates",
                             ".rds"))

Neg_date_format <- date_format(input_data = Neg_dates[,c(1, 4:ncol(Neg_dates))], date_pattern = "EXP_DT",
                               PATIENT_ID_col = "PATIENT_ID")
Neg_date_format$lookback_date <- mdy(Neg_dates$lookback_date)
Neg_date_format$index_date <- mdy(Neg_dates$index_date)
# check that none of these dates occur before the lookback date:
# These are actually the date difference variables:
Date_diffs_neg_df <- as.data.frame(sapply(Neg_date_format[,-1], function(x) { 
  Neg_date_format$index_date - x
  }))
min(Date_diffs_neg_df, na.rm = TRUE)

# compare the lookback date to the first and last exposure dates:
neg_lookback_diff <- as.data.frame(sapply(Neg_date_format[,-1], function(x) {
  x - Neg_date_format$looback_date
}))
min(neg_lookback_diff, na.rm = TRUE)
sapply(neg_lookback_diff, class)

# FUNCTIONS ---------------------------------------------------------------

# convert date format:
#
date_format <- function(input_data, date_pattern, PATIENT_ID_col) {
  date_data <- select(input_data, contains(date_pattern))
  formatted <- lapply(date_data, mdy)
  df_date <- as.data.frame(formatted)
  df <- data.frame(PATIENT_ID = input_data[PATIENT_ID_col],
                   df_date
  )
  return(df)
}

#  proportion missing for each variable:
prop_missing <- function(input_data) {
  
  prop_miss <- sapply(input_data, function(x) {
    
    sum(is.na(x))/nrow(input_data)
    
  })
  print(summary(prop_miss))
  return(prop_miss)
}