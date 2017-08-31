
#  ------------------------------------------------------------------------
# Create date difference variables for modelling dataset
#  ------------------------------------------------------------------------

library(lubridate)
library(tidyverse)


# Globals -----------------------------------------------------------------
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# Date in -----------------------------------------------------------------

dates_unform <- read_rds(paste0(data_dir, "01_train_combined_dates_unformatted.rds"))

######### INDEX DATE ISSUE:
Neg_PA <- read_csv("F:/Projects/Strongbridge/data/Cohorts/00_Raw_data_pull/Strongbridge_Neg_PA.csv")
# 61 unique index dates

Pos_PA <- read_csv("F:/Projects/Strongbridge/data/Cohorts/00_Raw_data_pull/Strongbridge_Pos_PA.csv")
# 1436 unique index dates

Neg_1000_PA <- read_csv("F:/Projects/Strongbridge/data/Cohorts/00_Raw_data_pull/Strongbridge_Neg_PA_C06_1000.csv")
# 61 unique index dates

length(unique(Neg_1000_PA$index_date))
##############################

# Format dates ------------------------------------------------------------

# deal with the 'S_' variables that are in a different format:
# I'm setting at these dates to the first of the month.
S_vars <- dplyr::select(dates_unform, dplyr::starts_with("S_"))
# S_vars_format <- as.data.frame(sapply(S_vars, function(x) { paste0(x, "01") }))
S_vars_format <- as.data.frame(sapply(S_vars, function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))

S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))

# add index date column;
S_vars_dates$index_date <- mdy(dates_unform$index_date)

# convert to yearmonths:
S_vars_yearmon <- as.data.frame(sapply(S_vars_dates, as.yearmon))

# # add in label column:
# S_vars_dates$label <- dates_unform$label
# # for positives, FIRST_EXP set to last day of month
# first_cols <- grep("FIRST", colnames(S_vars_dates))
# S_vars_dates[S_vars_dates$label == 1,first_cols] <- lapply(S_vars_dates[S_vars_dates$label == 1, first_cols], function(x)
#   { ceiling_date(x, unit = "month")- 1 })
# # for negatives, LAST_EXP set to last day of month
# last_cols <- grep("LAST", colnames(S_vars_dates))
# S_vars_dates[S_vars_dates$label == 0,last_cols] <- lapply(S_vars_dates[S_vars_dates$label == 0, last_cols], function(x)
# { ceiling_date(x, unit = "month")- 1 })
# 
# S_vars_dates$label <- NULL

# create date differences for these variables:

S_date_diffs <- as.data.frame(sapply(select(S_vars_yearmon, -index_date),
                                     function(x) {(S_vars_yearmon$index_date - x)*12}))

# convert 'D' G' and 'P' variables to correct format
dates_form <-  date_format(input_data = dates_unform,
                            date_pattern = "_EXP_DT",
                            PATIENT_ID_col = "PATIENT_ID")


# add index date column for creation of date diffs
dates_form$index_date <- mdy(dates_unform$index_date)

# create date difference columns
date_differences <- create_date_diffs(input = dates_form[,2:ncol(dates_form)],
                                      index_col = "index_date")

# add necessary columns
date_diffs_combined <- data.frame(dates_unform[,1:5],
                                  date_differences,
                                  S_date_diffs)

write_rds(date_diffs_combined, paste0(output_dir, "01_train_combined_date_differences.rds"))

# DEAL WITH PROBLEM WITH POSITIVE SPECIALITY DATES
# IF S_XXX_LAST > LOOKBACK then ASSIGN IT THE VALUE OF S_XXX_FIRST
# date_diffs_combined <- read_rds(paste0(output_dir, "01_train_combined_date_differences.rds"))
# 
# date_diffs_combined$lookback_length <- mdy(date_diffs_combined$index_date) - mdy(date_diffs_combined$lookback_date)
# 
# date_diffs <- date_diffs_combined[, 6:(ncol(date_diffs_combined) - 1)]
# 
# # if date difference is greater than lookback then set them equal to lookback. 
# # if datae difference is less than 0, set to 0.
# date_diffs_fixed <- as.data.frame(sapply(date_diffs, function(x) { 
#   ifelse(x > date_diffs_combined$lookback_length, date_diffs_combined$lookback_length, 
#          ifelse(x < 0, 0, x))
#   }))
# 
# date_diffs_combined <- cbind(date_diffs_combined[,1:5], date_diffs_fixed)

# date_diffs_combined$S_S83_FIRST_EXP[date_diffs_combined$PATIENT_ID == "53622404"]
# date_diffs$S_S83_FIRST_EXP[45]
# 
# date_diffs_combined$S_S83_LAST_EXP_[date_diffs_combined$PATIENT_ID == "53622404"]
# date_diffs$S_S83_LAST_EXP_[45]

# NOTE: I have not now done any re-arranging of first and last dates for 
# speciality variables:
# write out to csv


# sapply(date_diffs, function(x) { length(na.omit(x[x>date_diffs_combined$lookback_length]))})
# sapply(date_diffs_fixed, function(x) { length(na.omit(x[x>date_diffs_combined$lookback_length]))})
# sapply(date_diffs, function(x) { length(na.omit(x[x<0]))})
# sapply(date_diffs_fixed, function(x) { length(na.omit(x[x<0]))})
# 
# date_diffs$label <- date_diffs_combined$label
# date_diffs$PATIENT_ID <- date_diffs_combined$PATIENT_ID
# sapply(date_diffs[date_diffs$label == 0,], function(x) { length(na.omit(x[x == 0]))})

# S_vars <- select(date_diffs_combined, starts_with("S"))
# 
# first <- grep("FIRST", colnames(S_vars))
# 
# last <-  grep("LAST", colnames(S_vars))

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
