
#  ------------------------------------------------------------------------
# Create date difference variables for modelling dataset
#  ------------------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(zoo)

# Globals -----------------------------------------------------------------
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# Date in -----------------------------------------------------------------

dates_unform <- read_rds(paste0(data_dir, "01_train_combined_dates_unformatted_new_index.rds"))

# Format dates ------------------------------------------------------------

# deal with the 'S_' variables that are in a different format:
S_vars <- dplyr::select(dates_unform, dplyr::starts_with("S_"))

S_vars_format <- as.data.frame(sapply(S_vars, function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))

S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))

# add index date column;
S_vars_dates$index_date <- ymd(dates_unform$index_date)

# convert to yearmonths:
S_vars_yearmon <- as.data.frame(sapply(S_vars_dates, as.yearmon))

# create date differences for these variables:

S_date_diffs <- as.data.frame(sapply(select(S_vars_yearmon, -index_date),
                                     function(x) {(S_vars_yearmon$index_date - x)*12}))

# convert 'D' G' and 'P' variables to correct format
dates_form <-  date_format(input_data = dates_unform,
                            date_pattern = "_EXP_DT",
                            PATIENT_ID_col = "PATIENT_ID")


# add index date column for creation of date diffs
dates_form$index_date <- ymd(dates_unform$index_date)

# create date difference columns
date_differences <- create_date_diffs(input = dates_form[,2:ncol(dates_form)],
                                      index_col = "index_date")

# add necessary columns
date_diffs_combined <- data.frame(dates_unform[,1:5],
                                  date_differences,
                                  S_date_diffs)

# prop missing
length(date_diffs_combined[is.na(date_diffs_combined[,6:ncol(date_diffs_combined)])])/((ncol(date_diffs_combined)-6) * nrow(date_diffs_combined))

write_rds(date_diffs_combined, paste0(output_dir, "01_train_combined_date_differences_new_index.rds"))

r
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
