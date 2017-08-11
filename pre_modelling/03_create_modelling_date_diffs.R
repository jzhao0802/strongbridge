
#  ------------------------------------------------------------------------
# Create date difference variables for modelling dataset
#  ------------------------------------------------------------------------

library(lubridate)
library(tidyverse)


# Globals -----------------------------------------------------------------
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# Date in -----------------------------------------------------------------

dates_unform <- read_rds(paste0(data_dir, "01_combined_dates_unformatted.rds"))


# Format dates ------------------------------------------------------------

# deal with the 'S_' variables that are in a different format:
# I'm setting at these dates to the first of the month.
S_vars <- dplyr::select(dates_unform, dplyr::starts_with("S_"))
S_vars_format <- as.data.frame(sapply(S_vars, function(x) { paste0(x, "01") }))
S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))

# convert 'D' G' and 'P' variables to correct format
dates_form <-  date_format(input_data = dates_unform,
                            date_pattern = "_EXP_DT",
                            PATIENT_ID_col = "PATIENT_ID")

# bind the two together
dates_all <- cbind(dates_form, S_vars_dates)

# add index date column for creation of date diffs
dates_all$index_date <- mdy(dates_unform$index_date)

# create date difference columns
date_differences <- create_date_diffs(input = dates_all,
                                      index_col = "index_date")

# add necessary columns
date_differences$PATIENT_ID <- dates_unform$PATIENT_ID
date_differences$lookback_date <- mdy(dates_unform$lookback_date)
date_differences$label <- dates_unform$label
date_differences$index_date <- dates_all$index_date

# write out to csv
write_rds(date_differences, paste0(output_dir, "01_combined_date_differences.rds"))

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
