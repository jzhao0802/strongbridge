# CREATE DATE DIFF VARIABLES ----------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)

# 1. READ IN DATA
Date_data <- read_rds(paste0("PATH",
                             "TO",
                             "FILE"))


# 2. FORMAT DATE DATA. Note this doesn't yet work for the 84 date variales in the
# procedures data because the date format is different. But it does work for the
# 3192 other date vaiables.
formatted_dates <- date_format(input_data = Date_data, date_pattern = "EXP_DT",
                               PATIENT_ID_col = "PATIENT_ID")

# 3. APPEND THE INDEX DATE COLUNM
formatted_dates$index_date <- mdy(Date_data$index_date)

# 4. CREATE THE DATE DIFFERENCES USING THE FOLLOWING FUNCTION
# first set patient_id to NULL. You will need to cbind patient id
# to the date differences after you have created them.
formatted_dates$PATIENT_ID <- NULL

date_diffs <- create_date_diffs(input = formatted_dates, index_col = "index_date")


# FUNCTIONS ---------------------------------------------------------------

# create date diff vars:

# please input only a dataframe full of dates into this function:
create_date_diffs <- function(input, index_col = "index_date") {
  
  date_cols <- input[, -which(colnames(input) == index_col)]
  
  date_diffs <- as.data.frame(sapply(date_cols, function(x) { 
    input[[index_col]] - x
    
  }))
  
  return(date_diffs)
}

# convert date format:
date_format <- function(input_data, date_pattern, PATIENT_ID_col) {
  date_data <- select(input_data, contains(date_pattern))
  formatted <- lapply(date_data, mdy)
  df_date <- as.data.frame(formatted)
  df <- data.frame(PATIENT_ID = input_data[PATIENT_ID_col],
                   df_date
  )
  return(df)
}