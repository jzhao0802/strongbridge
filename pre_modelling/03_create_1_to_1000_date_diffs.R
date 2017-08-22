
#  ------------------------------------------------------------------------
# Create date difference variables for modelling dataset
#  ------------------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(stringr)

# Globals -----------------------------------------------------------------
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# Date in -----------------------------------------------------------------

dates_unform <- read_rds(paste0(data_dir, "02_Neg_dates_1_to_1000.rds"))


# Format dates ------------------------------------------------------------

# deal with the 'S_' variables that are in a different format:
# I'm setting these dates to the first of the month for now.
S_vars <- dplyr::select(dates_unform, dplyr::starts_with("S_"))
S_vars_format <- as.data.frame(sapply(S_vars, function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))

S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))

# the dates are all on the first. We need to change variables with 'LAST' to the
# last day of the month:
last_cols <- grep("LAST", colnames(S_vars_dates))
S_vars_dates[ ,last_cols] <- lapply(S_vars_dates[ , last_cols], function(x) { ceiling_date(x, unit = "month") - 1 })


# convert 'D' G' and 'P' variables to correct format
dates_form <-  date_format(input_data = dates_unform,
                           date_pattern = "_EXP_DT",
                           PATIENT_ID_col = "PATIENT_ID")

# bind the two together
dates_all <- cbind(dates_form, S_vars_dates)

# add index date column for creation of date diffs
dates_all$index_date <- mdy(dates_unform$index_date)

# create date difference columns
date_differences_776000 <- create_date_diffs(input = dates_all[1:776000, 2:ncol(dates_all)],
                                      index_col = "index_date")
write_rds(date_differences_776000, paste(output_dir, "date_diffs_1_1000_776000.rds"))
date_differences_end <- create_date_diffs(input = dates_all[776001:nrow(dates_all), 2:ncol(dates_all)],
                                             index_col = "index_date")
write_rds(date_differences_end, paste(output_dir, "date_diffs_1_1000_end.rds"))


# JOIN THE DATA TOGETER TO MAKE A SINGLE SET OF VARIABLES -----------------
#
date_differences_776000 <- read_rds(paste(output_dir, "date_diffs_1_1000_776000.rds"))
date_differences_end <- read_rds(paste(output_dir, "date_diffs_1_1000_end.rds"))

all.equal(colnames(date_differences_776000), colnames(date_differences_end))

date_diffs_all <- rbind(date_differences_776000, date_differences_end)

# add label column

# add necessary columns
date_diffs_combined <- data.frame(dates_unform[,1:5],
                                  label = 0,
                                  date_diffs_all)

# write out to csv
write_rds(date_diffs_combined, paste0(output_dir, "02_1_to_1000_date_differences.rds"))

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
