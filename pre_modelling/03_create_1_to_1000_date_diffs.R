
#  ------------------------------------------------------------------------
# Create date difference variables for modelling dataset
#  ------------------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(stringr)
library(zoo)

# Globals -----------------------------------------------------------------
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# Date in -----------------------------------------------------------------

dates_unform <- read_rds(paste0(data_dir, "02_Neg_dates_1_to_1000.rds"))


# Format dates ------------------------------------------------------------

# deal with the 'S_' variables that are in a different format:
S_vars <- dplyr::select(dates_unform, dplyr::starts_with("S_"))

S_vars_format <- as.data.frame(sapply(S_vars, function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))

S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))

# add index date column;
S_vars_dates$index_date <- mdy(dates_unform$index_date)

# convert to yearmonths:
S_vars_yearmon <- as.data.frame(sapply(S_vars_dates, as.yearmon))

# create date differences for these variables:

S_date_diffs <- as.data.frame(sapply(select(S_vars_yearmon, -index_date),
                                     function(x) {(S_vars_yearmon$index_date - x)*12}))
write_rds(S_date_diffs, paste0(output_dir, "S_1_1000_date_differences.rds"))

# convert 'D' G' and 'P' variables to correct format
dates_form <-  date_format(input_data = dates_unform,
                           date_pattern = "_EXP_DT",
                           PATIENT_ID_col = "PATIENT_ID")


# add index date column for creation of date diffs
dates_form$index_date <- mdy(dates_unform$index_date)

# create date difference columns
date_differences_776000 <- create_date_diffs(input = dates_form[1:776000, 2:ncol(dates_form)],
                                             index_col = "index_date")
write_rds(date_differences_776000, paste(output_dir, "date_diffs_1_1000_776000.rds"))
gc()
date_differences_end <- create_date_diffs(input = dates_form[776001:nrow(dates_form), 2:ncol(dates_form)],
                                          index_col = "index_date")
write_rds(date_differences_end, paste(output_dir, "date_diffs_1_1000_end.rds"))
gc()

# JOIN THE DATA TOGETER TO MAKE A SINGLE SET OF VARIABLES -----------------
#
date_differences_776000 <- read_rds(paste(output_dir, "date_diffs_1_1000_776000.rds"))
date_differences_end <- read_rds(paste(output_dir, "date_diffs_1_1000_end.rds"))
S_date_diffs <- read_rds(paste0(output_dir, "S_1_1000_date_differences.rds"))

all.equal(colnames(date_differences_776000), colnames(date_differences_end))

date_diffs <- rbind(date_differences_776000, date_differences_end)



# add necessary columns
date_diffs_combined <- data.frame(dates_unform[,1:5],
                                  label = 0,
                                  date_diffs,
                                  S_date_diffs)

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
