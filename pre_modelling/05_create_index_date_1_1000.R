
#  ------------------------------------------------------------------------
# Create index date for the negatives using the maximum last exposure date
# from the DX, RX, PR and SP files.
#  ------------------------------------------------------------------------

library(tidyverse)
library(plyr)
library(lubridate)

raw_dir <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/1_to_1000_dataset/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# common vars:
PA <- read_rds(paste0(raw_dir, "Neg_PA.rds"))

# DATES

DX_date <- read_rds(paste0(raw_dir, "Neg_DX_dates.rds"))
RX_date <- read_rds(paste0(raw_dir, "Neg_RX_dates.rds"))
PR_date <- read_rds(paste0(raw_dir, "Neg_PR_dates.rds"))
SP_date <- read_rds(paste0(raw_dir, "Neg_SP_dates.rds"))

# Extract new index date:

DX_date <- DX_date %>% select(PATIENT_ID, contains("_LAST_"))
RX_date <- RX_date %>% select(PATIENT_ID, contains("_LAST_"))
PR_date <- PR_date %>% select(PATIENT_ID, contains("_LAST_"))
SP_date <- SP_date %>% select(PATIENT_ID, contains("_LAST_"))

# Convert to dates;
DX_format <- date_format(input_data = DX_date, date_pattern = "EXP",
                         PATIENT_ID_col = "PATIENT_ID")
RX_format <- date_format(input_data = RX_date, date_pattern = "EXP",
                         PATIENT_ID_col = "PATIENT_ID")
PR_format <- date_format(input_data = PR_date, date_pattern = "EXP",
                         PATIENT_ID_col = "PATIENT_ID")

SP_affix<- cbind(SP_date$PATIENT_ID,
            as.data.frame(sapply(SP_date[,2:ncol(SP_date)], function(x) { ifelse(is.na(x), NA, paste0(x, "01")) })))

colnames(SP_affix)[1] <- "PATIENT_ID"

SP_format <- date_format_S(input_data = SP_affix, date_pattern = "EXP",
                         PATIENT_ID_col = "PATIENT_ID")

rm(DX_date, RX_date, PR_date, SP_date)
gc()

# Find max date per row:

DX_max <- apply(DX_format[,-1], 1, function(x) {max(x, na.rm = TRUE)})
RX_max <- apply(RX_format[,-1], 1, function(x) {max(x, na.rm = TRUE)})
PR_max <- apply(PR_format[,-1], 1, function(x) {max(x, na.rm = TRUE)})
SP_max <- apply(SP_format[,-1], 1, function(x) {max(x, na.rm = TRUE)})

DX_df <- data.frame(PATIENT_ID = DX_format$PATIENT_ID,
                    DX_max = DX_max)
RX_df <- data.frame(PATIENT_ID = RX_format$PATIENT_ID,
                    RX_max = RX_max)
PR_df <- data.frame(PATIENT_ID = PR_format$PATIENT_ID,
                    PR_max = PR_max)
SP_df <- data.frame(PATIENT_ID = SP_format$PATIENT_ID,
                    SP_max = SP_max)

rm(DX_format, RX_format, PR_format, SP_format)
gc()

# Join to PATIENT_ID from PA file:
PA_ID <- data.frame(PATIENT_ID = PA$PATIENT_ID)
All_date <- join_all(list(PA_ID, DX_df, RX_df, PR_df, SP_df), by = "PATIENT_ID", type = "left")
All_date_form <- All_date
All_date_form[,2:5] <- as.data.frame(lapply(All_date_form[,2:5], ymd))

All_date_max <- apply(All_date_form[,-1], 1, function(x) {max(x, na.rm = TRUE)})

All_max_df <- data.frame(PATIENT_ID = PA_ID$PATIENT_ID, old_index_date = (PA$index_date), 
                         new_index_date = (All_date_max))

All_max_df$old_index_date <- mdy(All_max_df$old_index_date)
All_max_df$new_index_date <- ymd(All_max_df$new_index_date)

# see if this works. Trying to impute missing values for the new index date with
# the old index date. Check it gives us dates not numbers. Success.
missing_indexes <- which(is.na(All_max_df$new_index_date))
All_max_df$new_index_date[missing_indexes] <- All_max_df$old_index_date[missing_indexes]

All_max_df$difference <- as.numeric(All_max_df$old_index_date - All_max_df$new_index_date)

write_rds(All_max_df, paste0(output_dir, "New_index_date_1_1000.rds"))

# look at distribution of index date differences above 1 month:
summary(All_max_df$difference[All_max_df$difference > 31])

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

x_date <- as.data.frame(lapply(x, function(y) { ymd(y)}))

