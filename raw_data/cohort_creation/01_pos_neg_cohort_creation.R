
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

Neg_DX <- read_csv(paste0(data_loc, "Neg_DX_Update20170724.csv"),
                   col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_PA <- read_csv(paste0(data_loc, "Neg_PA.csv"),
                   col_types = (cols(patient_id = col_character(), .default = col_guess())))
Neg_PR <-  read_csv(paste0(data_loc, "Neg_PR_Update20170724.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_RX <-  read_csv(paste0(data_loc, "Neg_RX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_SP <-  read_csv(paste0(data_loc, "Neg_SP.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_DX <-  read_csv(paste0(data_loc, "Pos_DX_Update20170724.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PA <-  read_csv(paste0(data_loc, "Pos_PA.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PR <-  read_csv(paste0(data_loc, "Pos_PR_Update20170724.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_RX <-  read_csv(paste0(data_loc, "Pos_RX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_SP <-  read_csv(paste0(data_loc, "Pos_SP.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))


# COMMON VARIABLE NAMES ---------------------------------------------------

colnames(Neg_PA) <- c("PATIENT_ID", "AGE", "GENDER", "index_date", "lookback_date",
                      "lookback_days", "test_patient_id") 
colnames(Pos_PA) <- c("PATIENT_ID", "AGE", "GENDER", "index_date", "lookback_date",
                      "lookback_days")

# EXCLUDE VARIABLES WHICH ONLY FIRE IN ONE OF THE COHORTS -----------------

exclude <- setdiff(colnames(Neg_DX), colnames(Pos_DX))
Neg_DX <- Neg_DX[,-which(colnames(Neg_DX) %in% exclude)]

# Left join cohorts together:
Pos_all <- join_all(list(Pos_PA, Pos_DX, Pos_RX, Pos_PR, Pos_SP), type = "left")
Neg_all <- join_all(list(Neg_PA, Neg_DX, Neg_RX, Neg_PR, Neg_SP), type = "left")

Pos_all$label <- 1
Neg_all$label <- 0
#  ------------------------------------------------------------------------
# VARIABLE ENCODING:
table(str_sub(colnames(Neg_fre), -5, -1))
table(str_sub(colnames(Pos_all), -5, -1))
# SUMMARY:
# Date variables: _EXP_, T_EXP, XP_DT: all contain "EXP"
# Claims counts: CLAIM_CNT, 
# Frequencies:
# Flags:
# Common variables:"PATIENT_ID", "AGE", "GENDER", "index_date", "lookback_date",
# "lookback_days", "test_patient_id"
#  ------------------------------------------------------------------------

# POSITIVES
Pos_flags <- select(Pos_all, PATIENT_ID, label, ends_with("_FLAG"))
Pos_claims <- select(Pos_all, PATIENT_ID, label, ends_with("_CLAIM_CNT"), -contains("AVG"))
Pos_common_frequencies <- select(Pos_all, PATIENT_ID, label, AGE, GENDER, index_date, lookback_date,
                          lookback_days,  ends_with("AVG_CLAIM_CNT"), ends_with("CLAIM"))
Pos_dates <- select(Pos_all, PATIENT_ID, label, index_date, lookback_date,
                    contains("_EXP"))
# NEGATIVES:
Neg_flags <- select(Neg_all, PATIENT_ID, test_patient_id, label, ends_with("_FLAG"))
Neg_claims <- select(Neg_all, PATIENT_ID, test_patient_id, label, ends_with("_CLAIM_CNT"), -contains("AVG"))
Neg_common_frequencies <- select(Neg_all, PATIENT_ID, test_patient_id, label, AGE, GENDER, index_date, lookback_date,
                          lookback_days,  ends_with("AVG_CLAIM_CNT"), ends_with("CLAIM"))
Neg_dates <- select(Neg_all, PATIENT_ID, test_patient_id, label, index_date, lookback_date,
                    contains("_EXP"))

# imputing NAs with 0s in Flags and Frequency:
Pos_flags[is.na(Pos_flags)] <- 0
Pos_common_frequencies[is.na(Pos_common_frequencies)] <- 0
Pos_claims[is.na(Pos_claims)] <- 0
Neg_flags[is.na(Neg_flags)] <- 0
Neg_common_frequencies[is.na(Neg_common_frequencies)] <- 0
Neg_claims[is.na(Neg_claims)] <- 0

# write all this out to a csv
write_rds( Neg_claims,
           paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/",
                  "Neg_claims",
                  ".rds"))


# SEGREGATE MODELLING AND FEATURE SEL DATASETS ----------------------------

MOD_ID <- read_csv("F:/Projects/Strongbridge/data/feature_selection/patient_ids_c06_strat_model_pos.csv",
                   col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
FS_ID <- read_csv("F:/Projects/Strongbridge/data/feature_selection/patient_ids_c07_strat_fs_pos.csv",
                  col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

Pos_MOD_index <- which(Pos_all$PATIENT_ID %in% MOD_ID$PATIENT_ID)
Pos_FS_index <- which(Pos_all$PATIENT_ID %in% FS_ID$PATIENT_ID)
Neg_MOD_index <- which(Neg_all$test_patient_id %in% MOD_ID$PATIENT_ID)
Neg_FS_index <- which(Neg_all$test_patient_id %in% FS_ID$PATIENT_ID)

# POSITIVES
Pos_flags_MOD <- Pos_flags[Pos_MOD_index, ]
Pos_flags_FS <- Pos_flags[Pos_FS_index, ]
Pos_common_frequencies_MOD <- Pos_frequencies[Pos_MOD_index, ]
Pos_common_frequencies_FS <- Pos_frequencies[Pos_FS_index, ]
Pos_claims_MOD <- Pos_claims[Pos_MOD_index, ]
Pos_claims_FS <- Pos_claims[Pos_FS_index, ]
Pos_dates_MOD <- Pos_dates[Pos_MOD_index, ]
Pos_dates_FS <- Pos_dates[Pos_FS_index, ]

# NEGATIVES
Neg_flags_MOD <- Neg_flags[Neg_MOD_index, ]
Neg_flags_FS <- Neg_flags[Neg_FS_index, ]
Neg_common_frequencies_MOD <- Neg_frequencies[Neg_MOD_index, ]
Neg_common_frequencies_FS <- Neg_frequencies[Neg_FS_index, ]
Neg_claims_MOD <- Neg_claims[Neg_MOD_index, ]
Neg_claims_FS <- Neg_claims[Neg_FS_index, ]
Neg_dates_MOD <- Neg_dates[Neg_MOD_index, ]
Neg_dates_FS <- Neg_dates[Neg_FS_index, ]

# write out to rds:
mod_dir <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/"
write_rds( Neg_dates_MOD,
           paste0(mod_dir,
                  "Neg_dates_MOD",
                  ".rds"))

feat_dir <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/"
write_rds( Neg_dates_FS,
           paste0(feat_dir,
                  "Neg_dates_FS",
                  ".rds"))



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




