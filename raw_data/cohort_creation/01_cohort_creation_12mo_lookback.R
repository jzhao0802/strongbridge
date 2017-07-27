#  ------------------------------------------------------------------------
# 01_COHORT CREATION_12_MONTH_LOOKBACK
#  ------------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(palab)
library(lubridate)
library(plyr)
# LOAD DATA ---------------------------------------------------------------

data_loc <- "F:/Projects/Strongbridge/data/Cohorts/02_Raw_data_pull_12_months/"

Neg_DX <- read_csv(paste0(data_loc, "Strongbridge_Neg_DX_12.csv"),
                   col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_PA <- read_csv(paste0(data_loc, "Strongbridge_Neg_PA.csv"),
                   col_types = (cols(patient_id = col_character(), .default = col_guess())))
Neg_PR <-  read_csv(paste0(data_loc, "Strongbridge_Neg_PR_12.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_RX <-  read_csv(paste0(data_loc, "Strongbridge_Neg_RX_12.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_SP <-  read_csv(paste0(data_loc, "Strongbridge_Neg_SP_12.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_DX <-  read_csv(paste0(data_loc, "Strongbridge_Pos_DX_12.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PA <-  read_csv(paste0(data_loc, "Strongbridge_Pos_PA.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PR <-  read_csv(paste0(data_loc, "Strongbridge_Pos_PR_12.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_RX <-  read_csv(paste0(data_loc, "Strongbridge_Pos_RX_12.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_SP <-  read_csv(paste0(data_loc, "Strongbridge_Pos_SP_12.csv"),
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

# remove rows with all NA apart from common vars:
Pos_all <- Pos_all[apply(Pos_all[,7:ncol(Pos_all)], 1, function(y) !all(is.na(y))),]
# this removes 0 from the positive cohort, and 128 from the negative
Neg_all <- Neg_all[apply(Neg_all[,8:ncol(Pos_all)], 1, function(y) !all(is.na(y))),]

# add label col
Pos_all$label <- 1
Neg_all$label <- 0

# VARIABLE ENCODING -------------------------------------------------------

table(str_sub(colnames(Neg_all), -5, -1))
table(str_sub(colnames(Pos_all), -5, -1))

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

# SEGREGATE MODELLING AND FEATURE SEL DATASETS ----------------------------

MOD_ID <- read_csv("F:/Projects/Strongbridge/data/feature_selection/patient_ids_c06_strat_model_pos.csv",
                   col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

Pos_MOD_index <- which(Pos_all$PATIENT_ID %in% MOD_ID$PATIENT_ID)
Neg_MOD_index <- which(Neg_all$test_patient_id %in% MOD_ID$PATIENT_ID)

# POSITIVES
Pos_flags_MOD <- Pos_flags[Pos_MOD_index, ]
Pos_common_frequencies_MOD <- Pos_common_frequencies[Pos_MOD_index, ]
Pos_claims_MOD <- Pos_claims[Pos_MOD_index, ]
Pos_dates_MOD <- Pos_dates[Pos_MOD_index, ]

# NEGATIVES
Neg_flags_MOD <- Neg_flags[Neg_MOD_index, ]
Neg_common_frequencies_MOD <- Neg_common_frequencies[Neg_MOD_index, ]
Neg_claims_MOD <- Neg_claims[Neg_MOD_index, ]
Neg_dates_MOD <- Neg_dates[Neg_MOD_index, ]

# write out to rds:

mod_dir <- "F:/Projects/Strongbridge/data/Cohorts/03_Cohorts_by_variable_type_12_months/"
# NEGATIVES
write_rds( Neg_flags_MOD,
           paste0(mod_dir,
                  "Neg_flags_MOD",
                  ".rds"))
write_rds( Neg_common_frequencies_MOD,
           paste0(mod_dir,
                  "Neg_common_frequencies_MOD",
                  ".rds"))
write_rds( Neg_claims_MOD,
           paste0(mod_dir,
                  "Neg_claims_MOD",
                  ".rds"))
write_rds( Neg_dates_MOD,
           paste0(mod_dir,
                  "Neg_dates_MOD",
                  ".rds"))
# POSITIVES
write_rds( Pos_flags_MOD,
           paste0(mod_dir,
                  "Pos_flags_MOD",
                  ".rds"))
write_rds( Pos_common_frequencies_MOD,
           paste0(mod_dir,
                  "Pos_common_frequencies_MOD",
                  ".rds"))
write_rds( Pos_claims_MOD,
           paste0(mod_dir,
                  "Pos_claims_MOD",
                  ".rds"))
write_rds( Pos_dates_MOD,
           paste0(mod_dir,
                  "Pos_dates_MOD",
                  ".rds"))



