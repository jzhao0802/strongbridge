
#  ------------------------------------------------------------------------
# create 1:1000 negative cohort. Split raw files by variable type
#  ------------------------------------------------------------------------

library(tidyverse)
library(plyr)
library(stringr)

data_loc <- "F:/Projects/Strongbridge/data/Cohorts/00_Raw_data_pull/"


# COMMON VARIABLE NAMES ---------------------------------------------------

Neg_PA <- read_csv(paste0(data_loc, "Strongbridge_Neg_PA_C06_1000.csv"),
                   col_types = (cols(patient_id = col_character(), .default = col_guess())))
Neg_PA$label <- 0
colnames(Neg_PA) <- c("PATIENT_ID", "AGE", "GENDER", "index_date", "lookback_date",
                      "lookback_days", "test_patient_id", "label") 
write_rds(Neg_PA, paste0(output_dir, "Neg_PA.rds"))
rm(Neg_PA)
# JOIN EVERYTHING TOGETHER
# Neg_all <- join_all(list(Neg_PA, Neg_DX, Neg_RX, Neg_PR, Neg_SP), type = "left")




# VARIABLE TYPES ----------------------------------------------------------

# DX
Neg_DX <- read_csv(paste0(data_loc, "Strongbridge_Neg_DX_C06_1000.csv"),
                   col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

Neg_DX_flags <- select(Neg_DX, PATIENT_ID, ends_with("_FLAG"))
Neg_DX_claims <- select(Neg_DX, PATIENT_ID, ends_with("_CLAIM_CNT"), -contains("AVG"))
Neg_DX_frequencies <- select(Neg_DX, PATIENT_ID, ends_with("AVG_CLAIM_CNT"))
Neg_DX_dates <- select(Neg_DX, PATIENT_ID, contains("_EXP"))
# write out
write_rds(Neg_DX_flags, paste0(output_dir, "Neg_DX_flags.rds"))
write_rds(Neg_DX_claims, paste0(output_dir, "Neg_DX_counts.rds"))
write_rds(Neg_DX_frequencies, paste0(output_dir, "Neg_DX_freqencies.rds"))
write_rds(Neg_DX_dates, paste0(output_dir, "Neg_DX_dates.rds"))
rm(Neg_DX, Neg_DX_flags, Neg_DX_frequencies, Neg_DX_dates, Neg_DX_claims)

# RX

Neg_RX <-  read_csv(paste0(data_loc, "Strongbridge_Neg_RX_C06_1000.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

Neg_RX_flags <- select(Neg_RX, PATIENT_ID, ends_with("_FLAG"))
Neg_RX_claims <- select(Neg_RX, PATIENT_ID, ends_with("_CLAIM_CNT"), -contains("AVG"))
Neg_RX_frequencies <- select(Neg_RX, PATIENT_ID, ends_with("AVG_CLAIM_CNT"))
Neg_RX_dates <- select(Neg_RX, PATIENT_ID, contains("_EXP"))
# write out
write_rds(Neg_RX_flags, paste0(output_dir, "Neg_RX_flags.rds"))
write_rds(Neg_RX_claims, paste0(output_dir, "Neg_RX_counts.rds"))
write_rds(Neg_RX_frequencies, paste0(output_dir, "Neg_RX_freqencies.rds"))
write_rds(Neg_RX_dates, paste0(output_dir, "Neg_RX_dates.rds"))
rm(Neg_RX, Neg_RX_flags, Neg_RX_frequencies, Neg_RX_dates, Neg_RX_claims)

# PR

Neg_PR <-  read_csv(paste0(data_loc, "Strongbridge_Neg_PR_C06_1000.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

table(str_sub(colnames(Neg_PR), -5, -1))

Neg_PR_flags <- select(Neg_PR, PATIENT_ID, ends_with("_FLAG"))
Neg_PR_claims <- select(Neg_PR, PATIENT_ID, ends_with("_CLAIM_CNT"), -contains("AVG"))
Neg_PR_frequencies <- select(Neg_PR, PATIENT_ID, ends_with("AVG_CLAIM_CNT"))
Neg_PR_dates <- select(Neg_PR, PATIENT_ID, contains("_EXP"))
# write out
write_rds(Neg_PR_flags, paste0(output_dir, "Neg_PR_flags.rds"))
write_rds(Neg_PR_claims, paste0(output_dir, "Neg_PR_counts.rds"))
write_rds(Neg_PR_frequencies, paste0(output_dir, "Neg_PR_freqencies.rds"))
write_rds(Neg_PR_dates, paste0(output_dir, "Neg_PR_dates.rds"))
rm(Neg_PR, Neg_PR_flags, Neg_PR_frequencies, Neg_PR_dates, Neg_PR_claims)

# SP

Neg_SP <-  read_csv(paste0(data_loc, "Strongbridge_Neg_SP_C06_1000.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

table(str_sub(colnames(Neg_SP), -5, -1))

Neg_SP_flags <- select(Neg_SP, PATIENT_ID, ends_with("_FLAG"))
Neg_SP_claims <- select(Neg_SP, PATIENT_ID, ends_with("_CLAIM_CNT"), -contains("AVG"))
Neg_SP_frequencies <- select(Neg_SP, PATIENT_ID, ends_with("CLAIM"))
Neg_SP_dates <- select(Neg_SP, PATIENT_ID, contains("_EXP"))
# write out
write_rds(Neg_SP_flags, paste0(output_dir, "Neg_SP_flags.rds"))
write_rds(Neg_SP_claims, paste0(output_dir, "Neg_SP_counts.rds"))
write_rds(Neg_SP_frequencies, paste0(output_dir, "Neg_SP_freqencies.rds"))
write_rds(Neg_SP_dates, paste0(output_dir, "Neg_SP_dates.rds"))
rm(Neg_SP, Neg_SP_flags, Neg_SP_frequencies, Neg_SP_dates, Neg_SP_claims)



# Join at this stage and then deal with the rest:

Flags_RX_DX <- join_all(list(Neg_PA, Neg_RX_flags, Neg_DX_flags), type = "left")
Freq_RX_DX <- join_all(list(Neg_PA, Neg_RX_frequencies, Neg_DX_frequencies), type = "left")
Count_RX_DX <- join_all(list(Neg_PA, Neg_RX_claims, Neg_DX_claims), type = "left")
Dates_RX_DX <- join_all(list(Neg_PA, Neg_RX_dates, Neg_DX_dates), type = "left")

# PR
Neg_PR_flags <- select(Neg_PR, PATIENT_ID, ends_with("_FLAG"))
Neg_PR_claims <- select(Neg_PR, PATIENT_ID, ends_with("_CLAIM_CNT"), -contains("AVG"))
Neg_PR_frequencies <- select(Neg_PR, PATIENT_ID, ends_with("AVG_CLAIM_CNT"))
Neg_PR_dates <- select(Neg_PR, PATIENT_ID, contains("_EXP"))
rm(Neg_PR)





# Feature select and join -----------------------------------------

raw_dir <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/1_to_1000_dataset/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# feature selection:
features <- read_csv("F:/Projects/Strongbridge/data/modelling/list_of_features.csv")
# common vars:
PA <- read_rds(paste0(raw_dir, "Neg_PA.rds"))

# DATES

DX_date <- read_rds(paste0(raw_dir, "Neg_DX_dates.rds"))
RX_date <- read_rds(paste0(raw_dir, "Neg_RX_dates.rds"))
PR_date <- read_rds(paste0(raw_dir, "Neg_PR_dates.rds"))
SP_date <- read_rds(paste0(raw_dir, "Neg_SP_dates.rds"))



DX_date_mod <- DX_date[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(DX_date))]
rm(DX_date)
RX_date_mod <- RX_date[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(RX_date))]
rm(RX_date)
PR_date_mod <- PR_date[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(PR_date))]
rm(PR_date)
SP_date_mod <- SP_date[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(SP_date))]
rm(SP_date)

All_date <- join_all(list(PA, DX_date_mod, RX_date_mod, PR_date_mod, SP_date_mod), by = "PATIENT_ID", type = "left")

write_rds(All_date, paste0(output_dir, "02_Neg_dates_1_to_1000.rds"))
rm(DX_date_mod, All_date, PR_date_mod, RX_date_mod, SP_date_mod)

# FREQUENCIES

DX_freq <- read_rds(paste0(raw_dir, "Neg_DX_frequencies.rds"))
RX_freq <- read_rds(paste0(raw_dir, "Neg_RX_frequencies.rds"))
PR_freq <- read_rds(paste0(raw_dir, "Neg_PR_frequencies.rds"))
SP_freq <- read_rds(paste0(raw_dir, "Neg_SP_frequencies.rds"))

DX_freq_mod <- DX_freq[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(DX_freq))]
rm(DX_freq)
RX_freq_mod <- RX_freq[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(RX_freq))]
rm(RX_freq)
PR_freq_mod <- PR_freq[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(PR_freq))]
rm(PR_freq)
SP_freq_mod <- SP_freq[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(SP_freq))]
rm(SP_freq)

All_freq <- join_all(list(PA, DX_freq_mod, RX_freq_mod, PR_freq_mod, SP_freq_mod), by = "PATIENT_ID", type = "left")
# impute missings with 0
All_freq[is.na(All_freq)] <- 0

write_rds(All_freq, paste0(output_dir, "02_Neg_frequencies_1_to_1000.rds"))

rm(DX_freq_mod, All_freq, PR_freq_mod, RX_freq_mod, SP_freq_mod)

# COUNTS

DX_count <- read_rds(paste0(raw_dir, "Neg_DX_counts.rds"))
RX_count <- read_rds(paste0(raw_dir, "Neg_RX_counts.rds"))
PR_count <- read_rds(paste0(raw_dir, "Neg_PR_counts.rds"))
SP_count <- read_rds(paste0(raw_dir, "Neg_SP_counts.rds"))

DX_count_mod <- DX_count[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(DX_count))]
rm(DX_count)
RX_count_mod <- RX_count[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(RX_count))]
rm(RX_count)
PR_count_mod <- PR_count[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(PR_count))]
rm(PR_count)
SP_count_mod <- SP_count[ ,grep(paste(features$Variable_Stem, collapse = "|"), colnames(SP_count))]
rm(SP_count)

All_count <- join_all(list(PA, DX_count_mod, RX_count_mod, PR_count_mod, SP_count_mod), by = "PATIENT_ID", type = "left")
# impute missings with 0
All_count[is.na(All_count)] <- 0

write_rds(All_count, paste0(output_dir, "02_Neg_counts_1_to_1000.rds"))

rm(DX_freq_mod, All_freq, PR_freq_mod, RX_freq_mod, SP_freq_mod)

