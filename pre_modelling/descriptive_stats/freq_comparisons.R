library(tidyverse)
library(lubridate)

# For 12 month lookback
path12 <- "F:/Projects/Strongbridge/data/Cohorts/03_Cohorts_by_variable_type_12_months/"
suffix12 <- "_12"


# For 13 month plus lookback
path13 <- "F:/Projects/Strongbridge/data/Cohorts/05_Cohorts_by_variable_type_13_months/"
suffix13 <- "_13"

# For complete
path <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/"
suffix <- ""


# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------

freq_pos12 <- read_rds(paste0(path12, "Pos_common_frequencies_MOD", suffix12, ".rds")) %>%
  select(-PATIENT_ID, -label, -AGE, -GENDER, -index_date, -lookback_date, -lookback_days)
freq_pos12 <- sapply(freq_pos12, as.numeric) %>% data.frame
freq_neg12 <- read_rds(paste0(path12, "Neg_common_frequencies_MOD", suffix12, ".rds")) %>%
  select(-PATIENT_ID, -label, -AGE, -GENDER, -index_date, -lookback_date, -lookback_days)
freq_neg12 <- sapply(freq_neg12, as.numeric) %>% data.frame

freq_pos13 <- read_rds(paste0(path13, "Pos_common_frequencies_MOD", suffix13, ".rds")) %>%
  select(-PATIENT_ID, -label, -AGE, -GENDER, -index_date, -lookback_date, -lookback_days)
freq_pos13 <- sapply(freq_pos13, as.numeric) %>% data.frame
freq_neg13 <- read_rds(paste0(path13, "Neg_common_frequencies_MOD", suffix13, ".rds")) %>%
  select(-PATIENT_ID, -label, -AGE, -GENDER, -index_date, -lookback_date, -lookback_days)
freq_neg13 <- sapply(freq_neg13, as.numeric) %>% data.frame

freq_pos <- read_rds(paste0(path, "Pos_common_frequencies_MOD", suffix, ".rds")) %>%
  select(-PATIENT_ID, -label, -AGE, -GENDER, -index_date, -lookback_date, -lookback_days)
freq_pos <- sapply(freq_pos, as.numeric) %>% data.frame
freq_neg <- read_rds(paste0(path, "Neg_common_frequencies_MOD", suffix, ".rds")) %>%
  select(-PATIENT_ID, -label, -AGE, -GENDER, -index_date, -lookback_date, -lookback_days)
freq_neg <- sapply(freq_neg, as.numeric) %>% data.frame

# ---------------------------------------------------------
# Overall
# ---------------------------------------------------------
nadea <- function(df) {

counts = data.frame(
  overall = mean(rowSums(df, na.rm = F, dims = 1)), 
  spec = mean(rowSums(df %>% select(starts_with("S")) , na.rm = F, dims = 1)),
  gpi6 = mean(rowSums(df %>% select(starts_with("G")) , na.rm = F, dims = 1)),
  diag = mean(rowSums(df %>% select(starts_with("D")) , na.rm = F, dims = 1)),
  proc = mean(rowSums(df %>% select(starts_with("P")) , na.rm = F, dims = 1))
)
}

pos_12 <- nadea(freq_pos12)
pos_13 <- nadea(freq_pos13)
neg_12 <- nadea(freq_neg12 %>% select(-test_patient_id))
neg_13 <- nadea(freq_neg13 %>% select(-test_patient_id))

pos_co <- nadea(freq_pos)
neg_co <- nadea(freq_neg %>% select(-test_patient_id))
