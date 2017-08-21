library(tidyverse)
library(lubridate)
install.packages("F:/PALab_Package/Current_Version/palab_1.0.0.tar.gz", repos = NULL)
library(palab)

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------

path <- "F:/Projects/Strongbridge/data/modelling/"

dfreq <- read_rds(paste0(path, "01_train_combined_common_frequencies.rds")) %>% 
  select(-test_patient_id, -index_date, -lookback_date)
dfreq <- sapply(dfreq, as.numeric) %>% data.frame

ddate <- read_rds(paste0(path, "01_train_combined_date_differences.rds")) %>%
  select(-test_patient_id, -index_date, -lookback_date)
ddate <- sapply(ddate, as.numeric) %>% data.frame

dall <- merge(dfreq, ddate %>% select(-label), by = "PATIENT_ID")

# ---------------------------------------------------------
# Run bivariate stats
# ---------------------------------------------------------

b <- bivar_stats_y_flag(
      input = dall, 
      var_config = paste0(path,"sb_var_config.csv"), 
      outcome_var = "label", 
      vargt0 = FALSE,
      output_dir = "F:/Projects/Strongbridge/results/descriptive_stats/bivariate_stats/"
    )

ba <- bivariate_stats_cat(
  input = dall, 
  var_config = paste0(path,"sb_var_config.csv"), 
  outcome_var = "label",
  vargt0 = TRUE,
  output_dir = "F:/Projects/Strongbridge/results/descriptive_stats/bivariate_stats/" 
)

# ---------------------------------------------------------
# Checks
# ---------------------------------------------------------

test <- dall %>% select(PATIENT_ID, starts_with("D_2768")) %>% filter(D_2768_AVG_CLAIM_CNT > 0)

test <- ddate %>% filter(S_S44_LAST_EXP_ < 0) %>% select(PATIENT_ID, S_S44_LAST_EXP_, S_S44_FIRST_EXP)

d <- read_rds("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/Pos_dates_MOD.rds")
ds <- filter(d, PATIENT_ID == "47438340"| PATIENT_ID == "48556661") %>% 
  select(PATIENT_ID, index_date, lookback_date, starts_with("S_S44"))
dd <- filter(ddate, PATIENT_ID == "47438340"| PATIENT_ID == "48556661") %>%
  select(PATIENT_ID, starts_with("S_S44"))

