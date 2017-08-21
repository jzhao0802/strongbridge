
#  ------------------------------------------------------------------------
# Topcode frequencies in 1:1000 negative cohort. Topcoding based on
# cap values used for topcoding training set
#  ------------------------------------------------------------------------

library(tidyverse)
library(palab)

# globals:
data_dir <- "F:/Projects/Strongbridge/data/modelling/"

# read in frequencies:
freq_raw <- read_rds(paste0(data_dir, "02_Neg_frequencies_1_to_1000.rds"))

# read in var config file:
var_config_raw <- read_csv(paste0(data_dir, "sb_var_config.csv"))

freq_config_index <- grep(paste(colnames(freq_raw), collapse = "|"), var_config$Column)

var_config <- var_config[freq_config_index, ]
setdiff(colnames(freq_raw), var_config$Column)
# var config missing: "index_date"      "lookback_date"   "lookback_days"   "test_patient_id"

# read in the extreme values thrsh file:
ex_val_thrsh <- read_csv(paste0(data_dir, "ex_val_caps_freq.csv"))

# convert all cols to numeric:
freq_raw$P_L2275_AVG_CLAIM_CNT <- as.numeric(freq_raw$P_L2275_AVG_CLAIM_CNT)
freq_raw$D_3591_AVG_CLAIM_CNT <- as.numeric(freq_raw$D_3591_AVG_CLAIM_CNT)
freq_raw$D_3592_AVG_CLAIM_CNT <- as.numeric(freq_raw$D_3592_AVG_CLAIM_CNT)
freq_raw$D_3590_AVG_CLAIM_CNT <- as.numeric(freq_raw$D_3590_AVG_CLAIM_CNT)
freq_raw$GENDER <- as.factor(freq_raw$GENDER)

# convert gender to dummy. Female == 1:
freq_raw$GENDER <- ifelse(freq_raw$GENDER == "F", 1, 0)

# # use palab function. Getting error owing to cols in data which are not in
# var_config.
# freq_capped <- extreme_values(input = freq_raw,
#                               var_config = paste0(data_dir, "sb_var_config.csv"),
#                               pth = 1,
#                               ex_val_thrsh = paste0(data_dir, "ex_val_caps_freq.csv"))

freq_capped <- freq_raw

# cap the dataset according to the ex_val_thrsh file:
for( i in 9:ncol(freq_capped)){
  
  # Find variable in thrsh file:
  Thrsh_index <- grep(colnames(freq_capped)[i], ex_val_thrsh$Variable)
  # Threshold corresponding to variable:
  Thrsh <- ex_val_thrsh$Thrsh[Thrsh_index]
  freq_capped[,i][freq_capped[,i] > Thrsh] <- Thrsh
  
}

# write out
write_rds(freq_capped, paste0(data_dir, "02_Neg_frequencies_1_to_1000_topcoded.rds"))
