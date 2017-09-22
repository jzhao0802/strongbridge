
#  ------------------------------------------------------------------------
# Topcode date differences in 1:1000 negative cohort. Topcoding based on
# cap values used for topcoding training set
#  ------------------------------------------------------------------------

library(tidyverse)

# globals:
data_dir <- "F:/Projects/Strongbridge/data/modelling/"

# read in frequencies:
dd_raw <- read_rds(paste0(data_dir, "02_1_to_1000_date_differences.rds"))

# # read in var config file:
# var_config_raw <- read_csv(paste0(data_dir, "sb_var_config.csv"))
# freq_config_index <- grep(paste(colnames(freq_raw), collapse = "|"), var_config$Column)
# var_config <- var_config[freq_config_index, ]
# setdiff(colnames(freq_raw), var_config$Column)
# # var config missing: "index_date"      "lookback_date"   "lookback_days"   "test_patient_id"

# read in the extreme values thrsh file:
ex_val_thrsh <- read_csv(paste0(data_dir, "ex_val_caps_dates.csv"))

# gender dummy:
dd_raw$GENDER <- ifelse(dd_raw$GENDER == "F", 1, 0)


dd_capped <- dd_raw

# cap the dataset according to the ex_val_thrsh file:
for( i in 7:ncol(dd_capped)){
  
  # Find variable in thrsh file:
  Thrsh_index <- grep(colnames(dd_capped)[i], ex_val_thrsh$Variable)
  # Threshold corresponding to variable:
  Thrsh <- ex_val_thrsh$Thrsh[Thrsh_index]
  dd_capped[,i][dd_capped[,i] > Thrsh] <- Thrsh
  
}

# round date differences to nearest whole number:
dd_capped <- read_rds(paste0(data_dir, "02_Neg_date_differences_1_to_1000_topcoded.rds"))

dd_capped[,7:ncol(dd_capped)] <- round(dd_capped[,7:ncol(dd_capped)])

# write out
write_rds(dd_capped, paste0(data_dir, "02_Neg_date_differences_1_to_1000_topcoded.rds"))
