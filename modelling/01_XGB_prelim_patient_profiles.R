
#  ------------------------------------------------------------------------
# Top 10 patient profiles
#  ------------------------------------------------------------------------

library(tidyverse)
library(BBmisc)

data_dir <- "F:/Projects/Strongbridge/data/modelling/preliminary_model_data/"
input_dir <- "F:/Projects/Strongbridge/results/modelling/XGBOOST_preliminary/"

# DATA IN -----------------------------------------------------------------

model_data <- read_rds(paste0(data_dir, "01_prelim_matched_combined_train_test.rds"))
model_preds <- read_rds(paste0(input_dir, "XGB_prelim_test_predictions_1_1000.rds"))


# EXTRACT TOP 10 RISK SCORES ----------------------------------------------

pred_order <- BBmisc::order

top_10 <- pred_order[1:10,]

# join data to these patients:
top_profiles <- left_join(top_10, model_data, by = "PATIENT_ID")



