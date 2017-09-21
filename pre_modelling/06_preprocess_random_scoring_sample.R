
#  ------------------------------------------------------------------------
# Preprocess random sample from scoring cohort
#  ------------------------------------------------------------------------

library(tidyverse)
library(mlr)
library(xgboost)

data_dir <- "F:/Projects/Strongbridge/data/Random_sample_scoring/"
training_dir <- "F:/Projects/Strongbridge/data/modelling/"
results_dir <- "F:/Projects/Strongbridge/data/modelling/"
# DATA IN -----------------------------------------------------------------

# sample
raw_data <- read_csv(paste0(data_dir, "Scoring_Final_Sample_C000_UP.csv"),
                     col_types = cols(PATIENT_ID = col_character(), .default = col_guess()))

# training data for model (for comparison):
train_data <- read_rds(paste0(training_dir, "01_train_combined_common_freq_new_index_topcoded.rds"))

# training data including date diffs:
train_combined <- read_rds(paste0(training_dir, "01_train_combined_date_differences_new_index.rds"))

# model
xgb_model <- read_rds("F:/Projects/Strongbridge/results/modelling/XGBOOST_preliminary/XGB_preliminary_model.rds")

# rename age and gender to AGE and GENDER:
colnames(raw_data)[c(2,3,4,5)] <- c("lookback_date", "index_date", "AGE", "GENDER")


# SELECT RELEVANT COLUMNS AND REORDER -------------------------------------

# extract shared cols:
shared_cols <- which(colnames(raw_data) %in% colnames(train_data))
# choose shared cols
raw_selection <- raw_data[,shared_cols]

# look at diffreences between the train_data and raw data:
setdiff(colnames(train_data), colnames(raw_data))

# re-order
raw_selection <- raw_selection[order(match(colnames(raw_selection), colnames(train_data[c(1,4:ncol(train_data))])))]
# check ordering
all.equal(colnames(raw_selection), colnames(train_data[c(1,4:ncol(train_data))]))

# gender dummy:
raw_selection$GENDER <- ifelse(raw_selection$GENDER == "F", 1, 0)

# Impute NAs with 0s:
raw_selection[is.na(raw_selection)]  <- 0

# TOPCODE -----------------------------------------------------------------

ex_val_thrsh <- read_csv(paste0(training_dir, "ex_val_caps_freq.csv"))

raw_capped <- raw_selection

for( i in 6:ncol(raw_capped)){
  
  # Find variable in thrsh file:
  Thrsh_index <- grep(colnames(raw_capped)[i], ex_val_thrsh$Variable)
  # Threshold corresponding to variable:
  Thrsh <- ex_val_thrsh$Thrsh[Thrsh_index]
  raw_capped[,i][raw_capped[,i] > Thrsh] <- Thrsh
  
}

raw_capped$label <- 0

# write out:
write_rds(raw_capped, paste0(results_dir, "03_random_scoring_freq_topcoded.rds"))


ggplot(train_data[train_data$S_S37_AVG_CLAIM > 0,], aes(x=S_S37_AVG_CLAIM, ..density.., fill=as.factor(label))) + 
  geom_density(alpha=.3)

ggplot(raw_capped, aes(x=S_S44_AVG_CLAIM, ..density..)) + 
  geom_density(alpha=.3)

ggplot(raw_selection, aes(x=S_S44_AVG_CLAIM, ..density..)) + 
  geom_density(alpha=.3)



