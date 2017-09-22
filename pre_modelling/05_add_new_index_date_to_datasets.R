
#  ------------------------------------------------------------------------
# ADDING NEW INDEX DATE TO DATASETS BASED ON MOST RECENT LAST EXPOSURE DATE
# (only added to negatives)
#  ------------------------------------------------------------------------

data_dir <- "F:/Projects/Strongbridge/data/modelling/"

# New index date dataframes:
new_index_1000 <- read_rds(paste0(data_dir, "New_index_date_1_1000.rds"))
new_index_50 <- read_rds(paste0(data_dir, "New_index_date_1_50.rds"))

# datasets to add new index to:
Neg_freq <- read_rds(paste0(data_dir, "02_Neg_frequencies_1_to_1000_topcoded.rds"))
Neg_dates <- read_rds(paste0(data_dir, "02_Neg_dates_1_to_1000.rds"))
train_freq <- read_rds(paste0(data_dir, "01_train_combined_common_freq_topcoded.rds"))
train_dates <- read_rds(paste0(data_dir, "01_train_combined_dates_unformatted.rds"))


# 1:1000 MATCHED NEGATIVES ------------------------------------------------

# Add new index date to neg frequencies and dates:
all.equal(Neg_freq$PATIENT_ID, as.character(new_index_1000$PATIENT_ID))
all.equal(Neg_dates$PATIENT_ID, as.character(new_index_1000$PATIENT_ID))
Neg_freq$index_date <- new_index_1000$new_index_date
Neg_dates$index_date <- new_index_1000$new_index_date
# write out
write_rds(Neg_freq, paste(data_dir, "02_Neg_frequencies_1_to_1000_new_index_topcoded.rds"))
write_rds(Neg_dates, paste(data_dir, "02_Neg_dates_1_to_1000_new_index.rds"))


# TRAINING DATA -----------------------------------------------------------

# Prepare to left join new index date onto training data:
new_index_train <- data.frame(PATIENT_ID = new_index_50$PATIENT_ID,
                              new_index_date = new_index_50$new_index_date)
new_index_train$PATIENT_ID <- as.character(new_index_train$PATIENT_ID)


# add new index to topcoded training frequencies:
train_freq_new_index <- left_join(train_freq, new_index_train, by = "PATIENT_ID")
train_freq_new_index$index_date <- as.character(mdy(train_freq_new_index$index_date))
train_freq_new_index$new_index_date <- as.character(train_freq_new_index$new_index_date)
# assign old index date to positives so new index date column is complete:
train_freq_new_index$index_date <- ifelse(is.na(train_freq_new_index$new_index_date), 
                                              train_freq_new_index$index_date,
                                              train_freq_new_index$new_index_date)
# set new_index_date column to NULL as it's no longer needed:
training_freq_new_index$new_index_date <- NULL
# write out:
write_rds(train_freq_new_index, paste0(data_dir, "01_train_combined_common_freq_new_index_topcoded.rds"))

# add new index date to topcoded training dates
train_dates_new_index <- left_join(train_dates, new_index_train, by = "PATIENT_ID")
train_dates_new_index$index_date <- as.character(mdy(train_dates_new_index$index_date))
train_dates_new_index$new_index_date <- as.character(train_dates_new_index$new_index_date)
# assign old index date to positives so new index date column is complete:
train_dates_new_index$index_date <- ifelse(is.na(train_dates_new_index$new_index_date), 
                                          train_dates_new_index$index_date,
                                          train_dates_new_index$new_index_date)
# set new_index_date column to NULL as it's no longer needed:
train_dates_new_index$new_index_date <- NULL
# write out:
write_rds(train_dates_new_index, paste0(data_dir, "01_train_combined_dates_unformatted_new_index.rds"))



