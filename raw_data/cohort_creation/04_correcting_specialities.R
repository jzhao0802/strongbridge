#  ------------------------------------------------------------------------
# Fixing error in speciality vcariables
#  ------------------------------------------------------------------------

library(tidyverse)


# Load in new speciality data ---------------------------------------------

speciality_dir <- "F:/Projects/Strongbridge/data/Cohorts/00_Raw_data_pull/New Specialties files 20170911/"
data_dir <- "F:/Projects/Strongbridge/data/modelling/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"


SP_train <- read_csv(paste0(speciality_dir, "Strongbridge_Neg_SP_C06_UP.csv"),
                     col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
SP_test <- read_csv(paste0(speciality_dir,"Strongbridge_Neg_SP_C06_1000_UP.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
gc()
# Load in uncapped frequencies and dates files ----------------------------

train_freq <- read_rds(paste0(data_dir, "01_train_combined_common_frequencies.rds"))
train_dates <- read_rds(paste0(data_dir,"01_train_combined_dates_unformatted.rds"))
test_freq <- read_rds(paste0(data_dir,"02_Neg_frequencies_1_to_1000.rds"))
test_dates <-  read_rds(paste0(data_dir,"02_Neg_dates_1_to_1000.rds"))



# Cut out old specialities and poitives -----------------------------------

train_freq_pos <- train_freq[train_freq$label == 1,]
train_freq_neg <- train_freq[train_freq$label == 0,]

train_dates_pos <- train_dates[train_dates$label == 1,]
train_dates_neg <- train_dates[train_dates$label == 0,]

train_freq_nosp <- select(train_freq_neg, -starts_with("S_"))
train_dates_nosp <- select(train_dates_neg, -starts_with("S_"))
test_freq_nosp <- select(test_freq, -starts_with("S_"))
test_dates_nosp <- select(test_dates, -starts_with("S_"))

# Replace variables in old cohort with new specialities -------------------

# get correct variable names:
SP_freq_names <- select(train_freq, starts_with("S_")) %>% select(ends_with("CLAIM")) %>% colnames()
SP_dates_names <- select(train_dates, starts_with("S_")) %>% select(contains("EXP")) %>% colnames()

# extract these variables from the SP_train and SP_test dataframes:
SP_train_freq <- select_(SP_train, .dots =  c("PATIENT_ID", SP_freq_names))
SP_train_dates <- select_(SP_train, .dots =  c("PATIENT_ID", SP_dates_names))
SP_test_freq <- select_(SP_test, .dots =  c("PATIENT_ID", SP_freq_names))
SP_test_dates <- select_(SP_test, .dots =  c("PATIENT_ID", SP_dates_names))


# Join everything together to create single datasets ----------------------


# training freq:
train_freq_neg_with_sp <- left_join(train_freq_nosp, SP_train_freq)
all.equal(colnames(train_freq_neg_with_sp), colnames(train_freq_pos))
train_freq_combined <- rbind(train_freq_pos, train_freq_neg_with_sp)
# impute NA with 0:
train_freq_combined[is.na(train_freq_combined)] <- 0
train_freq_combined$test_patient_id[train_freq_combined$test_patient_id == 0] <- NA

# training dates
train_dates_neg_with_sp <- left_join(train_dates_nosp, SP_train_dates)
all.equal(colnames(train_dates_neg_with_sp), colnames(train_dates_pos))
train_dates_combined <- rbind(train_dates_pos, train_dates_neg_with_sp)

# test freq
test_freq_with_sp <- left_join(test_freq_nosp, SP_test_freq)
test_freq_with_sp[is.na(test_freq_with_sp)] <- 0

# test dates
test_dates_with_sp <- left_join(test_dates_nosp, SP_test_dates)

# Write out new files (archive previous versions) -------------------------

write_rds(train_freq_combined, paste0(output_dir, "01_train_combined_common_frequencies.rds"))
write_rds(train_dates_combined, paste0(output_dir, "01_train_combined_dates_unformatted.rds"))
write_rds(test_freq_with_sp, paste0(output_dir, "02_Neg_frequencies_1_to_1000.rds"))
write_rds(test_dates_with_sp, paste0(output_dir, "02_Neg_dates_1_to_1000.rds"))






