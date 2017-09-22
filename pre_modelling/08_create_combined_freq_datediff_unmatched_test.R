
#  ------------------------------------------------------------------------
# Create unmatched combined dataset for testing model
#  ------------------------------------------------------------------------

library(tidyverse)

input_dir <- "F:/Projects/Strongbridge/data/modelling/"
data_dir <- "F:/Projects/Strongbridge/data/modelling/Advanced_model_data/"

# data in -----------------------------------------------------------------

unmatched <- read_rds(paste0(input_dir, "05_unmatched_neg_capped_freq_datediff.rds"))

combined_matched <- read_rds(paste0(data_dir, "04_combined_train_matched_test_capped_freq_datediff.rds"))


# replace test negs with negs from unmatched sample -----------------------

# remove patient ID which appear in the positives or training negatives:
dupes <- c(which(unmatched$PATIENT_ID %in% combined_matched$PATIENT_ID[combined_matched$subset == "train_neg"]),
           which(unmatched$PATIENT_ID %in% combined_matched$PATIENT_ID[combined_matched$subset == "pos"]))
unmatched <- unmatched[-dupes,]
unmatched_deduped <- unmatched[!duplicated(unmatched$PATIENT_ID), ]

set.seed(123)
random_sample <- sample(nrow(unmatched_deduped), length(combined_matched$subset[combined_matched$subset == "test_neg"]), 
                        replace = FALSE)

unmatched_sample <- unmatched_deduped[random_sample, ]


unmatched_sample$subset <- "test_neg"
unmatched_sample$test_patient_id <- NA

# order columns so they are the same:
unmatched_sample <- unmatched_sample[order(match(colnames(unmatched_sample), colnames(combined_matched)))]
all.equal(colnames(unmatched_sample), colnames(combined_matched))

combined_matched[combined_matched$subset == "test_neg", ] <- unmatched_sample

# write out unmatched data:
write_rds(combined_matched, paste0(data_dir, "05_advanced_combined_train_unmatched_test.rds"))

