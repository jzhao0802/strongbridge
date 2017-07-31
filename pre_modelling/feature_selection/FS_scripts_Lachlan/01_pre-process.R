#  ------------------------------------------------------------------------
# PRE-PROCESS STRONGBRIDGE DATA FOR FEATURE SELECTION
#  ------------------------------------------------------------------------

library(mlr)
library(tidyverse)
library(stringr)
library(palabmod)
library(xgboost)
library(caTools)

# DATA IN -----------------------------------------------------------------

Pos_freq <- read_rds(paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/",
                            "Pos_common_frequencies_FS",
                            ".rds"))
Neg_freq <- read_rds(paste0("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/",
                            "Neg_common_frequencies_FS",
                            ".rds"))


# MANIPULATION  -----------------------------------------------------------

# set test patient id in positive cohort to NA
Pos_freq$test_patient_id <- NA

# Order variables so the datasets can be bound
setdiff(colnames(Pos_freq), colnames(Neg_freq))
setdiff(colnames(Neg_freq), colnames(Pos_freq))
Pos_order <- Pos_freq[order(match(colnames(Pos_freq), colnames(Neg_freq)))]
all.equal(colnames(Pos_order), colnames(Neg_freq))

# convert all frequency variables to numeric:
Neg_freq[,8:ncol(Neg_freq)] <- sapply(Neg_freq[,8:ncol(Neg_freq)], as.numeric)
Pos_order[,8:ncol(Pos_order)] <- sapply(Pos_order[,8:ncol(Pos_order)], as.numeric)
combined_data <- bind_rows(Neg_freq, Pos_order)

# create gender dummy
combined_data$gender_dum <- ifelse(combined_data$GENDER == "F", 1, 0)
combined_data$GENDER <- NULL

# write out to csv:
write_rds(combined_data, "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/combined_common_frequencies.rds")

# split the data for clustering into training and testing:
split_data <- sample.split(combined_data$label, SplitRatio = 0.75)
train_clusters <- combined_data[split_data, ]
test_clusters <- combined_data[!split_data, ]

# write training and testing sets to rds:
write_rds(train_clusters, "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/train_clustering.rds")
write_rds(test_clusters, "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/test_clustering.rds")

