
#  ------------------------------------------------------------------------
# Creating the feature-selected top-coded modelling training data 
# (frequencies and dates)
#  ------------------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(stats)

# Globals
raw_dir <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/"
output_dir <- "F:/Projects/Strongbridge/data/modelling/"

# Raw modelling data in  --------------------------------------------------

raw_neg_freq <- read_rds(paste0(raw_dir, "Neg_common_frequencies_MOD.rds"))
raw_pos_freq <- read_rds(paste0(raw_dir, "Pos_common_frequencies_MOD.rds"))
raw_neg_dates <- read_rds(paste0(raw_dir, "Neg_dates_MOD.rds"))
raw_pos_dates <- read_rds(paste0(raw_dir, "Pos_dates_MOD.rds"))


# Create combined ---------------------------------------------------------
# FREQUENCIES
two_way_setdiff(colnames(raw_neg_freq), colnames(raw_pos_freq))
raw_pos_freq$test_patient_id <- NA
# Order variables so the datasets can be bound
pos_freq_order <- raw_pos_freq[order(match(colnames(raw_pos_freq), colnames(raw_neg_freq)))]
all.equal(colnames(pos_freq_order), colnames(raw_neg_freq))

combined_freq <- rbind(pos_freq_order, raw_neg_freq)

# DATES
raw_pos_dates$test_patient_id <- NA
# Order variables so the datasets can be bound
pos_dates_order <- raw_pos_dates[order(match(colnames(raw_pos_dates), colnames(raw_neg_dates)))]
all.equal(colnames(pos_dates_order), colnames(raw_neg_dates))

combined_dates <- rbind(pos_dates_order, raw_neg_dates)


# SELECT MODELLING FEATURES -----------------------------------------------

features <- read_csv("F:/Projects/Strongbridge/data/modelling/list_of_features.csv")

# FREQUENCIES
# columns with the features we want:
features_index_freq <- grep(paste(features$Variable_Stem, collapse = "|"), colnames(combined_freq))
combined_freq_select <- combined_freq[, features_index_freq]

# DATES
# columns with the features we want:
features_index_dates <- grep(paste(features$Variable_Stem, collapse = "|"))
# FUNCTIONS ---------------------------------------------------------------

# function to topcode
topcode <- function(input, cap) {
  
  capped <- sapply(input, function(x) { 
    quant <- quantile(x, cap)
    x[x > P99] <- quant
    return(x)
  })
  
  return(as.data.frame(capped))
  
}

# function to perform setdiff both ways (saves a bit of typing)
two_way_setdiff <- function(x, y, lengths_only = FALSE) {
  
  x_no_y <- setdiff(x, y)
  print(paste("In x but not y:"))
  if(lengths_only) {
    print(length(x_no_y)) } else {
      print(x_no_y)
    }
  
  y_no_x <- setdiff(y, x)
  print(paste("In y but not x:"))
  if(lengths_only) {
    print(length(y_no_x)) } else {
      print(y_no_x)
    }
}





