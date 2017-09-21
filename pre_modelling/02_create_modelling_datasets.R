
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

# READ IN DATA IF HAVEN'T ALREADY

# list of features:
features <- read_csv("F:/Projects/Strongbridge/data/modelling/list_of_features.csv")

# FREQUENCIES
# columns with the features we want:
features_index_freq <- grep(paste(features$Variable_Stem, collapse = "|"), colnames(combined_freq))
combined_freq_select <- combined_freq[, features_index_freq]

# DATES
# columns with the features we want:
features_index_dates <- grep(paste(features$Variable_Stem, collapse = "|"), colnames(combined_dates))
combined_dates_select <- combined_dates[, features_index_dates]

which(sapply(combined_freq_select, class) == "character")


# Write out results -------------------------------------------------------

write_rds(combined_freq_select, paste0(output_dir, "01_combined_common_frequencies.rds"))
write_rds(combined_dates_select, paste0(output_dir, "01_combined_dates_unformatted.rds"))




# FUNCTIONS ---------------------------------------------------------------

# function to topcode based on max cap over classes.
topcode <- function(input, cap, label) {
  # extract index of positives and negatives:
  pos_index <- which(input[[label]] == 1)
  neg_index <- which(input[[label]] == 0)
  
  capped <- sapply(input, function(x) { 
    # segregate the vector into positives and negatives:
    pos <- x[pos_index]
    neg <- x[neg_index]
    # work out which class has the higher cap:
    quant_pos <- stats::quantile(pos, cap, na.rm = TRUE)
    quant_neg <- stats::quantile(neg, cap, na.rm = TRUE)
    quant <- max(quant_pos, quant_neg)
    
    # set anything in the dataset above this value to this value
    x[x > quant] <- quant
    
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





