
#  ------------------------------------------------------------------------
# Top coding data as part of pre-modelling pipeline
#  ------------------------------------------------------------------------

library(tidyverse)

# globals
data_dir <-"F:/Projects/Strongbridge/data/modelling/"


# Training frequencies ----------------------------------------------------

training_freq_raw <- read_rds(paste0(data_dir, "01_train_combined_common_frequencies.rds"))

# extract variable for topcoding and label:

training_freq <- dplyr::select(training_freq_raw, label, dplyr::contains("CLAIM"))
training_freq <- as.data.frame(sapply(training_freq, function(x) { as.numeric(as.character(x)) } ))
training_freq_topcoded <- topcode_frequencies(training_freq, 0.99, label = "label")
# bind with common variables:
training_freq_combined <- data.frame(training_freq_raw[,1:7],
                                training_freq_topcoded)

# convert gender to dummy:
training_freq_combined$GENDER <- ifelse(training_freq_combined$GENDER == "F", 1, 0)

# write out:
write_rds(training_freq_combined, paste0(data_dir, "01_train_combined_common_freq_topcoded.rds"))


# Training date differences -----------------------------------------------
train_diffs_raw <- read_rds(paste0(data_dir, "01_train_combined_date_differences.rds"))

# extract variables for topcoding and label:
train_dates <- dplyr::select(train_diffs_raw, label, dplyr::contains("EXP"))
train_dates_topcode <- topcode_date_diffs(input = train_dates, cap = 0.99, label = "label")
# bind with common variables:
train_dates_combined <- data.frame(train_diffs_raw[,1:5],
                                   train_dates_topcode)

# write out:
write_rds(train_dates_combined, paste0(data_dir, "01_train_combined_date_differences_topcoded.rds"))


# CREATING EX_VAL_THRSH FILES ---------------------------------------------
# find maximums of each column in the topcoded dataset and save for future reference:

data_dir <-"F:/Projects/Strongbridge/data/modelling/"

train_dates_combined <- read_rds(paste0(data_dir, "01_train_combined_date_differences_topcoded.rds"))

training_freq_combined <- read_rds(paste0(data_dir, "01_train_combined_common_freq_topcoded.rds"))

train_dates_max <- sapply(train_dates_combined[,6:ncol(train_dates_combined)], function(x) {max(x, na.rm = TRUE)})

training_freq_max <- sapply(training_freq_combined[,8:ncol(training_freq_combined)], function(x) { max(x, na.rm = TRUE)})

# create extreme vals config files for each dataset 

ex_freq_config <- data.frame(Variable = names(train_freq_max), 
                            Thrsh = training_freq_max)

ex_date_config <- data.frame(Variable = names(train_dates_max), 
                             Thrsh = train_dates_max)
# write out
write_csv(ex_freq_config, paste0(data_dir, "ex_val_caps_freq.csv"))
write_csv(ex_date_config, paste0(data_dir, "ex_val_caps_dates.csv"))


# TOP CODING FUNCTION -----------------------------------------------------

topcode_date_diffs <- function(input, cap, label) {
  # extract index of positives and negatives:
  pos_index <- which(input[[label]] == 1)
  neg_index <- which(input[[label]] == 0)
  # set label to NULL
  input[[label]] <- NULL
  # cap variables:
  capped <- sapply(input, function(x) { 
    # segregate the vector into positives and negatives:
    pos <- x[pos_index]
    neg <- x[neg_index]
    # work out which class has the higher cap:
    quant_pos <- stats::quantile(pos, cap, na.rm = TRUE)
    quant_neg <- stats::quantile(neg, cap, na.rm = TRUE)
    quant <- max(quant_pos, quant_neg, na.rm = TRUE)
    
    # set anything in the dataset above this value to this value
    x[x > quant] <- quant
    
    return(x)
  })
  
  return(as.data.frame(capped))
  
}

# slight alteration for frequencies: only use values above 0 for percentiles:

topcode_frequencies <- function(input, cap, label) {
  # extract index of positives and negatives:
  pos_index <- which(input[[label]] == 1)
  neg_index <- which(input[[label]] == 0)
  # set label to NULL
  input[[label]] <- NULL
  # cap variables:
  capped <- sapply(input, function(x) { 
    # segregate the vector into positives and negatives:
    pos <- x[pos_index]
    neg <- x[neg_index]
    # work out which class has the higher cap:
    quant_pos <- stats::quantile(pos[pos > 0], cap, na.rm = TRUE)
    quant_neg <- stats::quantile(neg[neg > 0], cap, na.rm = TRUE)
    quant <- max(quant_pos, quant_neg, na.rm = TRUE)
    
    # set anything in the dataset above this value to this value
    x[x > quant] <- quant
    
    return(x)
  })
  
  return(as.data.frame(capped))
  
}
