
#  ------------------------------------------------------------------------
# QC: STRONGBRIDGE DATA
#  ------------------------------------------------------------------------
# install.packages("F:/Lachlan/palab/palab_1.0.1.tar.gz", repos = NULL)
library(tidyverse)
library(stringr)
library(palab)
library(lubridate)
# LOAD DATA ---------------------------------------------------------------

data_loc <- "F:/Projects/Strongbridge/data/fs_modelling_cohorts/Strongbridge_"

Neg_DX <- read_csv(paste0(data_loc, "Neg_DX_Update20170724.csv"),
                   col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_PA <- read_csv(paste0(data_loc, "Neg_PA.csv"),
                     col_types = (cols(patient_id = col_character(), .default = col_guess())))
Neg_PR <-  read_csv(paste0(data_loc, "Neg_PR_Update20170724.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_RX <-  read_csv(paste0(data_loc, "Neg_RX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_SP <-  read_csv(paste0(data_loc, "Neg_SP.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_DX <-  read_csv(paste0(data_loc, "Pos_DX_Update20170724.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PA <-  read_csv(paste0(data_loc, "Pos_PA.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PR <-  read_csv(paste0(data_loc, "Pos_PR_Update20170724.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_RX <-  read_csv(paste0(data_loc, "Pos_RX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_SP <-  read_csv(paste0(data_loc, "Pos_SP.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

Pos_cohort_test <- read.table("F:/Projects/Strongbridge/data/pos_cohort_20170607/Positive Cohort_v2_20170607.txt",
                              sep = ",", header = TRUE)


# STRANGE CHARACTERS ------------------------------------------------------

file.names <- list.files("F:/Projects/Strongbridge/data/fs_modelling_cohorts/", 
                                 full.names = TRUE)

for (i in file.names) {
  x <- funny_chars_check(i)
  print(x)
}


# A Host of Set operations: -----------------------------------------------

two_way_setdiff(x = colnames(Pos_PA), y = colnames(Neg_PA))
two_way_setdiff(x = colnames(Pos_DX), y = colnames(Neg_DX))
two_way_setdiff(x = colnames(Pos_RX), y = colnames(Neg_RX))
two_way_setdiff(x = colnames(Pos_PR), y = colnames(Neg_PR))
two_way_setdiff(x = colnames(Pos_SP), y = colnames(Neg_SP))


# tables for variable suffixes: -------------------------------------------
# counts, exposure dates:
table(str_sub(colnames(Neg_DX), -5, -1))
table(str_sub(colnames(Pos_DX), -5, -1))
# flags, frequencies, exposure dates:
table(str_sub(colnames(Neg_RX), -5, -1))
table(str_sub(colnames(Pos_RX), -5, -1))
# counts, exposure dates:
table(str_sub(colnames(Neg_PR), -5, -1))
table(str_sub(colnames(Pos_PR), -5, -1))
# flags, exposure dates, average claims, claim count.
table(str_sub(colnames(Neg_SP), -5, -1))
table(str_sub(colnames(Pos_SP), -5, -1))

table(str_sub(colnames(Neg_PA), -5, -1))
table(str_sub(colnames(Pos_PA), -5, -1))

# Missing values:

Miss_Pos_DX <- prop_missing(Pos_DX)
Miss_Pos_RX <- prop_missing(Pos_RX)
Miss_Pos_PR <- prop_missing(Pos_PR)
Miss_Pos_PA <- prop_missing(Pos_PA)
Miss_Pos_SP <- prop_missing(Pos_SP)
Miss_Neg_DX <- prop_missing(Neg_DX)
Miss_Neg_RX <- prop_missing(Neg_RX)
Miss_Neg_PR <- prop_missing(Neg_PR)
Miss_Neg_PA <- prop_missing(Neg_PA)
Miss_Neg_SP <- prop_missing(Neg_SP)


# Plot lookback length:
Neg_lookback <- data.frame(lookback_days = Neg_PA$LOOKBACK_DAYS,
                     label = 0)
summary(Neg_lookback$lookback_days)
Pos_lookback <- data.frame(lookback_days = Pos_PA$lookback_days,
                           label = 1)
summary(Pos_lookback$lookback_days)
Comb_lookback <- bind_rows(Neg_lookback, Pos_lookback)
ggplot(Comb_lookback, aes(x = lookback_days, ..density.., fill = as.factor(label))) + 
  geom_density(alpha = .3)


# Left join cohorts together:
Pos_all <- join_all(list(Pos_PA, Pos_DX, Pos_RX, Pos_PR, Pos_SP), type = "left")
Neg_all <- join_all(list(Neg_PA, Neg_DX, Neg_RX, Neg_PR, Neg_SP), type = "left")


# CHECK DATE VARIABLES ----------------------------------------------------


# POSITIVES ---------------------------------------------------------------
# Convert all dates to correct format:
Pos_dates <- data.frame(PATIENT_ID = Pos_all$PATIENT_ID,
                        select(Pos_all, ends_with("EXP_DT")))
Pos_dates$PATIENT_ID <- as.character(Pos_dates$PATIENT_ID)
Pos_date_format <- date_format(input_data = Pos_dates, date_pattern = "EXP_DT",
                               PATIENT_ID_col = "PATIENT_ID")

Pos_date_format$looback_days <- mdy(Pos_all$final_lookback)
Pos_date_format$index_date <- mdy(Pos_all$final_index)

# summary of lookback and index:
summary(Pos_date_format$looback_days)
summary(Pos_date_format$index_date)

# check that none of these dates occur before the lookback date:
# These are actually the date difference variables:
Date_diffs_pos_df <- as.data.frame(sapply(Pos_date_format[,-1], function(x) { 
  Pos_date_format$index_date - x
  }))
min(Date_diffs_pos_df, na.rm = TRUE)

# compare the lookback date to the first and last exposure dates:
pos_lookback_diff <- as.data.frame(sapply(Pos_date_format[,-1], function(x) {
  x - Pos_date_format$looback_days
}))
min(pos_lookback_diff, na.rm = TRUE)


# NEGATIVES ---------------------------------------------------------------
Neg_dates <- data.frame(PATIENT_ID = Neg_all$PATIENT_ID,
                        select(Neg_all, ends_with("EXP_DT")))
Neg_dates$PATIENT_ID <- as.character(Neg_dates$PATIENT_ID)
Neg_date_format <- date_format(input_data = Neg_dates, date_pattern = "EXP_DT",
                               PATIENT_ID_col = "PATIENT_ID")

Neg_date_format$looback_days <- mdy(Neg_all$lookback_date)
Neg_date_format$index_date <- mdy(Neg_all$index_date)

# summary of lookback and index:
summary(Neg_date_format$looback_days)
summary(Neg_date_format$index_date)

# check that none of these dates occur before the lookback date:
# These are actually the date difference variables:
Date_diffs_neg_df <- as.data.frame(sapply(Neg_date_format[,-1], function(x) { 
  Neg_date_format$index_date - x
  }))
min(Date_diffs_neg_df, na.rm = TRUE)

# compare the lookback date to the first and last exposure dates:
neg_lookback_diff <- as.data.frame(sapply(Neg_date_format[,-1], function(x) {
  x - Neg_date_format$looback_days
}))
min(neg_lookback_diff, na.rm = TRUE)



# NOTE: LOOKBACK COMPARISONS BETWEEN POS AND NEG ARE IN SEPARATE SCRIPT

# FUNCTIONS ---------------------------------------------------------------

# check for funny characters:
funny_chars_check <- function(filename){
  df <- readr::read_csv(filename,n_max = 0)
  names<-colnames(df)
  funny_chars<-c( "\\","&", ".", "-","$", "%", "*", "~", "#", "+", ">", "<", "?", "/",  "|", " ", "'")
  for(name in names){
    for (char in funny_chars){
      if (grepl(char,name, fixed=TRUE)){
        passed=FALSE
        message(sprintf("Test Failed. Some funny characters are there."))
        return(passed)
      }
    }
  }
  passed=TRUE
  message(sprintf("Test Passed. Regular characters in all the columns."))
  return(passed)
}

# check time units:
time_units_check <- function(df1, df2, contains1='Freq', contains2='Freq', all=FALSE){
  if (all==TRUE){
    cols1<-df1
    cols2<-df2
  }else{
    cols1<-dplyr::select(df1, contains(prefix1))
    cols2<-dplyr::select(df2, contains(prefix2))
    if (dim(cols1)[2] <1 | dim(cols2)[2] <1){
      message(sprintf("Error. Could not find columns specified."))
      return(FALSE)
    }
  }
  mean1<- mean(colMeans(cols1))
  mean2<- mean(colMeans(cols2))
  ratio= mean1/mean2
  if(ratio > 1000 | (1/ratio) >1000){
    message(sprintf("Test Failed. Units does not seem the same. 3 orders of magnitude different."))
    return(FALSE)
  } else{
    message(sprintf("Test Passed. Difference in magnitude is less than 3 orders."))
    return(TRUE)
  }
}

# compute set diff both ways and print result:
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

#  proportion missing for each variable:
prop_missing <- function(input_data) {
  
  prop_miss <- sapply(input_data, function(x) {

    sum(is.na(x))/nrow(input_data)
    
  })
  print(summary(prop_miss))
  return(prop_miss)
}

# convert date format:

date_format <- function(input_data, date_pattern, PATIENT_ID_col) {
  date_data <- select(input_data, contains(date_pattern))
  formatted <- lapply(date_data, mdy)
  df_date <- as.data.frame(formatted)
  df <- data.frame(PATIENT_ID = as.character(input_data[PATIENT_ID_col]),
                   df_date
  )
  return(df)
}




