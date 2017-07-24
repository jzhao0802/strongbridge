
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

Neg_DX <- read_csv(paste0(data_loc, "Neg_DX.csv"),
                   col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_PA <- read_csv(paste0(data_loc, "Neg_PA.csv"),
                     col_types = (cols(patient_id = col_character(), .default = col_guess())))
Neg_PR <-  read_csv(paste0(data_loc, "Neg_PR.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_RX <-  read_csv(paste0(data_loc, "Neg_RX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Neg_SP <-  read_csv(paste0(data_loc, "Neg_SP.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_DX <-  read_csv(paste0(data_loc, "Pos_DX.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PA <-  read_csv(paste0(data_loc, "Pos_PA.csv"),
                    col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))
Pos_PR <-  read_csv(paste0(data_loc, "Pos_PR.csv"),
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
# common vars, lookback date/length and index date
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


# ALL DATE VARS IN SAME FORMAT --------------------------------------------

Pos_DX_dates <- data.frame(PATIENT_ID = Pos_DX$PATIENT_ID, select(Pos_DX, ends_with("EXP_DT")))
Pos_PR_dates <- data.frame(PATIENT_ID = Pos_PR$PATIENT_ID, select(Pos_PR, ends_with("EXP_DT")))
Pos_RX_dates <- data.frame(PATIENT_ID = Pos_RX$PATIENT_ID, select(Pos_RX, ends_with("EXP_DT")))

Neg_DX_dates <- data.frame(PATIENT_ID = Neg_DX$PATIENT_ID, select(Neg_DX, ends_with("EXP_DT")))
Neg_PR_dates <- data.frame(PATIENT_ID = Neg_PR$PATIENT_ID, select(Neg_PR, ends_with("EXP_DT")))
Neg_RX_dates <- data.frame(PATIENT_ID = Neg_RX$PATIENT_ID, select(Neg_RX, ends_with("EXP_DT")))

# Convert all dates to correct format:
Pos_DX_date_form <- date_format(input_data = Pos_DX_dates, date_pattern = "EXP_DT")
Pos_PR_date_form <- date_format(input_data = Pos_PR_dates, date_pattern = "EXP_DT")
Pos_RX_date_form <- date_format(input_data = Pos_RX_dates, date_pattern = "EXP_DT")
Neg_DX_date_form <- date_format(input_data = Neg_DX_dates, date_pattern = "EXP_DT")
Neg_PR_date_form <- date_format(input_data = Neg_PR_dates, date_pattern = "EXP_DT")
Neg_RX_date_form <- date_format(input_data = Neg_RX_dates, date_pattern = "EXP_DT")

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

date_format <- function(input_data, date_pattern) {
  date_data <- select(input_data, contains(date_pattern))
  formatted <- lapply(date_data, mdy)
  df_date <- as.data.frame(formatted)
  df <- data.frame(select(input_data, -contains(date_pattern)),
                   df_date
                   )
  return(df)
}

# function to check that no dates are after the index date or



