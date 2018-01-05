#  ------------------------------------------------------------------------
# Create date difference variables for modelling datasets
#  ------------------------------------------------------------------------
rm(list=ls())
gc()
library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
source('F:/Shaun/Strongbridge/Code/strongbridge_ppp/matching_experiments/01_pre_modelling/helper_functions.R')
#base_dir <- 'F:/Projects/Strongbridge/data/raw_data_cohorts/'
#output_dir <- 'F:/Projects/Strongbridge/data/matching_experiments/'

base_dir <- 'F:/Projects/Strongbridge/data/matching_experiments/01_pre_modelling/'

#create list of inputs/outputs for <12 and 13+ month cohorts
inputs_outputs <- list(list(paste(base_dir, '01_lte_12_months_train/', '01_combined_dates_unformatted.rds', sep='/'), paste(base_dir, '01_lte_12_months_train', '01_combined_date_differences.rds', sep='/')),
                       list(paste(base_dir, '02_gt_13_months_train', '01_combined_dates_unformatted.rds', sep='/'), paste(base_dir, '02_gt_13_months_train', '01_combined_date_differences.rds', sep='/')))

# 1. READ IN DATA
for (input_output in inputs_outputs){
  input = input_output[[1]]
  output = input_output[[2]]
  #input <- paste(base_dir, '03_Cohorts_by_variable_type_12_months', 'Pos_dates_MOD_12.rds', sep='/')
  #output <- 
  dates <- readRDS(input)
  
  dates_unform <- dates %>% 
    dplyr::select(-one_of(c('label', 'lookback_date', 'test_patient_id')))
  
  
  # Format dates ------------------------------------------------------------
  
  # deal with the 'S_' variables that are in a different format:
  S_date_diffs <- dplyr::select(dates_unform, dplyr::starts_with("S_")) %>%
    calculate_s_date_diffs(as.yearmon(mdy(dates_unform$index_date)))%>%
      setNames(gsub('EXPDT', 'EXP_DT', paste0(names(.),"DT_DIFF")))
  
  #S_vars_format <- as.data.frame(sapply(S_vars, function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))
  
  #S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))
  
  # add index date column;
  #index_date <- as.yearmon(mdy(dates_unform$index_date))
  
  # convert to yearmonths:
  #S_vars_yearmon <- as.data.frame(sapply(S_vars_dates, as.yearmon))
  
  # create date differences for these variables:
  
  #S_date_diffs <- as.data.frame(sapply(S_vars_yearmon,
  #                                     function(x) {((index_date - x)*12)})) %>%
  #  setNames(gsub('EXPDT', 'EXP_DT', paste0(names(.),"DT_DIFF")))
  
  # convert 'D' G' and 'P' variables to correct format
  dates_form <-  date_format(input_data = dplyr::select(dates_unform, -dplyr::starts_with("S_")),
                             date_pattern = "_EXP_DT",
                             PATIENT_ID_col = "PATIENT_ID")
  
  
  # add index date column for creation of date diffs
  dates_form$index_date <- mdy(dates_unform$index_date)
  
  # create date difference columns
  date_differences <- create_date_diffs(input = dplyr::select(dates_form, -one_of('PATIENT_ID')),
                                        index_col = "index_date") %>%
                      setNames(gsub('EXP_DT$',"EXP_DT_DIFF", names(.))) %>%
                      setNames(gsub('EXP_$',"EXP_DT_DIFF", names(.))) %>%
                      setNames(gsub('EXP$',"EXP_DT_DIFF", names(.)))  
  # add necessary columns
  date_diffs_combined <- data.frame(dplyr::select(dates, c('label', 'lookback_date', 'PATIENT_ID', 'index_date', 'test_patient_id')),
                                    date_differences,
                                    S_date_diffs) 
  
  
  
  # prop missing
  #length(date_diffs_combined[is.na(date_diffs_combined[,6:ncol(date_diffs_combined)])])/((ncol(date_diffs_combined)-6) * nrow(date_diffs_combined))
  
  write_rds(date_diffs_combined, output)
  
}
