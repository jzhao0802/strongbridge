#-------------------------------------------------------------------------------
# Stratifying positive cohort by predefined set of variables
#-------------------------------------------------------------------------------

# for the most recent version of the strongr package:
install.packages("F:/Projects/Strongbridge/strongr_package/strongr_1.0.0.tar.gz", repos = NULL)

install.packages("F:/Projects/Strongbridge/strongr_package/strongr_1.0.0.zip", repos = NULL)

detach(strongr)
library(strongr)
library(dplyr)
library(lubridate)
library(readr)

#-------------------------------------------------------------------------------
# Data and variable input
#-------------------------------------------------------------------------------

pos_raw <- read_csv("F:/Projects/Strongbridge/data/pos_cohort_20170420/Positive Cohort_v1_20170425.csv",
                     col_types = do.call(cols, list("PATIENT_ID" = "n")))

strat_variables <- read_csv("F:/Projects/Strongbridge/data/stratification_criteria/Strat_vars_15_05_2017NL.csv")

#-------------------------------------------------------------------------------
# Filter dataset
#-------------------------------------------------------------------------------

# correct date type
pos_raw$D07 <- mdy(pos_raw$D07)

# filter dataset
filtered <- pos_raw %>% filter(D07 > "2012-01-01" &
                                D03 == "Y" &
                                D04 == "Y" &
                                D09 >= 12 &
                                (D01 == "M" | D01 == "F") &
                                D02 > 0)


# impute missing values with 0:
pos_raw[is.na(pos_raw)] <- 0



#-------------------------------------------------------------------------------
# Stratify dataset
#-------------------------------------------------------------------------------

# define the list of variables for use in stratification
strats <- strat_variables$Variable_name_in_extract[1:17]
# correct the name of one of the variables
strats[14] <- paste0(strats[14], "_FLAG")
strats[12] <- "G_581600_FLAG"
# stratify dataset
stratified <- stratify_data(input = filtered,
                            strat_vars = strats,
                            output_dir = "F:/Projects/Strongbridge/results/stratification_20170606",
                            output_csv = TRUE,
                            prefix = "strongbridge_v1")

#-------------------------------------------------------------------------------
# Other
#-------------------------------------------------------------------------------

# test of function. Provided the dataset hasn't changed, this should have 4807 rows
# strat_test <- filtered %>% filter(S_DIAGNOSTIC_RADIOLOGY_FLAG > 0 |
#                                     S_NEUROLOGY_FLAG > 0 |
#                                   S_NEURORADIOLOGY_FLAG > 0 |
#                                     S_ENDOCRINOLOGY__DIABETES___META > 0
#                                   )

