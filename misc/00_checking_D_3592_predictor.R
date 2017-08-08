
#  ------------------------------------------------------------------------
# Check D_3592 predictor --------------------------------------------------
#  ------------------------------------------------------------------------

library(tidyverse)

# read in positive cohort:

Pos_cohort <- read_csv("F:/Lachlan/Strongbridge/Positive_Cohort_1.csv",
                       col_types = (cols(PATIENT_ID = col_character(), .default = col_guess())))

Pos_flags <- read_rds("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Pos_flags.rds")

Pos_flags_FS <- read_rds("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/Pos_flags_FS.rds")

Pos_flags_Mod <- read_rds("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/Pos_flags_MOD.rds")

D_3592 <- data.frame(PATIENT_ID = Pos_flags$PATIENT_ID,
                     D_3592_FLAG = Pos_flags$D_3592_FLAG)

D_3592$PATIENT_ID <- as.character(D_3592$PATIENT_ID)

Pos_join <- inner_join(D_3592, Pos_cohort, "PATIENT_ID")

Pos_join_FS <- inner_join(Pos_flags_FS, Pos_cohort, "PATIENT_ID")

table(Pos_join$D12)

# number of patients in positive cohort who have D12 but not D11:
nrow(filter(Pos_cohort, D11 == "N" & D12 == "Y"))
# 4738

# out of these, how many in the final cohort had D12 but not D11:
nrow(filter(Pos_join, D11 == "N" & D12 == "Y"))
# 1008

# out of the 4738, how many in the FS cohort had D12 but not D11:
nrow(filter(Pos_join_FS, D11 == "N" & D12 == "Y"))
# 932
# out of these 932, 272 of them had the D_3592_FLAG
nrow(filter(Pos_join_FS, D11 == "N" & D12 == "Y" & D_3592_FLAG.x == 0))

# how many FS patients had D11 = N, D12 = N and D_3592 = 1?
nrow(filter(Pos_join_FS, D11 == "N" & D12 == "N"))


