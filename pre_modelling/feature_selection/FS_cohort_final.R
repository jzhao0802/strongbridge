library(tidyverse)

# -------------------------------------------------------------
# Read in positve cohorts 
# -------------------------------------------------------------

x_init <- 
  read.csv(
    "F:/Projects/Strongbridge/data/pos_cohort_20170420/Strongbridge_Initial Cohort_Update_20170503.csv",
    stringsAsFactors=FALSE)
pos_orig <- 
  read_csv(
    "F:/Projects/Strongbridge/data/pos_cohort_20170607/Positive Cohort_v2_20170607.txt",
    col_types = do.call(cols, list("PATIENT_ID" = "n")))

# -------------------------------------------------------------
# Apply initial criteria
# -------------------------------------------------------------

x_pos <- x_init %>% 
  filter(as.Date(as.character(FIRST_PPP_EXPOSURE_DATE),format="%d/%m/%Y") 
         > as.Date("01/01/2012",format="%m/%d/%Y")) %>% 
  filter(PATIENT_AGE_AT_TIME_OF_FIRST_EXPOSURE > 0) %>% 
  filter((LRx_FLAG == "Y") & (Dx_FLAG == "Y")) %>% 
  filter((PATIENT_GENDER == "M") | (PATIENT_GENDER == "F")) %>%
  filter(AVAILABLE_LOOKBACK_MONTHS >= 24) %>% 
  filter(!(EXCLUSION_1 | EXCLUSION_2 | EXCLUSION_3))

# -------------------------------------------------------------
# Apply stratification criteria
# -------------------------------------------------------------

x_pos_strat <- merge(x_pos, pos_orig, by = "PATIENT_ID", all.x=TRUE) %>% 
  rename (
    S_DR_FLAG = S_DIAGNOSTIC_RADIOLOGY_FLAG, 
    S_CD_FLAG = S_CARDIOVASCULAR_DISEASE_FLAG, 
    S_NR_FLAG = S_NEURORADIOLOGY_FLAG, 
    S_NE_FLAG = S_NEUROLOGY_FLAG,
    D_7288_Flag2 = D_7288_FLAG, 
    S_EN_FLAG = S_ENDOCRINOLOGY__DIABETES___META
  ) %>% 
  
  filter(
    D_MW_FLAG | D_NONPPP_FLAG | G_594000_FLAG | S_PT_FLAG | S_PS_FLAG | D_7288_Flag2 |
      S_NE_FLAG | G_726000_FLAG | C_93010_FLAG  | D_PS_FLAG | S_CD_FLAG | S_NR_FLAG    |
      S_EN_FLAG | G_581600_FLAG | G_751000_FLAG | D_7194_FLAG 
  )

# -------------------------------------------------------------
# Create Narrow cohort for modelling - C06
# -------------------------------------------------------------
x_pos_narrow <- x_pos_strat %>%
  filter( 
    (SELECTION_1B == 1 | SELECTION_2B == 1) 
  ) %>% 
  merge(pos_orig, by="PATIENT_ID")

# 1553 patients
