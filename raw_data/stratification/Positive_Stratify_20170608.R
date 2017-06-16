#-------------------------------------------------------------------------------
# Stratifying positive cohort by predefined set of variables
#-------------------------------------------------------------------------------

# For the most recent version of the strongr package:

install.packages("F:/Projects/Strongbridge/strongr_package/strongr_1.0.0.zip", repos = NULL)

library(strongr)
library(dplyr)
library(lubridate)
library(readr)

#-------------------------------------------------------------------------------
# Read in data
#-------------------------------------------------------------------------------

pos_orig <- read_csv("F:/Projects/Strongbridge/data/pos_cohort_20170607/Positive Cohort_v2_20170607.txt",
                    col_types = do.call(cols, list("PATIENT_ID" = "n")))

neg_orig <- read_csv("F:/Projects/Strongbridge/data/scoring_prestrat_20170606/Stratification Radom Sample_20170606.txt", 
                col_types = do.call(cols, list("PATIENT_ID" = "n")))

#-------------------------------------------------------------------------------
# NArrow positive cohort down
#-------------------------------------------------------------------------------

# Read in patient_ids of final 1720 cohort
path <- "F:/Projects/Strongbridge/data/pos_cohort_20170420/"
x_init <- read.csv(paste0(path, "Strongbridge_Initial Cohort_Update_20170503.csv"),stringsAsFactors=FALSE)
# Apply initial criteria
x_pos <- x_init %>% 
  filter(as.Date(as.character(FIRST_PPP_EXPOSURE_DATE),format="%d/%m/%Y") 
         >= as.Date("01/01/2012",format="%m/%d/%Y")) %>% 
  filter(PATIENT_AGE_AT_TIME_OF_FIRST_EXPOSURE > 0) %>% 
  filter((LRx_FLAG == "Y") & (Dx_FLAG == "Y")) %>% 
  filter((PATIENT_GENDER == "M") | (PATIENT_GENDER == "F")) %>%
  filter(AVAILABLE_LOOKBACK_MONTHS >= 24) %>% 
  filter(!(EXCLUSION_1 | EXCLUSION_2 | EXCLUSION_3))

x_pos %>% select(PATIENT_ID, FIRST_PPP_EXPOSURE_DATE, PATIENT_AGE_AT_TIME_OF_FIRST_EXPOSURE, 
                 LRx_FLAG, Dx_FLAG, PATIENT_GENDER, AVAILABLE_LOOKBACK_MONTHS, 
                 EXCLUSION_1, EXCLUSION_2, EXCLUSION_3) %>% 
  write.csv("F:/Projects/Strongbridge/data/pos_cohort_20170420/SB_patients_for_matching.csv", 
            row.names = F)

# Narrow cohort
x_pos_narrow <- x_pos %>%
  filter( 
    (SELECTION_1B == 1 | SELECTION_2B == 1) &
      !(EXCLUSION_1 | EXCLUSION_2 | EXCLUSION_3)
  )

# Filter the positve down
pos_orig$D07 <- mdy(pos_orig$D07)

# filter dataset
filtered2 <- pos_orig %>% filter(D07 > "2012-01-01" &
                                 D03 == "Y" &
                                 D04 == "Y" &
                                 D09 >= 24 &
                                 (D01 == "M" | D01 == "F") &
                                 D02 > 0) %>%
 merge(x_pos_narrow %>% select(PATIENT_ID), by = "PATIENT_ID",all.y = T) %>% tbl_df


#-------------------------------------------------------------------------------
# Keep just the stratification columns
#-------------------------------------------------------------------------------

pos <- filtered2 %>% select(
  C_93010_FLAG,
  D_7194_FLAG,
  D_MW_FLAG,
  D_NONPPP_FLAG,
  G_221000_FLAG,
  G_581600_FLAG,
  G_726000_FLAG,
  G_751000_FLAG,
  S_ER_FLAG,
  S_PS_FLAG,
  S_PT_FLAG,
  D_PS_FLAG,
  G_594000_FLAG,
  S_DIAGNOSTIC_RADIOLOGY_FLAG,
  S_CARDIOVASCULAR_DISEASE_FLAG,
  S_NEURORADIOLOGY_FLAG,
  S_NEUROLOGY_FLAG,
  D_7288_FLAG,
  S_ENDOCRINOLOGY__DIABETES___META) %>% 
  rename (
    S_DR_FLAG = S_DIAGNOSTIC_RADIOLOGY_FLAG, 
    S_CD_FLAG = S_CARDIOVASCULAR_DISEASE_FLAG, 
    S_NR_FLAG = S_NEURORADIOLOGY_FLAG, 
    S_NE_FLAG = S_NEUROLOGY_FLAG,
    D_7288_Flag2 = D_7288_FLAG, 
    S_EN_FLAG = S_ENDOCRINOLOGY__DIABETES___META
  ) %>% 
  mutate(label = 1L)
  
neg <- neg_orig %>% select(
  C_93010_FLAG,
  D_7194_FLAG,
  D_MW_FLAG,
  D_NONPPP_FLAG,
  G_221000_FLAG,
  G_581600_FLAG,
  G_726000_FLAG,
  G_751000_FLAG,
  S_ER_FLAG,
  S_PS_FLAG,
  S_PT_FLAG,
  D_PS_FLAG,
  G_594000_FLAG,
  S_DR_FLAG,
  S_CD_FLAG,
  S_NR_FLAG,
  S_NE_FLAG,
  D_7288_Flag2,
  S_EN_FLAG) %>% 
  mutate(label = 0L)

# Impute missing values with 0:
neg[is.na(neg)] <- 0
pos[is.na(pos)] <- 0

#-------------------------------------------------------------------------------
# Stratify dataset
#-------------------------------------------------------------------------------

strat_var_loc <- "F:/Projects/Strongbridge/data/stratification_criteria/"
pred <- read.csv(paste0(strat_var_loc, "Strat_vars_20170608.txt"), stringsAsFactors=FALSE)
pred <- read.csv(paste0(strat_var_loc, "Strat_vars_20170608_2.txt"), stringsAsFactors=FALSE)
pred <- read.csv(paste0(strat_var_loc, "Strat_vars_20170608_3.txt"), stringsAsFactors=FALSE)
pred <- read.csv(paste0(strat_var_loc, "Strat_vars_20170608_4.txt"), stringsAsFactors=FALSE)


top_preds <- pred$top_preds[1:nrow(pred)]

# variables for storing results
pos_counts <- c()
pos_percs <- c()
neg_counts <- c()
neg_percs <- c()

# calculate counts and percentages for the top rows of required table
for (i in 1:length(top_preds)){
  print(i)
  preds <- top_preds[1:i]
  rule_check(preds)
  print(pos_percs[i])
  print(neg_percs[i])
}

# merge results and save them
results007 = data.frame(pred, pos_counts, neg_counts, pos_percs, neg_percs)



#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------

# function to do the checking/counting
rule_check <- function(preds){
  pos_count <- sum(apply(pos[,preds] >= 1, 1, sum) > 0)
  neg_count <- sum(apply(neg[,preds] >= 1, 1, sum) > 0)
  pos_counts <<- c(pos_counts, pos_count)
  neg_counts <<- c(neg_counts, neg_count)
  pos_percs <<- c(pos_percs, pos_count / dim(pos)[1] * 100)
  neg_percs <<- c(neg_percs, neg_count / dim(neg)[1] * 100)
}


x <- pos[,preds] >= 1

flag_stats <- function(data) {
  
  df <- data
  ncols <- length(colnames((df)))
  
  counts <- as.data.frame(matrix(ncol=2, nrow=ncols))
  
  colnames(counts)[1] <- "Variable"
  colnames(counts)[2] <- "Count"
  
  for (i in 1:ncols) {
    counts[i,1] <- colnames(df)[i]
    counts[i,2] <- sum(df[,i] > 0)
  }
  return(counts)
}

uni_pos <- flag_stats(pos) 
uni_neg <- flag_stats(neg) 

