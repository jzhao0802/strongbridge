library(tidyverse)

path <- "F:/Projects/Strongbridge/data/pos_cohort_20170420/"
x_init <- read.csv(paste0(path, "Strongbridge_Initial Cohort_Update_20170503.csv"),stringsAsFactors=FALSE)
x_data <- read.csv(paste0(path, "Positive Cohort_v1_20170425.csv"),stringsAsFactors=FALSE)

# Apply initial criteria
x_pos <- x_init %>% 
  filter(as.Date(as.character(FIRST_PPP_EXPOSURE_DATE),format="%d/%m/%Y") 
         > as.Date("01/01/2012",format="%m/%d/%Y")) %>% 
  filter(PATIENT_AGE_AT_TIME_OF_FIRST_EXPOSURE > 0) %>% 
  filter((LRx_FLAG == "Y") & (Dx_FLAG == "Y")) %>% 
  filter((PATIENT_GENDER == "M") | (PATIENT_GENDER == "F")) %>%
  filter(AVAILABLE_LOOKBACK_MONTHS >= 24)

# Narrow cohort
x_pos_narrow <- x_pos %>%
  filter( 
    (SELECTION_1B == 1 | SELECTION_2B == 1) &
      !(EXCLUSION_1 | EXCLUSION_2 | EXCLUSION_3)
  ) %>% 
  merge(x_data, by="PATIENT_ID")

# FS cohort 1 (Wide prime with no exclusions)
x_pos_wide <- x_pos %>%   
  filter((CLEAN_PPP_DX_FLAG == "Y") |
           ((DIRTY_PPP_DX_FLAG == "Y") & (TREATED_FLAG == "Y")))

x_pos_wide_p <-  anti_join(x_pos_wide, x_pos_narrow, by = "PATIENT_ID") %>% 
  merge(x_data, by="PATIENT_ID")


# FS cohort 2 (Wide prime with exclusions)
x_pos_widex2 <- x_pos %>%   
  filter((CLEAN_PPP_DX_FLAG == "Y") |
           ((DIRTY_PPP_DX_FLAG == "Y") & (TREATED_FLAG == "Y"))) %>%
  filter( !(EXCLUSION_1 | EXCLUSION_2 | EXCLUSION_3) )

x_pos_widex2_p <-  anti_join(x_pos_widex2, x_pos_narrow, by = "PATIENT_ID") %>% 
  merge(x_data, by="PATIENT_ID")

# FS cohort 3 (Experiment)
x_pos_expex <- x_pos %>% 
  filter((NUMBER_OF_CLEAN_PPP_DIAGNOSIS_CLAIMS >=1 ) | 
           ((CLEAN_PPP_DX_FLAG == "Y") & (NUMBER_OF_TREATMENT_CLAIMS_FOR_ACETAZOLAMIDE +
                                          NUMBER_OF_TREATMENT_CLAIMS_FOR_POTASSIUM
                                          >=2))) %>%
  filter( !(EXCLUSION_1 | EXCLUSION_2 | EXCLUSION_3) )
x_pos_expex_p <-  anti_join(x_pos_exp, x_pos_narrow, by = "PATIENT_ID") %>% 
  merge(x_data, by="PATIENT_ID")


# UNIVARIATE STATS ON FLAGS
flag_stats <- function(data) {
  df <- data %>% select(ends_with("_FLAG"), ends_with("_Flag"),
    SELECTION_1B, SELECTION_2B, EXCLUSION_1, EXCLUSION_2, EXCLUSION_3, CLEAN_PPP_DX_FLAG, DIRTY_PPP_DX_FLAG, TREATED_FLAG )
  df[is.na(df)] <- 0
  df[df =="Y"] <- 1
  df[df =="N"] <- 0
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

uni_N <- flag_stats(x_pos_narrow)  
uni_WP <- flag_stats(x_pos_wide_p)
uni_WEP <- flag_stats(x_pos_widex_p)  
uni_EEP <- flag_stats(x_pos_expex_p)

uni_comp <- merge(
  uni_N %>% rename(Narrow = Count), 
  uni_WP %>% rename(Wide_p = Count),   by = "Variable") %>% merge(
    uni_WEP %>% rename(Widex_p = Count), by = "Variable") %>% merge(
      uni_EEP %>% rename(Expex_p = Count), by = "Variable")

uni_comp <- uni_comp %>% mutate(
  Narrow_Perc = Narrow/nrow(x_pos_narrow) *100, 
  Wide_p_Perc = Wide_p/nrow(x_pos_wide_p) *100, 
  Widex_p_Perc = Widex_p/nrow(x_pos_widex_p) *100, 
  Expex_p_Perc = Expex_p/nrow(x_pos_expex_p) *100,
  
  Diff_Wide = Wide_p_Perc - Narrow_Perc,
  Diff_Widex = Widex_p_Perc - Narrow_Perc,
  Diff_Expex = Expex_p_Perc - Narrow_Perc
)
uni_comp %>% write.csv("F:/Projects/Strongbridge/results/pos_unistats/compare_uni_stats_20170607.csv")
