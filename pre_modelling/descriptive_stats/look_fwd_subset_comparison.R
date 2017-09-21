library(tidyverse)
library(lubridate)

# For 12 month lookback
path <- "F:/Projects/Strongbridge/data/Cohorts/03_Cohorts_by_variable_type_12_months/"
outdir <- "F:/Projects/Strongbridge/results/descriptive_stats/12_month_lookback/"
suffix <- "_MOD_12"

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------

flag_pos <- read_rds(paste0(path, "Pos_flags", suffix, ".rds"))
flag_pos <- sapply(flag_pos, as.numeric) %>% data.frame

forward <- read_csv(paste0(path, "Lookforward_C06_12Months_PatientID.csv"))
flag_pos_fwd <- flag_pos %>% merge(forward, all.y = T, by = "PATIENT_ID")

# ---------------------------------------------------------
# Flags
# ---------------------------------------------------------

pos_flag_counts <- flag_count(flag_pos, "Pos_")
posfwd_flag_counts <- flag_count(flag_pos_fwd, "PosFwd_")

all_flag_counts <- merge(pos_flag_counts, posfwd_flag_counts, by = "Variable", all.x = T, all.y = T)
all_flag_counts[is.na(all_flag_counts)] <- 0
all_flag_counts <- all_flag_counts %>%
  mutate(Difference_in_Prop = abs(Pos_Above_0_Prop - PosFwd_Above_0_Prop)
         ) %>% 
  mutate(Variable_Stem = strsplit(Variable, "_FLAG") %>% as.character())

write.csv(all_flag_counts, paste0(outdir, "compare_pos12lb_pos12fwd.csv"))


# ---------------------------------------------------------
# Functions
# ---------------------------------------------------------


flag_count <- function(df, label) {
  num_cols <- length(names(df))
  
  counts <- data.frame(matrix(nrow = num_cols, ncol = 3))
  colnames(counts)[1] <- "Variable"
  colnames(counts)[2] <- paste0(label, "Above_0")
  colnames(counts)[3] <- paste0(label, "Above_0_Prop")
  
  for (i in 1:num_cols) {
    counts[i,1] <- names(df)[i]
    counts[i,2] <- sum((df[,i] > 0))
    counts[i,3] <- counts[i,2] / dim(df)[1]
  }
  
  counts[is.na(counts)] <- 0
  
  return(counts)
}
