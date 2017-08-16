library(tidyverse)
library(lubridate)


# For complete
path <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/"
outdir <- "F:/Projects/Strongbridge/results/descriptive_stats/Complete/"
suffix <- "_MOD"

# For complete FS
path <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection//"
outdir <- "F:/Projects/Strongbridge/results/descriptive_stats/Complete_FS/"
suffix <- "_FS"

# For 12 month lookback
path <- "F:/Projects/Strongbridge/data/Cohorts/03_Cohorts_by_variable_type_12_months/"
outdir <- "F:/Projects/Strongbridge/results/descriptive_stats/12_month_lookback/"
suffix <- "_MOD_12"


# For 13 month plus lookback
path <- "F:/Projects/Strongbridge/data/Cohorts/05_Cohorts_by_variable_type_13_months/"
outdir <- "F:/Projects/Strongbridge/results/descriptive_stats/13_month_plus/"
suffix <- "_MOD_13"

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------

flag_pos <- read_rds(paste0(path, "Pos_flags", suffix, ".rds"))
flag_pos <- sapply(flag_pos, as.numeric) %>% data.frame
flag_neg <- read_rds(paste0(path, "Neg_flags", suffix, ".rds"))
flag_neg <- sapply(flag_neg, as.numeric) %>% data.frame

claims_pos <- read_rds(paste0(path, "Pos_claims", suffix, ".rds"))
claims_pos <- sapply(claims_pos, as.numeric) %>% data.frame
claims_neg <- read_rds(paste0(path, "Neg_claims", suffix, ".rds"))
claims_neg <- sapply(claims_neg, as.numeric) %>% data.frame

freq_pos <- read_rds(paste0(path, "Pos_common_frequencies", suffix, ".rds"))
freq_pos <- sapply(freq_pos, as.numeric) %>% data.frame
freq_neg <- read_rds(paste0(path, "Neg_common_frequencies", suffix, ".rds"))
freq_neg <- sapply(freq_neg, as.numeric) %>% data.frame


dates_pos <- read_rds(paste0(path, "Pos_dates", suffix, ".rds"))

setdiff(colnames(flag_neg), colnames(flag_pos))


# ---------------------------------------------------------
# Flags
# ---------------------------------------------------------

pos_flag_counts <- flag_count(flag_pos, "Pos_")
neg_flag_counts <- flag_count(flag_neg, "Neg_")

all_flag_counts <- merge(pos_flag_counts, neg_flag_counts, by = "Variable", all.x = T, all.y = T)
all_flag_counts[is.na(all_flag_counts)] <- 0
all_flag_counts <- all_flag_counts %>%
  mutate(Difference_in_Prop = abs(Pos_Above_0_Prop - Neg_Above_0_Prop),
         Total_Above_0 = Pos_Above_0 + Neg_Above_0, 
         Total_Above_0_Prop = Total_Above_0 / (nrow(flag_pos) +  nrow(flag_neg)), 
         Precision = Pos_Above_0 / Total_Above_0
  ) %>% 
  mutate(Variable_Stem = strsplit(Variable, "_FLAG") %>% as.character())
write.csv(all_flag_counts, paste0(outdir, "flag_counts.csv"), row.names = F)

# ---------------------------------------------------------
# Claims
# ---------------------------------------------------------

pos_claims_counts <- uni_summary(claims_pos, "Pos_Count_Above_0_")
neg_claims_counts <- uni_summary(claims_neg, "Neg_Count_Above_0_")

all_claims_counts <- merge(pos_claims_counts, neg_claims_counts, by = "Variable", all.x = T, all.y = T) %>% 
  mutate(Variable_Stem = strsplit(Variable, "_CLAIM_CNT") %>% as.character)
all_claims_counts[is.na(all_claims_counts)] <- 0

write.csv(all_claims_counts, paste0(outdir, "claims_counts.csv"), row.names = F)

# ---------------------------------------------------------
# Freq
# ---------------------------------------------------------

pos_freq_counts <- uni_summary(freq_pos, "Pos_Count_Above_0_")
neg_freq_counts <- uni_summary(freq_neg, "Neg_Count_Above_0_")

all_freq_counts <- merge(pos_freq_counts, neg_freq_counts, by = "Variable", all.x = T, all.y = T) %>% 
  mutate(Variable_Stem = strsplit(Variable, "_AVG_CLAIM_CNT") %>% as.character)
all_freq_counts[is.na(all_freq_counts)] <- 0

write.csv(all_freq_counts, paste0(outdir, "freq_counts.csv"), row.names = F)


# ---------------------------------------------------------
# Date differences
# ---------------------------------------------------------

dates_pos_spec <- dates_pos %>% select(contains("FIRST")) %>% select(starts_with("S_"))
dates_pos_normal <- dates_pos %>% select(contains("FIRST")) %>% select(-starts_with("S_"))

df <- data.frame(index_date = mdy(dates_pos$index_date), 
                 lapply(dates_pos_normal, mdy) %>% as.data.frame(), 
                 lapply(dates_pos_spec, function(x) {ymd(paste0(x, "01"))} ) %>% as.data.frame())
date_diffs <- data.frame(PATIENT_ID = dates_pos$PATIENT_ID, 
                         create_date_diffs(df))
date_diffs <- sapply(date_diffs, as.numeric) %>% data.frame

date_summary_results <- uni_summary(date_diffs, "Pos_Dates_") %>% 
  mutate(Variable_Stem = strsplit(Variable, "_FIRST_EXP_DT") %>% as.character %>%
           strsplit("_FIRST_EXP") %>% as.character
  )

write.csv(date_summary_results, paste0(outdir, "Pos_Dates_summary.csv"), row.names = F)

# ---------------------------------------------------------
# Combine all together
# ---------------------------------------------------------

all_res <- merge( all_flag_counts %>% select(-Variable), all_claims_counts %>% select(-Variable), 
                  by = "Variable_Stem", all.x=T, all.y=T)
all_res <- merge( all_res, date_summary_results %>% select(-Variable), 
                  by = "Variable_Stem", all.x=T, all.y=T)

write.csv(all_res, paste0(outdir, "All_Results_merged.csv"), row.names = F)

# ---------------------------------------------------------
# Location and complexity
# ---------------------------------------------------------

pos_flag_counts_lc <- flag_count(loc_com(flag_pos) %>% select(starts_with("L")), "Pos_")
neg_flag_counts_lc <- flag_count(loc_com(flag_neg) %>% select(starts_with("L")), "Neg_")

all_flag_counts_lc <- merge(pos_flag_counts_lc, neg_flag_counts_lc, 
                            by = "Variable", all.x = T, all.y = T)

write.csv(all_flag_counts_lc, paste0(outdir, "location_complexity_flag_counts.csv"), row.names = F)


# ---------------------------------------------------------
# Freq including 0s in means
# ---------------------------------------------------------

pos_freq_counts_inc_0 <- uni_mean(freq_pos, "Pos_Count_")
neg_freq_counts_inc_0 <- uni_mean(freq_neg, "Neg_Count_")

all_freq_counts_inc_0 <- merge(pos_freq_counts_inc_0, neg_freq_counts_inc_0, by = "Variable", all.x = T, all.y = T) %>% 
  mutate(Variable_Stem = strsplit(Variable, "_AVG_CLAIM_CNT") %>% as.character)
all_freq_counts_inc_0[is.na(all_freq_counts_inc_0)] <- 0

write.csv(all_freq_counts_inc_0, paste0(outdir, "freq_counts_inc_0.csv"), row.names = F)


# ---------------------------------------------------------
# Distributions of count variables
# ---------------------------------------------------------

dist_HPJ("D_3449_CLAIM_CNT")
dist_HPJ("D_2768_CLAIM_CNT")
dist_HPJ("G_797000_CLAIM_CNT")

# ---------------------------------------------------------
# Overall counts of predictors
# ---------------------------------------------------------
overall_sum_pos <- data.frame(o_count = 
                                rowSums(flag_pos %>% select(-PATIENT_ID,-label), na.rm = T, dims = 1)
)
pos_o_count <- overall_sum_pos %>% group_by(o_count) %>% summarise(pos_count=n())

overall_sum_neg <- data.frame(o_count = 
                                rowSums(flag_neg %>% select(-PATIENT_ID,-label, -test_patient_id), na.rm = F, dims = 1)
)
neg_o_count <- overall_sum_neg %>% group_by(o_count) %>% summarise(neg_count=n())

all_o_counts <- merge(pos_o_count, neg_o_count, by = "o_count", all.x=T, all.y=T)
all_o_counts[is.na(all_o_counts)] <- 0
write.csv(all_o_counts, paste0(outdir, "overall_count_of_flags.csv"), row.names = F)

# ---------------------------------------------------------
# AD HOC
# ---------------------------------------------------------

flag_pos <- read_rds(paste0(path, "Pos_flags", suffix, ".rds"))
flag_neg <- read_rds(paste0(path, "Neg_flags", suffix, ".rds"))

adhoc <- merge(
  flag_pos %>% group_by(G_797000_FLAG, D_2768_FLAG) %>% summarise(PosCount  = n()), 
  flag_neg %>% group_by(G_797000_FLAG, D_2768_FLAG) %>% summarise(NegCount  = n()),
  by = c("G_797000_FLAG", "D_2768_FLAG")
)




# ---------------------------------------------------------
# Functions
# ---------------------------------------------------------

dist_HPJ <- function(var) {
  
  dist <- merge(
    claims_pos %>% group_by_(.dots = var) %>% summarise(PosCount=n()),
    claims_neg %>% group_by_(.dots = var) %>% summarise(NegCount=n()),
    by = var, all.x = T, all.y = TRUE) %>%
    mutate(
      PosPerc = PosCount / dim(claims_pos)[1],
      NegPerc = NegCount / dim(claims_neg)[1]
    )
  write.csv(dist, 
            paste0(outdir, var, ".csv"),
            row.names = FALSE)
}

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

create_date_diffs <- function(input, index_col = "index_date") {
  
  date_cols <- input[, -which(colnames(input) == index_col)]
  
  date_diffs <- as.data.frame(sapply(date_cols, function(x) { 
    ( input[[index_col]] - x ) / 365
    
  }))
  
  return(date_diffs)
}

uni_summary <- function(df, label) {
  num_cols <- length(names(df))
  
  counts <- data.frame(matrix(nrow = num_cols, ncol = 5))
  colnames(counts)[1] <- "Variable"
  colnames(counts)[2] <- paste0(label, "Min")
  colnames(counts)[3] <- paste0(label, "Median")
  colnames(counts)[4] <- paste0(label, "Mean")
  colnames(counts)[5] <- paste0(label, "Max")
  
  for (i in 1:num_cols) {
    counts[i,1] <- names(df)[i]
    counts[i,2] <- min(df[df[,i] > 0, i], na.rm=T)
    counts[i,3] <- median(df[df[,i] > 0, i], na.rm=T)
    counts[i,4] <- mean(df[df[,i] > 0, i], na.rm=T) 
    counts[i,5] <- max(df[df[,i] > 0, i], na.rm=T) 
  }
  return(counts)
}

uni_mean <- function(df, label) {
  num_cols <- length(names(df))
  
  counts <- data.frame(matrix(nrow = num_cols, ncol = 2))
  colnames(counts)[1] <- "Variable"
  colnames(counts)[2] <- paste0(label, "Mean")
  
  for (i in 1:num_cols) {
    counts[i,1] <- names(df)[i]
    counts[i,2] <- mean(df[, i], na.rm=T) 
  }
  return(counts)
}



loc_com <- function(df) {
  df2 <- df %>% mutate(
    LC_Office_Visit_Other_Outpatient_High =  ifelse( P_99205_FLAG | P_99215_FLAG | P_99245_FLAG, 1, 0), 
    LC_Office_Visit_Other_Outpatient_Low =  ifelse( P_99203_FLAG | P_99211_FLAG | P_99213_FLAG | P_99243_FLAG, 1, 0), 
    LC_Office_Visit_Other_Outpatient_Moderate =  ifelse( P_99204_FLAG | P_99214_FLAG | P_99244_FLAG, 1, 0), 
    LC_Office_Visit_Other_Outpatient_Straightforward =  ifelse( P_99201_FLAG | P_99202_FLAG | P_99212_FLAG | P_99242_FLAG, 1, 0), 
    LC_ER_Visit_High =  ifelse( P_99285_FLAG, 1, 0), 
    LC_ER_Visit_Low =  ifelse( P_99282_FLAG, 1, 0), 
    LC_ER_Visit_Moderate =  ifelse( P_99283_FLAG | P_99284_FLAG, 1, 0), 
    LC_Hospital_High =  ifelse( P_99220_FLAG | P_99223_FLAG | P_99226_FLAG | P_99233_FLAG | P_99236_FLAG | P_99255_FLAG, 1, 0), 
    LC_Hospital_Low =  ifelse( P_99221_FLAG | P_99253_FLAG, 1, 0), 
    LC_Hospital_Moderate =  ifelse( P_99219_FLAG | P_99222_FLAG | P_99225_FLAG | P_99232_FLAG | P_99235_FLAG | P_99254_FLAG, 1, 0), 
    LC_Hospital_Straightforward =  ifelse( P_99252_FLAG, 1, 0), 
    LC_Hospital_Straightforward_Low =  ifelse( P_99231_FLAG, 1, 0), 

    L_Critical_Care =  ifelse( P_99291_FLAG | P_99292_FLAG, 1, 0), 
    L_ER =  ifelse( P_99282_FLAG | P_99283_FLAG | P_99284_FLAG | P_99285_FLAG, 1, 0), 
    L_Hospital =  ifelse( P_99217_FLAG | P_99219_FLAG | P_99220_FLAG | P_99221_FLAG | P_99222_FLAG | P_99223_FLAG | P_99225_FLAG | P_99226_FLAG | P_99231_FLAG | P_99232_FLAG | P_99233_FLAG | P_99235_FLAG | P_99236_FLAG | P_99238_FLAG | P_99239_FLAG | P_99252_FLAG | P_99253_FLAG | P_99254_FLAG | P_99255_FLAG, 1, 0), 
    L_Nursing_Facility =  ifelse( P_99305_FLAG | P_99306_FLAG | P_99307_FLAG | P_99308_FLAG | P_99309_FLAG, 1, 0), 
    L_Office_Other_Outpatient_Visit =  ifelse( P_99201_FLAG | P_99202_FLAG | P_99203_FLAG | P_99204_FLAG | P_99205_FLAG | P_99211_FLAG | P_99212_FLAG | P_99213_FLAG | P_99214_FLAG | P_99215_FLAG | P_99242_FLAG | P_99243_FLAG | P_99244_FLAG | P_99245_FLAG | P_99354_FLAG, 1, 0) 
    
    ) 
}

