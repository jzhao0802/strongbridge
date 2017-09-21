library(tidyverse)
library(lubridate)
install.packages("F:/PALab_Package/Current_Version/palab_1.0.0.tar.gz", repos = NULL)
library(palab)
library(ggplot2)

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------

path <- "F:/Projects/Strongbridge/data/modelling/"

dfreq <- read_rds(paste0(path, "01_train_combined_common_frequencies.rds")) %>% 
  select(-test_patient_id, -index_date, -lookback_date)
dfreq <- sapply(dfreq, as.numeric) %>% data.frame

ddate <- read_rds(paste0(path, "01_train_combined_date_differences.rds")) %>%
  select(-test_patient_id, -index_date, -lookback_date)
ddate <- sapply(ddate, as.numeric) %>% data.frame

dall <- merge(dfreq, ddate %>% select(-label), by = "PATIENT_ID")

# ---------------------------------------------------------
# Run bivariate stats
# ---------------------------------------------------------

b <- bivar_stats_y_flag(
      prefix="new",
      input = dall, 
      var_config = paste0(path,"sb_var_config.csv"), 
      outcome_var = "label", 
      vargt0 = FALSE,
      output_dir = "F:/Projects/Strongbridge/results/descriptive_stats/bivariate_stats/"
    )

ba <- bivariate_stats_cat(
  prefix="newcaps",
  input = dall, 
  var_config = paste0(path,"sb_var_config.csv"), 
  outcome_var = "label",
  vargt0 = TRUE,
  output_dir = "F:/Projects/Strongbridge/results/descriptive_stats/bivariate_stats/" 
)

# ---------------------------------------------------------
# Checks
# ---------------------------------------------------------


test <- dall %>% select(PATIENT_ID, starts_with("D_2768")) %>% filter(D_2768_AVG_CLAIM_CNT > 0)

test <- dall %>% filter(S_S83_FIRST_EXP < 0) %>% select(PATIENT_ID, S_S83_LAST_EXP_, S_S83_FIRST_EXP)

d <- read_rds("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/Pos_dates_MOD.rds")
dn <- read_rds("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/Neg_dates_MOD.rds")
c <- read_rds("F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Modelling/Pos_claims_MOD.rds")

ds <- filter(d, PATIENT_ID == "47438340"| PATIENT_ID == "48556661") %>% 
  select(PATIENT_ID, index_date, lookback_date, starts_with("S_S83"))
dd <- filter(ddate, PATIENT_ID == "47438340"| PATIENT_ID == "48556661") %>%
  select(PATIENT_ID, starts_with("S_S83"))

ds <- filter(d, PATIENT_ID == "37734629"| PATIENT_ID == "1420959910") %>% 
  select(PATIENT_ID, index_date, lookback_date, label, starts_with("S_S24"))
dd <- filter(ddate, PATIENT_ID == "37734629"| PATIENT_ID == "1420959910") %>%
  select(PATIENT_ID, starts_with("S_S24"))
dc <- filter(c, PATIENT_ID == "37734629"| PATIENT_ID == "1420959910") %>%
  select(PATIENT_ID, starts_with("S_S83"))

x <- ddate %>% select(S_S24_LAST_EXP_, label) %>% filter(label==1) 
plot(table(x$S_S24_LAST_EXP_), xlim = c(0,100))
table(x$S_S24_LAST_EXP_[x$S_S24_LAST_EXP_ < 50])

test <- ddate %>% filter(S_S44_LAST_EXP_ == 0) %>% select(PATIENT_ID, S_S44_LAST_EXP_, S_S44_FIRST_EXP, label)

test2 <- merge(test, dn %>% select(PATIENT_ID, S_S44_LAST_EXP_, index_date), by="PATIENT_ID") %>%
    mutate(day = day(mdy(index_date)))


ggplot(ddate, aes(x=S_S44_LAST_EXP_,  fill=as.factor(label))) + geom_density(alpha=.3)

ddate2 %>% group_by(S_S44_LAST_EXP_) %>% summarise(pos=sum(label), neg=(n()-pos)) %>% View
ddate$S_S44_LAST_EXP_ %>% table

ddate2 <- read_rds(paste0(path, "Archive/01_train_combined_date_differences.rds"))

ggplot(ddate2, aes(x=round(S_S44_LAST_EXP_, 3), ..count.., fill=as.factor(label))) + 
  geom_bar() + xlim(0,180)
ddate2 %>% group_by(S_S44_LAST_EXP_) %>% summarise(pos=sum(label), neg=(n()-pos)) %>% View

ggplot(ddate , aes(x=round(S_S44_LAST_EXP_, 3), ..count.., fill=as.factor(label))) + 
  geom_bar() + xlim(0,4)
ddate %>% group_by(round(S_S44_LAST_EXP_, 3)) %>% summarise(pos=sum(label), neg=(n()-pos)) %>% View

