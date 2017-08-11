
#  ------------------------------------------------------------------------
# Correlations
#  ------------------------------------------------------------------------


path <- "F:/Projects/Strongbridge/data/Cohorts/01_Cohorts_by_variable_type/Feat_selection/"

df <- readRDS(paste0(path, "combined_common_frequencies.rds"))

VI <- read_csv("F:/Projects/Strongbridge/results/feature_selection/XGBoost_method_ex_D_3592/01_XGBoost_importance.csv")

# Set up data for correlations --------------------------------------------

df$PATIENT_ID <- NULL
df$test_patient_id <- NULL
df$AGE <- NULL
df$index_date <- NULL
df$lookback_date <- NULL
df$lookback_days <- NULL
df$gender_dum <- NULL

cor_df <- cor(df, method = "spearman")
View(as.data.frame(cor_df))

write.csv(as.data.frame(cor_df), "F:/Projects/Strongbridge/results/feature_selection/cor_frequencies.csv")

# pick up any variables which occur in the top 400 variables by importance:
# 1. Subset cor_df to only those top 400 vars
cor_df <- as.data.frame(cor_df)
# select top 400 (excluding rows 2 and 33 (AGE and Gender))
top_400 <- VI$Feature[c(1, 3:32, 34:402)]
top_400 
cor_400 <- cor_df %>% select_(.dots = top_400)

# select top correlators
cor_high <- as.data.frame(sapply(cor_400, function(x) { 
  vec <- rep(NA, 25)
  high_cor <- rownames(cor_400)[which(x >= 0.85 & x < 1)]
  if(length(high_cor) == 0) {
    high_cor <- NA
  }
  vec[1:length(high_cor)] <- high_cor
  return(vec)
  }))

write_csv(cor_high, "F:/Projects/Strongbridge/results/feature_selection/cor_with_top_400.csv")





