library('tidyverse')

cohorts <- c('00_matched_train_unmatched_test', '01_lte_12_months_train', '02_gt_13_months_train', '03_lt_12_gt_13_months_train')
features <- c('freq_only', 'dd_only', 'freq_dd_only')
cohort_names <- c("'Full' Lookback", "12 Month Fixed", "12 Month Offset", "12&13+ Months")
feature_names <- c("FREQ Only", "DD Only", "FREQ+DD")
suffix <- '_new_indexes'
test_strat <- 'standard_5_fold_CV/'
results_folder <- 'F:/Projects/Strongbridge/results/matching_experiments/modelling/'
df <- data.frame(Recall=1:100)
df$Recall <- df$Recall * 0.01

for (i in 1:length(cohorts)){
  for (j in 1:length(features)){
    df_tmp <- readr::read_csv(paste0(results_folder, test_strat, cohorts[[i]], '/', 'PRCurve_XGB_', features[[j]], suffix, '.csv'))
    df[paste(cohort_names[[i]], feature_names[[j]])]  <- df_tmp$prec
  }
}

melted <- reshape2::melt(df, id.vars='Recall')
melted['Feature Set'] <- melted$variable
melted$variable <- NULL
melted$Precision <- melted$value
melted$value <- NULL
library('ggplot2')
library('RColorBrewer')
plot <- ggplot(data=melted, aes(x=Recall, y=Precision, color=`Feature Set`)) + geom_line() + scale_y_continuous(label=scales::percent) +
  scale_x_continuous(label=scales::percent) + scale_fill_brewer(palette="Set3")
ggsave(paste0(results_folder, test_strat, 'PR_Curve_all', suffix, '.png'), plot)
