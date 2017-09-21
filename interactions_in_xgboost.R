library(xgboost)
library(tidyverse)
library(mlr)

path = "F:/Projects/Strongbridge/results/modelling/XGBOOST_preliminary/No_specialities/"

# Read in model
bst <- readRDS(paste0(path, "XGB_preliminary_model.rds"))

importance <- xgb.importance(feature_names = bst$features, model = bst$learner.model)
head(importance)

xgb.plot.tree(model = bst$learner.model, n_first_tree = 0, feature_names = bst$features)
xgb.dump(bst, with_stats = T)

featureList <- bst$features
featureVector <- c() 
for (i in 1:length(featureList)) { 
  featureVector[i] <- paste(i-1, featureList[i], "q", sep="\t") 
}
write.table(
  featureVector, paste0(path, "fmap.txt"), 
  row.names=FALSE, quote = FALSE, col.names = FALSE)
xgb.dump(
  model = bst$learner.model, 
  fname = paste0(path, "xgb.dump"), 
  fmap = paste0(path, "fmap.txt"), 
  with_stats = TRUE)


# Exploring the interactions
df <- readRDS("F:/Projects/Strongbridge/data/modelling/01_train_combined_common_freq_topcoded.rds")

g = ggplot(df, aes(x=AGE, 
                   y=D_2768_AVG_CLAIM_CNT , 
                   ..count.., 
                   color=as.factor(label), 
                   size=as.factor(label)
                   ))
g + geom_point(alpha=0.3)

