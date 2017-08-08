
#  ------------------------------------------------------------------------
# Top code the clustering data --------------------------------------------
#  ------------------------------------------------------------------------
library(dplyr)
library(readr)
library(strongr)

path <- "F:/Projects/Strongbridge/data/feature_selection/"

train <- readRDS(paste0(path, "train_clustering_rm_3592.rds"))

test <- read_rds(paste0(path, "test_clustering_rm_3592.rds"))

train_common <- train[,c(1:6, 1687)]
test_common <- test[,c(1:6, 1687)]

train_freq <- train[, -c(1:6, 1687)]
test_freq <- test[, -c(1:6, 1687)]

train_p99 <- topcode(input = train_freq, cap = 0.99)
test_p99 <- topcode(input = test_freq, cap = 0.99)

train_capped <- data.frame(train_common, train_p99)
test_capped <- data.frame(test_common, test_p99)

write_rds(train_capped, paste0(path, "train_clustering_rm_3592_capped_p99.rds"))
write_rds(test_capped, paste0(path, "test_clustering_rm_3592_capped_p99.rds"))
