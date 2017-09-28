library(tidyverse)

#-------------------------------------------------------------------------------
# Read in the data
#-------------------------------------------------------------------------------
path = "F:/Projects/Strongbridge/results/modelling/XGBOOST_advanced/02_XGB_optimal_HP/"

df <- readRDS(paste0(path,"XGB_adv_test_predictions_1_1000.rds"))

vars <- readRDS("F:/Projects/Strongbridge/data/modelling/Advanced_model_data/05_combined_train_unmatched_test_capped_freq_datediff.rds")


#-------------------------------------------------------------------------------
# Run relative risks
#-------------------------------------------------------------------------------
count = 12
rel_risks <- vector("list", count)  

rel_risks[[1]] <- relative_risk(rr_col = "AGE", bin_size_o = 10, c_min_o = 0, include_lowest = F)
rel_risks[[2]] <- relative_risk(rr_col = "GENDER", bin_size_o = 1, include_lowest = F)

rel_risks[[3]] <- relative_risk(rr_col = "D_2768_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[4]] <- relative_risk(rr_col = "G_797000_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[5]] <- relative_risk(rr_col = "S_S44_AVG_CLAIM", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[6]] <- relative_risk(rr_col = "G_371000_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[7]] <- relative_risk(rr_col = "P_99214_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[8]] <- relative_risk(rr_col = "D_7807_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[9]] <- relative_risk(rr_col = "D_7288_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[10]] <- relative_risk(rr_col = "D_4019_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[11]] <- relative_risk(rr_col = "D_3449_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)
rel_risks[[12]] <- relative_risk(rr_col = "P_99204_AVG_CLAIM_CNT", bin_size_o = 1, c_min_o = 0, include_lowest = F)

rel_risks_all <- rel_risks[[1]]
for (i in 2:count) {
  rel_risks_all <- rbind(rel_risks_all, rel_risks[[i]])
}
rel_risks_all <- rel_risks_all %>% arrange(Name)
write.csv(rel_risks_all, paste0(path, "XGB_adv_relative_risks.csv") , 
          row.names = F)

#-------------------------------------------------------------------------------
# Function
#-------------------------------------------------------------------------------
relative_risk <- function(rr_col, prob_col="prob.1", bin_num=5, 
                          include_lowest=TRUE, bin_size_o=NA, c_min_o=NA){
  
  df2 = merge(df, vars %>% select(one_of("PATIENT_ID", rr_col)), by="PATIENT_ID", all.x=T)
  df2 = df2[,c(rr_col, prob_col)] 
  colnames(df2) = c("col", "prob_1")
  
  #remove NA variables
  df2 <- df2[!is.na(df2$col),]
  df2$col <- as.numeric(df2$col)
  
  # define bin size based on range and bin_num
  c_max = max(df2$col)
  c_min = min(df2$col)
  c_range = c_max - c_min 
  if (c_range > bin_num){
    bin_size = round(c_range/bin_num)
  }else{
    # we need cutpoints with decimal places
    bin_size = round(c_range/bin_num, 1)
  }
  
  if(!is.na(bin_size_o)) {
    bin_size = bin_size_o
  }
  
  if(!is.na(c_min_o)) {
    c_min = c_min_o
  }
  
  # define cuts, if the last threshold not the max add another interval
  cuts = seq(c_min, c_max, bin_size)
  if (max(cuts) != c_max){
    cuts = c(cuts, max(cuts) + bin_size)
  }
  
  # bin variable using cuts
  df2 = df2 %>%
    mutate(x_binned=cut(col, breaks=cuts, include.lowest=include_lowest, dig.lab=5)) %>%
    group_by(x_binned) %>%
    summarise(avg_prob=mean(prob_1), n=n()) %>%
    arrange(as.numeric(x_binned)) %>%
    mutate(Name = paste(rr_col), x_binned = as.character(x_binned)) %>%
    select(Name, x_binned, avg_prob, n)
  df2 = as.data.frame(df2)
  if (!include_lowest){
    # replace NAs (lowest values of var that were excluded) with c_min
    df2$x_binned[is.na(df2$x_binned)] = as.character(c_min)
    # reorder so that the lowest_name row is the first in the table
    df2 = df2[c(nrow(df2), 1:nrow(df2)-1),]
    row.names(df2) <- 1:nrow(df2)
  }
  df2 <- df2 %>% mutate(RR = avg_prob/.$avg_prob[1])
  df2
}



############################
# Checks

ggplot(vars, aes(x=D_V053_AVG_CLAIM_CNT, fill=as.factor(label))) + geom_density()
x <- vars %>% select(D_V053_AVG_CLAIM_CNT, label) %>% 
  mutate( D_V053_AVG_CLAIM_CNT2 = round(D_V053_AVG_CLAIM_CNT, 1), labelf=as.numeric(label)-1) %>%
  group_by(D_V053_AVG_CLAIM_CNT2) %>% 
  summarise(pos=sum(labelf), neg=n()-pos, precision=pos/n()*100)


