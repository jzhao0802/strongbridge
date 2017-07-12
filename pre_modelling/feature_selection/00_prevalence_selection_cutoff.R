
#  ------------------------------------------------------------------------
# Creating summary dataframes across predictors. These dataframes express 
# the counts of patients in each cohort by each predictor. A percentage
# of the total patients in the cohort is also given per predictor.
#  ------------------------------------------------------------------------

library(tidyverse)

# DATA IN -----------------------------------------------------------------

data_loc <- "F:/Projects/Strongbridge/data/feature_selection/"

neg_diag <- read_csv(paste0(data_loc, "Diag4_Neg.csv"))
pos_diag <- read_csv(paste0(data_loc, "Diag4_Pos.csv"))

neg_gpi <- read_csv(paste0(data_loc, "GPI6_Neg.csv"))
pos_gpi <- read_csv(paste0(data_loc, "GPI6_Pos.csv"))

neg_proc <- read_csv(paste0(data_loc, "Proc_Neg.csv"))
pos_proc <- read_csv(paste0(data_loc, "Proc_Pos.csv"))

neg_spec <- read_csv(paste0(data_loc, "Spec_Neg.csv"))
pos_spec <- read_csv(paste0(data_loc, "Spec_Pos.csv"))


# REMOVE ROWS WITH NA IN KEY COLUMN ---------------------------------------
neg_diag <-  neg_diag[complete.cases(neg_diag$DIAG4),]
pos_diag <-  pos_diag[complete.cases(pos_diag$DIAG4),]

neg_gpi <-  neg_gpi[complete.cases(neg_gpi$GPI6),]
pos_gpi <-  pos_gpi[complete.cases(pos_gpi$GPI6),]

neg_proc <-  neg_proc[complete.cases(neg_proc$`Procedure Code`),]
pos_proc <-  pos_proc[complete.cases(pos_proc$`Procedure Code`),]

neg_spec <-  neg_spec[complete.cases(neg_spec$Specialty),]
pos_spec <-  pos_spec[complete.cases(pos_spec$Specialty),]

# JOIN TABLES TOGETHER ----------------------------------------------------

join_diag <- full_join(neg_diag, pos_diag, by = "DIAG4")
colnames(join_diag)[3] <- "Positive_Count_of_Patients"

join_gpi <- full_join(neg_gpi, pos_gpi, by = "GPI6")
colnames(join_gpi)[c(3,5)] <- c("Negative_count", "Positive_count")

join_proc <- full_join(neg_proc, pos_proc, by = "Procedure Code")
colnames(join_proc)[c(4,7)] <- c("Negative_count", "Positive_count")

join_spec <- full_join(neg_spec, pos_spec, by = "Specialty")
colnames(join_spec)[c(2,3)] <- c("Negative_count", "Positive_count")


# impute missings with 0 --------------------------------------------------
join_diag[is.na(join_diag)] <- 0
join_gpi[is.na(join_gpi)] <- 0
join_proc[is.na(join_proc)] <- 0
join_spec[is.na(join_spec)] <- 0

# ADD PERCENTAGE COLUMNS --------------------------------------------------

negs <- 271150
pos <- 5423

join_diag$Neg_trigger_rate <- join_diag$Negative_Count_of_Patients/negs
join_diag$Pos_trigger_rate <- join_diag$Positive_Count_of_Patients/pos

join_gpi$Neg_trigger_rate <- join_gpi$Negative_count/negs
join_gpi$Pos_trigger_rate <- join_gpi$Positive_count/pos 

join_proc$Neg_trigger_rate <- join_proc$Negative_count/negs
join_proc$Pos_trigger_rate <- join_proc$Positive_count/pos 

join_spec$Neg_trigger_rate <- join_spec$Negative_count/negs
join_spec$Pos_trigger_rate <- join_spec$Positive_count/pos 

write_csv(join_spec, paste0(data_loc, "join_spec.csv"))

trigger_matrix_diag <- form_trigger_matrix(input = join_diag,
                                      positive_trigger = "Pos_trigger_rate",
                                      negative_trigger = "Neg_trigger_rate")

trigger_matrix_gpi <- form_trigger_matrix(input = join_gpi,
                                           positive_trigger = "Pos_trigger_rate",
                                           negative_trigger = "Neg_trigger_rate")

trigger_matrix_proc <- form_trigger_matrix(input = join_proc,
                                           positive_trigger = "Pos_trigger_rate",
                                           negative_trigger = "Neg_trigger_rate")

trigger_matrix_spec <- form_trigger_matrix(input = join_spec,
                                           positive_trigger = "Pos_trigger_rate",
                                           negative_trigger = "Neg_trigger_rate")




# FUNCTIONS ---------------------------------------------------------------

form_trigger_matrix <- function(input, positive_trigger, negative_trigger) {
  
  trig_matrix <- matrix(nrow=4, ncol = 4)
  colnames(trig_matrix) <- c("neg_0.00", "neg_0.01", "neg_0.02", "neg_0.03")
  rownames(trig_matrix) <- c("pos_0.00", "pos_0.01", "pos_0.02", "pos_0.03")
  
  
  trig_matrix[1,1] <- nrow(input[input[positive_trigger] > 0.00 | input[negative_trigger] > 0.00,])
  trig_matrix[1,2] <- nrow(input[input[positive_trigger] > 0.00 | input[negative_trigger] > 0.01,])
  trig_matrix[1,3] <- nrow(input[input[positive_trigger] > 0.00 | input[negative_trigger] > 0.02,])
  trig_matrix[1,4] <- nrow(input[input[positive_trigger] > 0.00 | input[negative_trigger] > 0.03,])
  
  trig_matrix[2,1] <- nrow(input[input[positive_trigger] > 0.01 | input[negative_trigger] > 0.00,])
  trig_matrix[2,2] <- nrow(input[input[positive_trigger] > 0.01 | input[negative_trigger] > 0.01,])
  trig_matrix[2,3] <- nrow(input[input[positive_trigger] > 0.01 | input[negative_trigger] > 0.02,])
  trig_matrix[2,4] <- nrow(input[input[positive_trigger] > 0.01 | input[negative_trigger] > 0.03,])
  
  trig_matrix[3,1] <- nrow(input[input[positive_trigger] > 0.02 | input[negative_trigger] > 0.00,])
  trig_matrix[3,2] <- nrow(input[input[positive_trigger] > 0.02 | input[negative_trigger] > 0.01,])
  trig_matrix[3,3] <- nrow(input[input[positive_trigger] > 0.02 | input[negative_trigger] > 0.02,])
  trig_matrix[3,4] <- nrow(input[input[positive_trigger] > 0.02 | input[negative_trigger] > 0.03,])
  
  trig_matrix[4,1] <- nrow(input[input[positive_trigger] > 0.03 | input[negative_trigger] > 0.00,])
  trig_matrix[4,2] <- nrow(input[input[positive_trigger] > 0.03 | input[negative_trigger] > 0.01,])
  trig_matrix[4,3] <- nrow(input[input[positive_trigger] > 0.03 | input[negative_trigger] > 0.02,])
  trig_matrix[4,4] <- nrow(input[input[positive_trigger] > 0.03 | input[negative_trigger] > 0.03,])
  

  
  return(trig_matrix)
  
}

# input <- data.frame(neg_perc = c(0, 0.01, 0.02, 0.03, 0.05),
#                 pos_perc = c(0.01, 0.02, 0.03, 0.05, 0.2))
# positive_trigger <- "pos_perc"
# negative_trigger <- "neg_perc"
