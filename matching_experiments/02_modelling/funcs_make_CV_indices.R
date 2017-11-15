#Function to convert patient ids to indicies for training/testing
convert_ids_to_indices <- function(ids, train_ids, test_ids){
  
  train_indices <- list() 
  test_indices <- list()
  for (i in 1:length(train_ids)){
    train_indices[[i]] <- which(ids %in% train_ids[[i]])
    test_indices[[i]] <- which(ids %in% test_ids[[i]])
    #print(i, train_indices, test_indices, train_ids[[i]], test_indices[[i]])
  }
  
  list(train_indices = train_indices, test_indices = test_indices)
  
}

#Function to create folds for CV (does not work properly right now)
create_cv_indices <- function(pos_patients, neg_train, neg_test = NULL, K = 5, 
                              seed = 123, matched_test_col = NULL, 
                              matched_train_col = NULL, 
                              patient_id_col='PATIENT_ID'){
  set.seed(seed)
  # CREATE RESAMPLING INDICES -----------------------------------------------
  
  # create a set of K bins:
  # remainder <- nrow(pos_patients) - nrow(pos_patients) %% K
  bins <- rep(1:K, nrow(pos_patients)/K)
  pos_randomise <- sample(nrow(pos_patients), nrow(pos_patients), replace = FALSE)
  pos_groups <- split(pos_patients[pos_randomise,], bins)

  if (is.null(neg_test)){
    neg_test <- neg_train
  }
  
  #Seed is the same, so if neg_test is neg_train, will end up with same splits for CV
  if (is.null(matched_test_col)){
      neg_test_randomise <- sample(nrow(neg_test), nrow(neg_test), replace = FALSE)
      neg_test_groups <- split(neg_test[neg_test_randomise,], bins)
  }
  if (is.null(matched_train_col)){
      neg_train_randomise <- sample(nrow(neg_train), nrow(neg_train), replace = FALSE)
      neg_train_groups <- split(neg_train[neg_train_randomise,], bins)
  }
  
    
  # create a series of training and testing indices:
  train_combined <- list()
  test_combined <- list()
  
  for(i in 1:length(pos_groups)) {
    # extract positive patients NOT in set i for training:
    train_pos <- do.call("rbind", pos_groups[-i])
    
    # combine positives and negatives:
    if(is.null(matched_test_col)) {
      test_combined[[i]] <- c(pos_groups[[i]][patient_id_col], 
                              neg_test_groups[[i]][patient_id_col])
    } else {
      test_combined[[i]] <- c(pos_groups[[i]][patient_id_col], 
                              neg_test[patient_id_col][neg_test[matched_test_col] %in% pos_groups[[i]][patient_id_col]])
    }
    if (is.null(matched_train_col)){
      train_combined[[i]] <- c(train_pos[patient_id_col], 
                               do.call("rbind", neg_train_groups[-i])[patient_id_col])
    } else {
      train_combined[[i]] <- c(train_pos[patient_id_col], 
                               neg_train[patient_id_col][!(neg_train[matched_test_col] %in% pos_groups[[i]][patient_id_col])])
    }
  }
    

  
  list(train_ids = train_combined, test_ids = test_combined)
}

