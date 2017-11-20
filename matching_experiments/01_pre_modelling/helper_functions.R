
combine_dataframes <- function(df_1, df_2){
  diffs <- two_way_setdiff(colnames(df_1), colnames(df_2))
  df_2 <- df_2 %>%
    select(-one_of(diffs))
  df_1 <- df_1 %>%
    select(-one_of(diffs))
  # Order variables so the datasets can be bound
  df_2_ordered <- df_2[order(match(colnames(df_2), colnames(df_1)))]
  all.equal(colnames(df_2_ordered), colnames(df_1))
  return(rbind(df_2_ordered, df_1))
}

topcode <- function(input, cap, label) {
  # extract index of positives and negatives:
  pos_index <- which(input[[label]] == 1)
  neg_index <- which(input[[label]] == 0)
  
  capped <- sapply(input, function(x) { 
    # segregate the vector into positives and negatives:
    pos <- x[pos_index]
    neg <- x[neg_index]
    # work out which class has the higher cap:
    quant_pos <- stats::quantile(pos, cap, na.rm = TRUE)
    quant_neg <- stats::quantile(neg, cap, na.rm = TRUE)
    quant <- max(quant_pos, quant_neg)
    
    # set anything in the dataset above this value to this value
    x[x > quant] <- quant
    
    return(x)
  })
  
  return(as.data.frame(capped))
  
}

create_date_diffs <- function(input, index_col = "index_date") {
  
  date_cols <- input[, -which(colnames(input) == index_col)]
  
  date_diffs <- as.data.frame(sapply(date_cols, function(x) { 
    input[[index_col]] - x
    
  }))
  
  return(date_diffs)
}

date_format <- function(input_data, date_pattern, PATIENT_ID_col) {
  date_data <- select(input_data, contains(date_pattern))
  formatted <- lapply(date_data, mdy)
  df_date <- as.data.frame(formatted)
  df <- data.frame(PATIENT_ID = input_data[PATIENT_ID_col],
                   df_date
  )
  return(df)
}


two_way_setdiff <- function(x, y, lengths_only = FALSE) {
  
  x_no_y <- setdiff(x, y)
  print(paste("In x but not y:"))
  if(lengths_only) {
    print(length(x_no_y)) } else {
      print(x_no_y)
    }
  
  y_no_x <- setdiff(y, x)
  print(paste("In y but not x:"))
  if(lengths_only) {
    print(length(y_no_x)) } else {
      print(y_no_x)
    }
  
  return(c(x_no_y, y_no_x))
}

calculate_s_date_diffs <- function(S_vars, index_date){
  #S variables - these are in months, need to be recalculated....
  S_vars_format <- as.data.frame(sapply(S_vars, function(x) { ifelse(is.na(x), NA, paste0(x, "01")) }))
  S_vars_dates <- as.data.frame(lapply(S_vars_format, ymd))
  # convert to yearmonths:
  S_vars_yearmon <- as.data.frame(sapply(S_vars_dates, as.yearmon))
  # create date differences for these variables:
  S_date_diffs <- as.data.frame(sapply(S_vars_yearmon,
                                       function(x) {((index_date - x)*12)})) 
  return(S_date_diffs)
}

topcode_date_diffs <- function(input, cap, label, max_val=NULL) {
  # extract index of positives and negatives:
  pos_index <- which(input[[label]] == 1)
  neg_index <- which(input[[label]] == 0)
  # set label to NULL
  input[[label]] <- NULL
  # cap variables:
  capped <- sapply(input, function(x) { 
    if (all(is.na(x))) {
      return(x)
    }
    # segregate the vector into positives and negatives:
    pos <- x[pos_index]
    neg <- x[neg_index]
    # work out which class has the higher cap:
    
    quant_pos <- stats::quantile(pos, cap, na.rm = TRUE)[[1]]
    quant_neg <- stats::quantile(neg, cap, na.rm = TRUE)[[1]]
    quant <- max(quant_pos, quant_neg, na.rm = TRUE)
    
    
    if ((!is.null(max_val)) && quant > max_val) {
      quant <- max_val
    }
    # set anything in the dataset above this value to this value
    if (quant != -Inf) {
      x[x > quant] <- quant
    }
    
    return(x)
  })
  
  return(as.data.frame(capped))
  
}

# slight alteration for frequencies: only use values above 0 for percentiles:

topcode_frequencies <- function(input, cap, label) {
  # extract index of positives and negatives:
  pos_index <- which(input[[label]] == 1)
  neg_index <- which(input[[label]] == 0)
  # set label to NULL
  input[[label]] <- NULL
  # cap variables:
  capped <- sapply(input, function(x) { 
    # segregate the vector into positives and negatives:
    pos <- x[pos_index]
    neg <- x[neg_index]
    # work out which class has the higher cap:
    quant_pos <- stats::quantile(pos[pos > 0], cap, na.rm = TRUE)
    quant_neg <- stats::quantile(neg[neg > 0], cap, na.rm = TRUE)
    quant <- max(quant_pos, quant_neg, na.rm = TRUE)
    
    # set anything in the dataset above this value to this value
    x[x > quant] <- quant
    
    return(x)
  })
  
  return(as.data.frame(capped))
  
}
