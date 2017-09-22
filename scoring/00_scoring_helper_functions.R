
#  ------------------------------------------------------------------------
# HELPER FUNCTIONS FOR STRONGBRIDGE SCORING
#  ------------------------------------------------------------------------


# two way set diff --------------------------------------------------------

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
}

# format_dates ------------------------------------------------------------
date_format <- function(input_data, date_pattern) {
  date_data <- dplyr::select(input_data, dplyr::contains(date_pattern))
  formatted <- lapply(date_data, mdy)
  df_date <- as.data.frame(formatted)
  df <- data.frame(df_date)
  return(df)
}


# create date differences -------------------------------------------------
create_date_diffs <- function(input, index_col = "index_date") {
  
  date_cols <- input[, -which(colnames(input) == index_col)]
  
  date_diffs <- as.data.frame(sapply(date_cols, function(x) { 
    input[[index_col]] - x
    
  }))
  
  return(date_diffs)
}


