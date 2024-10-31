# Create function that introduces errors into dataframe with numeric columns
intro_errors <- function(df, 
                         neg_count = 5,
                         neg_min = -100,
                         non_numeric_count = 5,
                         non_numeric_value = "none",
                         large_count = 2,
                         large_mutliplier = 10) {
  
  
  
  # introduce negative values
  for (i in 1:neg_count) {
    row <- sample(1:nrow(df), 1)
    col <- sample(1:ncol(df), 1)
    if (is.numeric(df[[col]])) {
      df[row, col] <- runif(1, min = neg_min, max = 0)  # Random negative value
    }
  }
  
  
  
  # introduce non-numeric values
  for (j in 1:non_numeric_count) {
    row <- sample(1:nrow(df), 1)
    col <- sample(1:ncol(df), 1)
    if (is.integer(df[[col]])) {
      df[row, col] <- non_numeric_value  # Setting a string value
      }
    
    return(df)
  }
  
  
  # introduce large numbers
  for (k in 1:large_count) {
    row <- sample(1:nrow(df), 1)
    col <- sample(1:ncol(df), 1)
    if (is.numeric(df[[col]])) {
      df[row, col] <- max(col) * large_mutliplier  # Setting value to maximum value of column times multiplier
      }
    
    return(df)
    }
}