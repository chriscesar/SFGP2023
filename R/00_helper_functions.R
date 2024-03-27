# 00_helper_functions.R
# Function to check if sum is zero for numeric columns
sum_zero <- function(x) {
  if (is.numeric(x)) {
    sum_val <- sum(x)
    return(sum_val == 0)
  } else {
    return(FALSE)
  }
}
