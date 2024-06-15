library(tidyverse)
library(gt)

# Defining the function to find and remove outliers

find_outliers <- function(data) {
  # Check if the input is a dataframe
  if (!is.data.frame(data)) {
    stop("The input must be a dataframe")
  }
  
  # Function to flag outliers in a column
  flag_outliers <- function(x) {
    outliers <- boxplot.stats(x)$out
    x %in% outliers
  }
  
  # Identify rows with outliers in any numeric column
  data_flagged <- data %>%
    mutate(across(where(is.numeric), 
                  ~ ifelse(flag_outliers(.), TRUE, FALSE), 
                  .names = "outlier_{col}")) %>%
    rowwise() %>%
    mutate(outlier = any(c_across(starts_with("outlier_")))) %>%
    ungroup()
  
  # Filter out the rows with outliers
  data_without_outliers <- data_flagged %>%
    filter(!outlier) %>%
    select(-starts_with("outlier_"))
  
  return(data_without_outliers)
}



# Define the function to calculate ECMEN statistics
  





