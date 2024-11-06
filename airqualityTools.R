######################################
# Function that detect missing values
######################################

#' Detect Missing Values
#'
#' This function detects missing values in each column of a dataset.
#' It returns a named vector with counts of missing values for each column that has missing data.
#'
#' @param data A data frame or tibble.
#' @return A named vector with counts of missing values for each column.
#' @export
#' @examples
#' detect_missing_values(airquality)
detect_missing_values <- function(data){
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <- colnames(data)

  for(i in 1:ncol(data)){
    missing_counts[i] <- sum(is.na(data[[i]]))
  }

  # Filter out columns with no missing values
  missing_counts <- missing_counts[missing_counts > 0]
  return(missing_counts)
}



########################################
# Calculate the median for each variable
########################################

#' Calculate Medians for Numeric Variables
#'
#' This function calculates the median of each numeric variable in a dataset.
#'
#' @param dataset A data frame or tibble.
#' @return A data frame with medians of each numeric variable.
#' @export
#' @examples
#' median_airquality(airquality)
median_airquality <- function(data){
  # Filter out non-numeric columns
  numeric_data <- data[sapply(data, is.numeric)]

  # Aplly function to columns
  medians <- sapply(numeric_data,median, na.rm = TRUE)

  # Create dataframe from results
  results_df <- data.frame(
    Medians = medians
  )
  return(results_df)
}



########################################
# Replace missing values with the median
########################################

#' Replace Missing Values with Medians
#'
#' This function replaces missing values in numeric columns with the median of each column.
#'
#' @param dataset A data frame or tibble.
#' @return A data frame with missing values replaced by column medians.
#' @export
#' @examples
#' replace_missing_with_median(airquality)
replace_missing_with_median <- function(data) {
  # Loop through each column
  for (i in 1:ncol(data)) {
    # Check if the column is numeric and contains missing values
    if (is.numeric(data[[i]]) && any(is.na(data[[i]]))) {
      # Calculate the median for the column, excluding NA values
      median_value <- median(data[[i]], na.rm = TRUE)
      # Replace missing values with the median
      data[[i]][is.na(data[[i]])] <- median_value
    }
  }
  return(data)
}

