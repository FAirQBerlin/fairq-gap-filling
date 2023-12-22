#' fill the constant variables (i.e. identifiers etc.)
#' by default all except variables defined in exclude_variables and date_time
#' 
#' @param ts_dat time series data frame of one location
#' @param exclude_variables which are excluded from filling (i. e. not constant)
fill_constant_variables <- function(ts_dat, exclude_variables) {
  
  constant_variables <- colnames(ts_dat)[!(colnames(ts_dat) %in% 
                                             c(exclude_variables, "date_time"))]
  
  cols_not_unique <- more_than_one_unique_value(
    ts_dat %>% select(constant_variables)
  )
  
  if (any(cols_not_unique)) {
    stop(sprintf("The following columns have more than one unique value,
                 and cannot be simply filled: %s", names(cols_not_unique)[cols_not_unique]))
  }
  
  for (colname in constant_variables) {
    ts_dat[, colname] <- unique_not_na(ts_dat[, colname])
  }
  
  ts_dat
  
}


#' Inspect a data.frame for unique values
#' @return vector with one element per data.frame column;
#' TRUE if all values in the column are identical (excluding NAs),
#' FALSE if there are at least two different values
#' 
#' @param df the data.frame to test
more_than_one_unique_value <- function(df) {
  unlist(lapply(df, function(x) length(unique_not_na(x)) > 1))
}


#' Unique values that are not NA
#'
#' @param x vector
unique_not_na <- function(x) {
  unique(x[!is.na(x)])
}
