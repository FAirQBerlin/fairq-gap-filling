#' Set values that were originally NULL back to NA
#'
#' @description Some values are not delivered in the original data.
#' We want them to be NULL again in the columns without "_filled" suffix in the target table.
#' However, in the query we already replace missing input values with the filled value
#' if it exists, in order to avoid filling the same gaps again and again in every gap filling.
#' This function uses the information from the "..._was_null" columns to set the
#' original columns back to NA (which will be NULL in the database).
#' We still need this information for model evaluation: Here we want to have the choice
#' whether we compare the predicted values with all values (including those we filled ourselves)
#' or only with the values that were originally delivered by the data source.
#'
#' @param locations_filled data frame with the original columns, the _filled columns, and
#' the _was_null columns containing the information whether the original value
#' delivered by the data source was NULL (0/1)
#' @param variables variables that were filled
#' @return data frame where the original columns are corrected such that values are NULL
#' again if they were not delivered; the "..._was_null" columns are dropped.
undelivered_values_back_to_na <-
  function(locations_filled, variables) {

    for (variable in variables) {
      var_was_null <- paste0(variable, "_was_null")
      # If the indicator is NA itself, it means that it's a row that was added
      # by complement_times, so there was no original value either
      missing_in_raw_data <-
        is.na(locations_filled[[var_was_null]]) |
        locations_filled[[var_was_null]] == 1
      locations_filled[[variable]][missing_in_raw_data] <- NA
    }
    
    locations_filled[, paste0(variables, "_was_null")] <- NULL
    
    return(locations_filled)
  }


#' Set last three gaps back to NA
#'
#' @description When a new gap starts, it's first a small gap (up to two missing values).
#' They are filled with the previous value.
#' But in may become a larger gap. Then we want it to be filled using ARIMA.
#' The filling values we computed before are already used and not recomputed per default.
#' Without this function here, larger gaps would be treated as small gaps bit by bit.
#' Therefore we need to set the last gap to missing again after filling small gaps.
#' The consequence: If they become larger than 2, they will be filled with ARIMA as they should.
#' To make sure this is still working if the gap filling was down for some hours,
#' we treat the last three gaps.
#'
#' @param ts_dat data.frame with the variable, the _filled column of the variable
#' and the _was_null column of the variable (information if the variable was NULL
#' in the raw data)
#' @param variable_name str name of the variable to be corrected
#' @return data.frame with same number of rows and columns, but the last three
#' gaps of the variable will be NA again (so they are ready to be filled with
#' the ARIMA method)
last_three_gaps_to_NA <- function(ts_dat, variable_name) {
  was_null_col <- paste0(variable_name, "_was_null")
  
  # Vector that indicates where the gaps start
  new_gap_starting <-
    (ts_dat[[was_null_col]] - dplyr::lag(ts_dat[[was_null_col]])) == 1
  
  # Where does the third last gap start?
  pos_new_gap_starting <- which(new_gap_starting)
  if (length(pos_new_gap_starting) >= 3) {
    pos_third_last_gap_start <-
      pos_new_gap_starting[length(pos_new_gap_starting) - 2]
  } else if (length(pos_new_gap_starting) > 0) {
    pos_third_last_gap_start <- pos_new_gap_starting[1]
  } else {
    # There are no gaps
    return(ts_dat)
  }
  # Logical vector indicating which values are set to NA:
  # They must be NA in the raw data AND they must not be before the third last gap!
  set_to_NA <- ts_dat[[was_null_col]] == 1
  set_to_NA[1:(pos_third_last_gap_start - 1)] <- FALSE
  
  ts_dat[[paste0(variable_name, "_filled")]][set_to_NA] <- NA
  
  return(ts_dat)
}
