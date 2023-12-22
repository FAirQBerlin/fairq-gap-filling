#' Restrict ranges of filled values to a meaningful range
#' 
#' e.g., >= 0 for all pollutants
#'
#' @param locations_filled data frame containing some columns whose values will be restricted
#' @param allowed_ranges list with allowed ranges. Per default, the internal function
#' allowed_ranges_filled_vars() creates the list. The argument is only present to hand over a
#' different list for testing. If a boundary value is NA, the values remain unchanged.
restrict_value_ranges <-
  function(locations_filled, allowed_ranges = allowed_ranges_filled_vars()) {
    for (col_name in colnames(locations_filled)) {
      if (col_name %in% names(allowed_ranges)) {
        allowed_range <- allowed_ranges[[col_name]]
        
        if (!is.na(allowed_range[1])) {
          # Set very small values to the lowest allowed boundary value
          locations_filled[[col_name]] <-
            pmax(locations_filled[[col_name]], allowed_range[1])
        }
        
        if (!is.na(allowed_range[2])) {
          # Set very high values to the highest allowed boundary value
          locations_filled[[col_name]] <-
            pmin(locations_filled[[col_name]], allowed_range[2])
        }
        
      }
    }
    return(locations_filled)
  }


allowed_ranges_filled_vars <- function() {
  list(
    # Measuring stations
    # Upper limits are 1.2 * the most extreme observed value until August 2022
    pm10_filled = c(0, 1501 * 1.2),
    pm25_filled = c(0, 587 * 1.2),
    no2_filled = c(0, 508 * 1.2),
    # DWD
    # Limits are mostly relative to the most extreme observed value in the data until August 2022
    # Higher factors for temperature and precipitation max because there will be more extreme events
    wind_direction_filled = c(0, 360), # degrees
    wind_speed_filled = c(0, 1.2 * 74.9),
    precipitation_filled = c(0, 1.5 * 67.7),
    temperature_filled = c(-30, 1.5 * 38.4),
    cloud_cover_filled = c(0, 100), # percent
    pressure_msl_filled = c(870, 1085), # most extreme values from Wikipedia
    sunshine_filled = c(0, 60) # minutes per hour
  )
}
