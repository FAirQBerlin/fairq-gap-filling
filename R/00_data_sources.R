#' Data sources to query from clickhouse
#'
#' List data sources. This list defines which data source to extract from clickhouse.
#'
#' @export
#' @rdname single_source
data_sources <- function() {
  # Create an object for all data sources
  list(
    single_source(
      name = "messstationen_filled",
      variables = list("pm10", "pm25", "no2"),
      fill_until = round(Sys.time(), "hour"),
      location_id_vars = "station_id",
      is_active_only = TRUE,
      optimize = TRUE
    ),
    single_source(
      name = "cams_all_latest_filled",
      variables = list(
        "no2",
        "pm25",
        "pm10"
      ),
      location_id_vars = list("lat_int", "lon_int"),
      fill_until = as.POSIXct(format(Sys.time(), '%Y-%m-%d %H:00:00')) + 3600 * 24 * 5
    )
  ) %>%
    name_data_sources()
}


#' Name elements of a list of data sources
#'
#' Use the $name of a single source to name its element in a
#' a list of data sources.
#'
#'@param data_sources_list (list) an unnamed list of single sources
#'@return list of named single sources, names correspond to elements'
#'        $name slot.
#'
name_data_sources <- function(data_sources_list) {
  names <- lapply(data_sources_list, `[[`, "name") %>% unlist
  names(data_sources_list) <- names
  return(data_sources_list)
}
