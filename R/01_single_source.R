#' Single source
#'
#' @param name (character) a unique name used as class and to identify target
#'   table on database.
#' @param variables list of variables to gap-fill
#' @param location_id_vars a single id or a list of variables that provide a unique spatial key
#' (i.e. within each date_time) separating different locations to split the data into time series
#' @param get_data (function)
#' @param load_mode (character) defaults to "replace"
#' @param load_table (character) the table in that we load data
#' @param send_data (function)
#' @param daily_only if TRUE, use only daily_arima for gap filling, default FALSE
#' @param fill_until time point to stop filling; if NULL (default), the max of ts_dat is used.
#' @param optimize Should the source table be optimized (i.e. duplicate dropping) beforehand? FALSE by default. 
#' @param ... arguments used within methods and available for queries
#'
#' @export
#' @rdname single_source
single_source <- function(name,
                          variables,
                          location_id_vars,
                          get_data = fairqDbtools::send_query,
                          load_mode = "replace",
                          load_table = name,
                          send_data = fairqDbtools::send_data,
                          daily_only = FALSE,
                          fill_until = NULL,
                          optimize = FALSE,
                          ...) {
  # name (character) defines the class and is used for logging
  # get_data (function) a function that given the name and ... can extract data
  # ... passed to send data or otherwise used by methods
  out <- list(
    name = name,
    variables = variables,
    location_id_vars = location_id_vars,
    get_data = get_data,
    load_mode = load_mode,
    load_table = load_table,
    send_data = send_data,
    daily_only = daily_only,
    fill_until = fill_until,
    optimize = optimize,
    mode = mode$env(),
    ...
  )
  class(out) <- c(name, "list")
  
  return(out)
}
