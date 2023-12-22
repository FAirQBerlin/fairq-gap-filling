etl <- function(x, ...) {
  if (x$optimize) {
    logging("--- Optimize --- %s", x$name)
    x <- optimize(x, ...)
  }
  logging("--- Extract --- %s", x$name)
  x <- extract(x, ...)
  logging("--- Transform --- %s", x$name)
  x <- transform(x, ...)
  logging("--- Load --- %s", x$name)
  lload(x, ...)
  logging("--- Finished --- %s", x$name)
  return(x)
}

optimize <- function(x, ...)
  UseMethod("optimize")
extract <- function(x, ...)
  UseMethod("extract")
transform <- function(x, ...)
  UseMethod("transform")
# to avoid name clash with base::load -> l(ocal)load:
lload <- function(x, ...)
  UseMethod("load")

optimize.NULL <- function(x, ...) {
  log_debug("Skipping optimize because of NULL value")
  NULL
}

extract.NULL <- function(x, ...) {
  log_debug("Skipping extract because of NULL value")
  NULL
}

transform.NULL <- function(x, ...) {
  log_debug("Skipping transform because of NULL value")
  NULL
}

load.NULL <- function(x, ...) {
  log_debug("Skipping load because of NULL value")
  NULL
}

optimize.default <- function(x, ...) {
  log_debug("Entering 'default' optimize method for '%s'", x$name)
  
  x$dat <- do.call(x$get_data, list(
    query = "optimize_table", 
    mode = x$mode, 
    table = x$name)
    )
  
  return(x)
}


extract.default <- function(x, ...) {
  log_debug("Entering 'default' extract method for '%s'", x$name)

  queryParams <- x[unlist(lapply(x, Negate(is.function)))]
  x$dat <- do.call(x$get_data, list(x$name, queryParams=queryParams), ...)
  
  if (nrow(x$dat) == 0)
    x <- NULL
  
  
  return(x)
}

transform.default <- function(x, ...) {
  log_debug("Entering 'default' transform method for '%s'", x$name)
  
  if (nrow(x$dat) == 0)
    x <- NULL
  
  variables <- x$variables
  daily_only <- x$daily_only
  fill_until <- x$fill_until
  dat <- x$dat
  
  location_id_vars <- unlist(x$location_id_vars)
  dat <- x$dat
  
  location_id <- do.call(paste, c(dat[location_id_vars], sep = "."))
  dat_list <- split(dat, location_id)
  
  # For dev
  # dat_list <- dat_list[1:2]
  
  locations_filled <- gap_fill_locations(
    dat_list,
    variables = variables,
    daily_only = daily_only,
    fill_until = fill_until
    )

  locations_filled <- restrict_value_ranges(locations_filled)
  locations_filled <-
    undelivered_values_back_to_na(locations_filled, variables)
  
  types <- lapply(locations_filled, typeof)
  prev_integers <- unlist(types[unlist(variables)]) == "integer"
  
  if(any(prev_integers)) {
    integer_vars <- unlist(variables)[prev_integers]
    
    locations_filled <- locations_filled %>%
      mutate_at(paste0(integer_vars, "_filled"), function(x)
        as.integer(round(x))) 
  }
  
  x$dat <- locations_filled
  
  return(x)
}

load.default <- function(x, ...) {
  log_debug("Entering 'default' lload method for '%s'", x$name)
  do.call(x$send_data,
          list(df = x$dat,
               table = x$load_table,
               mode = x$load_mode))
}
