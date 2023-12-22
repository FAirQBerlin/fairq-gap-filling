#' gap fill a list of locations
#'
#' @param dat_list list of data.frames,
#' each with one location to gap fill
#' @param variables list of variables to fill
#' @param fill_until time point to stop filling, if NULL the max of ts_dat is used.
#' @param daily_only consider only a daily cycle for gap filling, no weekly cycle
gap_fill_locations <-
  function(dat_list,
           variables,
           fill_until,
           daily_only = FALSE) {
    out <- mclapply(dat_list,
                    function(ts_dat, variables) {
                      log_location(ts_dat, variables)
                      gap_fill_variables(
                        ts_dat,
                        variables = variables,
                        fill_until = fill_until,
                        daily_only = daily_only
                      )
                    },
                    variables = variables,
                    mc.cores = Sys.getenv("N_WORKERS", 1))
    gc() # Free working memory
    out %>% bind_rows()
  }


log_location <- function(ts_dat, variables) {
  logging_sel <-
    colnames(ts_dat)[!colnames(ts_dat) %in% c(variables, "date_time")]
  logging_sel <- logging_sel[!grepl("_was_null", logging_sel)]
  logging(sprintf(
    "-- Location gap filling for %s",
    paste(
      logging_sel,
      unique(ts_dat[logging_sel]),
      sep = "=",
      collapse = ", "
    )
  ))
}


#' gap fill the data of one variable
#'
#' @param variables list of variables to fill
#' @param ts_dat data.frame of one multiple time series to fill
#' @param fill_until time point to stop filling, if NULL the max of ts_dat is used.
#' @param daily_only consider only a daily cycle for gap filling, no weekly cycle
gap_fill_variables <-
  function(ts_dat,
           variables,
           fill_until,
           daily_only = FALSE) {
    stopifnot(variables %in% colnames(ts_dat))
    
    ts_dat <- complement_times(ts_dat, fill_until)
    
    ts_dat <- fill_constant_variables(ts_dat, c(variables,
                                                paste0(variables, "_was_null")))
    
    filled_vars <- lapply(
      variables,
      gap_fill_variable,
      ts_dat = ts_dat,
      fill_until = fill_until,
      daily_only = daily_only
    )
    
    names(filled_vars) <- variables
    
    filled_df <- join_all(filled_vars, by = "date_time", type = "full")
    filled_df[unlist(variables)] <- NULL
    
    filled_df <- ts_dat %>% left_join(filled_df, by = "date_time")
    
    rm(ts_dat)
    gc()
    
    filled_df
  }


#' gap fill the data of one variable
#'
#' @param variable_name the variable to fill
#' @param ts_dat data.frame of one time series to fill
#' @param fill_until time point to stop filling, if NULL the max of ts_dat is used.
#' @param daily_only consider only a daily cycle for gap filling, no weekly cycle
gap_fill_variable <- function(variable_name,
                              ts_dat,
                              fill_until,
                              daily_only = FALSE) {
  logging(sprintf("Starting gap filling for variable %s", variable_name))
  if (length(unique(ts_dat$date_time)) != length(ts_dat$date_time)) {
    stop("duplicates in date_time of time series")
  }
  
  variable_name_filled <- sprintf("%s_filled", variable_name)
  
  all_na <- all(is.na(ts_dat[[variable_name]]))
  if (all_na) {
    ts_dat[[variable_name_filled]] <- NA
    return(ts_dat %>% select("date_time", variable_name, variable_name_filled))
  }
  
  ts_dat <- complement_times(ts_dat, fill_until)
  
  stopifnot(variable_name %in% colnames(ts_dat))
  
  ts_dat <- fill_small_gaps(ts_dat, variable_name = variable_name)
  ts_dat <- last_three_gaps_to_NA(ts_dat, variable_name = variable_name)

  ts_dat <-
    ts_dat %>% select("date_time", variable_name, variable_name_filled)
  
  ts_dat_list <-
    split_trailing_na(ts_dat, variable_name = variable_name_filled)
  rm(ts_dat)
  gc()
  
  out <- gap_fill_trailing_na_list(ts_dat_list,
                                   variable_name = variable_name_filled,
                                   daily_only = daily_only)
  
  out$split_group <- NULL
  
  out
  
}


#' complements a data frame with all hourly values in the respective
#' time frame of the date_time column
#'
#' @param ts_dat time series data.frame, needs a date_time column in POSIX.ct format
#' @param fill_until time point to stop filling, if NULL the max of ts_dat is used.
complement_times <- function(ts_dat, fill_until = NULL) {
  stopifnot("date_time" %in% colnames(ts_dat))
  
  if (is.null(fill_until)) {
    fill_until <- max(ts_dat$date_time)
  }
  
  if (is.character(fill_until)) {
    fill_until <- as.POSIXct(fill_until)
  }
  
  orig_times <- data.frame(date_time = ts_dat$date_time)
  
  all_times <- data.frame(date_time = seq(min(ts_dat$date_time),
                                          fill_until,
                                          by = 3600))
  
  ts_dat <- all_times %>%
    left_join(ts_dat, by = "date_time") %>%
    arrange(.data$date_time)
}


#' remove leading NAs from a data.frame
#'
#' @param ts_dat the data.frame
#' @param variable_name variable that should be used for removing leading NAs
remove_leading_na <- function(ts_dat, variable_name) {
  stopifnot(variable_name %in% colnames(ts_dat))
  first_not_na <- min(which(!is.na(ts_dat[[variable_name]])))
  ts_dat %>% filter(seq(1, nrow(ts_dat)) >= first_not_na)
}


#' splits the data frame into groups with trailing consecutive NAs.
#' The data is arranged according to the date_time column, leading NAs are removed.
#' i.e. each group has first some values, then some trailing NA values
#'
#' @param ts_dat the data.frame for splitting
#' @param variable_name variable to split
#'
#' @export
split_trailing_na <- function(ts_dat, variable_name) {
  stopifnot(variable_name %in% colnames(ts_dat))
  stopifnot("date_time" %in% colnames(ts_dat))
  
  ts_dat <- ts_dat %>%
    arrange(.data$date_time) %>%
    remove_leading_na(variable_name)
  
  dat <- ts_dat %>%
    mutate(grp = cumsum(c(0, abs(diff(
      !is.na(.data[[variable_name]])
    )) == 1))) %>%
    group_by(.data$grp) %>%
    mutate(split_group = floor(.data$grp / 2)) %>%
    ungroup() %>%
    select(-.data$grp)
  
  split(dat, dat$split_group)
}


#' Fill small (length up to length 2) gaps in the data by linear interpolation
#'
#' @param ts_dat the data.frame fo fill
#' @param variable_name variable to fill
#'
#' @export
fill_small_gaps <- function(ts_dat, variable_name) {
  stopifnot(variable_name %in% colnames(ts_dat))
  stopifnot("date_time" %in% colnames(ts_dat))

  ts_dat <- ts_dat %>% arrange(.data$date_time)
  
  # Fill up to 2 gaps with the previous value
  filled <- na.locf0(ts_dat[[variable_name]], maxgap = 2)
  ts_dat[[paste0(variable_name, "_filled")]] <- filled
  ts_dat
}
