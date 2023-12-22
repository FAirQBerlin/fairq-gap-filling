#' fill gaps in a loop over an ordered list, each element with trailing NAs at the end
#' The functions starts at the beginning of the list and fills the gaps forward in sequence
#' thus the list has to be ordered by time
#' 
#' @param ts_dat_list ordered list of trailing NA blocks
#' @param variable_name variable_name for gap filling
#' @param daily_only consider only a daily cycle for gap filling, no weekly cycle
gap_fill_trailing_na_list <- function(ts_dat_list, variable_name, daily_only = FALSE) {
  
  filled_list <- ts_dat_list

  for (i in seq_along(ts_dat_list)) {

    filled_list[[i]] <- do_gap_filling_element(
      i,
      filled_list,
      variable_name = variable_name,
      daily_only = daily_only
      )

  }
  
  filled_list <- filled_list %>% 
    bind_rows() %>% 
    arrange(.data$date_time)
  
  filled_list
}


#' do the gap filling to one element of an ordered list, each element with trailing NAs at the end
#' the function applies the gap filling to one element of the list
#' 
#' @param i element of the list
#' @param ts_dat_list ordered list of trailing NA blocks
#' @param variable_name variable_name for gap filling
#' @param daily_only consider only a daily cycle for gap filling, no weekly cycle
do_gap_filling_element <- function(i, ts_dat_list, variable_name, daily_only = FALSE) {
  log_debug(sprintf("do_gap_filling_element i = %s for variable %s", i, variable_name))
  
  trailing_dat <- ts_dat_list[1:i]
  
  trailing_dat <- trailing_dat %>% bind_rows()
  
  last_is_not_na <- !is.na(tail(trailing_dat[[variable_name]], 1))
  all_na <- all(is.na(trailing_dat[[variable_name]]))
  if (last_is_not_na || all_na) {
    log_debug(sprintf("cannot fill the gap as last_is_not_na = %s and all_na = %s",
                      last_is_not_na, all_na))
    return(ts_dat_list[[i]])
  }
  
  filled_dat <- fill_large_gaps(
    trailing_dat,
    variable_name = variable_name,
    daily_only = daily_only
    )
  
  needed_times <- ts_dat_list[[i]] %>% select(.data$date_time)
  
  ts_dat_element_out <- needed_times %>% 
    left_join(filled_dat, by = "date_time") %>% 
    arrange(.data$date_time)
  
}


#' fills gaps in the data with daily and weekly cycle
#' 
#' @param trailing_dat data.frame with trailing NAs (to fill)
#' @param variable_name name of the column to fill
#' @param daily_only consider only a daily cycle for gap filling, no weekly cycle
fill_large_gaps <- function(trailing_dat, variable_name, daily_only = FALSE) {
  
  # get only last 28 days of non-NA data
  last_datetime_with_data <- max(trailing_dat$date_time[!is.na(trailing_dat[variable_name])])
  model_dat <- trailing_dat %>% filter(.data$date_time > last_datetime_with_data - 3600 * 24 * 28)
  
  model_dat <- add_daily_cycle(model_dat, variable_name = variable_name)

  preds_df <- fill_arima(model_dat, "dev_from_cycle", daily_only = daily_only)
  
  preds_df <- model_dat %>% 
    left_join(preds_df, by = "date_time") %>%
    mutate(preds = .data$arma_preds + .data$daily_cycle)
  
  trailing_dat <- preds_df %>% 
    select("date_time", "preds") %>% 
    right_join(trailing_dat, by = "date_time")
  
  trailing_dat[[variable_name]] <- coalesce(trailing_dat[[variable_name]], trailing_dat$preds)
  
  trailing_dat$preds <- NULL
  
  trailing_dat %>% arrange(.data$date_time)
  
}


#' extracts the daily cycle of the specified variable and joins it on the data,
#' as well as the deviation
#' 
#' @param ts_dat time series data.frame with a date_time column POSIXct
#' @param variable_name column_name of the variable for which the cycle should by calculated
#' 
add_daily_cycle <- function(ts_dat, variable_name = "no2") {
  stopifnot("date_time" %in% colnames(ts_dat))
  stopifnot(variable_name %in% colnames(ts_dat))
  
  ts_dat <- ts_dat %>% 
    mutate(hour_of_day = format(ts_dat$date_time, "%H"))
  
  ts_dat <- ts_dat %>% 
    group_by(.data$hour_of_day) %>% 
    summarise(daily_cycle = mean(.data[[variable_name]], na.rm = T)) %>% 
    right_join(ts_dat, by = "hour_of_day") %>% 
    mutate(dev_from_cycle = .data[[variable_name]] - .data$daily_cycle) %>% 
    arrange(.data$date_time) %>% 
    select(-.data$hour_of_day)
  
  ts_dat
}


#' fill the NAs at the end of trailing dat with msarma estimates,
#' expects hourly data with a date_time POSIXct column
#' 
#' @param trailing_dat data.frame to fill
#' @param variable_name column name to fill
#' @param daily_only consider only a daily cycle, no weekly
#' 
fill_arima <- function(trailing_dat, variable_name, daily_only = FALSE) {
  stopifnot("date_time" %in% colnames(trailing_dat))
  stopifnot(variable_name %in% colnames(trailing_dat))
  
  # number of NAs at the end to fill
  n_ahead <- nrow(trailing_dat) - max(which(!is.na(trailing_dat[[variable_name]])))
  log_debug(sprintf("filling length %s", n_ahead))
  if (n_ahead < 1) stop("there are no trailing NAs to predict")
  if (nrow(trailing_dat) - n_ahead <= 24) stop("<= 24h of data - cannot fit model")
  
  # Create time series object
  time_series <- ts(
    trailing_dat[[variable_name]],
    start = min(trailing_dat$date_time),
    deltat = 3600,
    end = max(trailing_dat$date_time))
  
  if (nrow(trailing_dat) - n_ahead < 14 * 24 | daily_only) { # two weeks
    ar <- daily_arma(time_series, n_ahead) 
  } else {
    ar <- weekly_arma(time_series, n_ahead)
  }
  
  pred_ar <- forecast(object = ar, h = n_ahead)
  
  preds_df <- data.frame(
    date_time = as.POSIXct(as.integer(time(pred_ar$forecast)), origin = "1970-01-01"),
    arma_preds = pred_ar$forecast
  )
  
  preds_df
}


#' fits an ar(1)ma(1) to a time_series with a daily cycle component
#' 
#' @param time_series time_series object created with ts()
#' @param n_ahead number of obs to holdout and to make the prediction
daily_arma <- function(time_series, n_ahead) {
  
  msarima(
    time_series,
    orders = list(
      ar = c(1, 1),
      ma = c(1, 1)
    ),
    lags = c(1, 24),
    holdout = TRUE, #holds the last n_ahead values (which are NA) out from fitting
    h = n_ahead
  )
  
}

#' fits an ar(1)ma(1) to a time_series with a daily and a weekly cycle component
#' 
#' @param time_series time_series object created with ts()
#' @param n_ahead number of obs to holdout and to make the prediction
weekly_arma <- function(time_series, n_ahead) {
  
  msarima(
    time_series,
    orders = list(
      ar = c(1, 1, 1),
      ma = c(1, 1, 1)
    ),
    lags = c(1, 24, 24 * 7),
    holdout = TRUE, #holds the last n_ahead values (which are NA) out from fitting
    h = n_ahead
  )
  
}
