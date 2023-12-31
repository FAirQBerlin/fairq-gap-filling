% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03b_gap_filll_trailing_NAs.R
\name{fill_arima}
\alias{fill_arima}
\title{fill the NAs at the end of trailing dat with msarma estimates,
expects hourly data with a date_time POSIXct column}
\usage{
fill_arima(trailing_dat, variable_name, daily_only = FALSE)
}
\arguments{
\item{trailing_dat}{data.frame to fill}

\item{variable_name}{column name to fill}

\item{daily_only}{consider only a daily cycle, no weekly}
}
\description{
fill the NAs at the end of trailing dat with msarma estimates,
expects hourly data with a date_time POSIXct column
}
