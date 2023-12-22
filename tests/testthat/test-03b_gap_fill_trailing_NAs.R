test_that("gap_fill_trailing_na_list works as expected", {
  
  df <- data.frame(
    date_time = as.POSIXct(1:51 * 3600, origin = "1970-01-01"),
    x = c(1:24, 1, NA, 3:24, NA, NA, NA)
  )
  
  df_list <- split_trailing_na(df, variable_name = "x")
  
  res <- gap_fill_trailing_na_list(df_list, variable_name = "x")
  expect_named(
    res,
    c("date_time", "x", "split_group")
  )
  
  expect_equal(
    round(res$x, 0),
    c(1:24, 1, 2, 3:24, 1, 2, 3)
  )
  
  expect_equal(
    res$date_time,
    df$date_time
  )
  
  
})

test_that("fill_arima", {
  input <- data.frame(
    date_time = as.POSIXct(1:336 * 3600, origin = "1970-01-01"),
    x = c(rep(1:24, 13), rep(NA, 24))
  )
  res <- fill_arima(input, "x")
  
  expect_named(res, c("date_time", "arma_preds"))
  expect_true(all(as.integer(round(res$arma_preds)) == seq(1:24)))
  
  
  input <- data.frame(
    date_time = as.POSIXct(1:100 * 3600, origin = "1970-01-01"),
    x = c(rep(1:24, 4), rep(NA, 4))
  )
  res <- fill_arima(input, "x")
  
  expect_named(res, c("date_time", "arma_preds"))
  expect_true(all(as.integer(round(res$arma_preds)) == seq(1:4)))
  
  
  input <- data.frame(
    date_time = as.POSIXct(1:336 * 3600, origin = "1970-01-01"),
    x = c(rep(1:24, 14))
  )
  expect_error(fill_arima(input, "x"),
               "there are no trailing NAs to predict")
  
  input <- data.frame(
    date_time = as.POSIXct(1:25 * 3600, origin = "1970-01-01"),
    x = c(rep(1:24, 1), NA)
  )
  expect_error(fill_arima(input, "x"),
               "<= 24h of data - cannot fit model")
  
})


test_that("get daily cycle works correctly", {
  
  df <- data.frame(
    date_time = as.POSIXct(seq(0, 3600 * 2 * 24 - 1, 3600), origin = "1970-01-01"),
    x = c(c(1:24), c(1:24) + 1)
  )
  
  out <- add_daily_cycle(df, "x")
  
  expect_equal(
    unique(out$daily_cycle),
    c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5,
      13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5, 21.5, 22.5, 23.5, 24.5)
  )
  
  expect_equal(
    out$dev_from_cycle,
    c(rep(-0.5, 24), rep(0.5, 24))
  )
  
  expect_named(out, c("daily_cycle", "date_time", "x", "dev_from_cycle"))
  
})
