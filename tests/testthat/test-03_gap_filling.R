ts_dat <-
  data.frame(
    date_time = c(1, 2, 3, 4, 5, 6, 7, 13, 12, 11, 10, 9, 8, 14, 15, 16),
    x = c(NA, 1, 4, NA, NA, NA, NA, 1, NA, 3, 4, NA, 7, NA, NA, 7)
  )

test_that("remove leading nas works correctly", {
  
  res <- remove_leading_na(ts_dat, "x")
  expect_true(!is.na(res$x[1]))
  expect_true(class(res) == "data.frame")
  expect_equal(
    structure(list(
      date_time = c(2, 3, 4, 5, 6, 7, 13, 12, 11, 10, 9, 8, 14, 15, 16),
      x = c(1, 4, NA, NA, NA, NA, 1, NA, 3, 4, NA, 7, NA, NA, 7)
      ), class = "data.frame", row.names = c(NA, -15L)),
    res)
  
})


test_that("split data into chunks with trailing na works correctly", {
  
  res <- split_trailing_na(ts_dat, "x")
  expect_true(class(res) == "list")
  expect_true(length(res) == 5)
  expect_true(all((res %>% bind_rows())$date_time == 2:16))
  expect_equal(
    (lapply(res, tail, n = 1) %>% bind_rows())$x,
    c(NA, NA, NA, NA, 7)
  )

})

test_that("fill small gaps works correctly", {
  
  res <- fill_small_gaps(ts_dat, "x")
  expect_true(sum(is.na(res$x_filled)) == 5) 
  expect_true(class(res) == "data.frame")
  expect_equal(
    res$x_filled,
    c(NA, 1, 4, NA, NA, NA, NA, 7, 7, 4, 3, 3, 1, 1, 1, 7)
  )
  expect_named(res,
               c("date_time", "x", "x_filled"))
  
})


test_that("complement times works correctly", {
  
  df <- data.frame(
    date_time = as.POSIXct(c(0, 7200), origin = "1970-01-01")
  )
  
  res <- complement_times(df)
  expect_equal(
    res,
    data.frame(
    date_time = as.POSIXct(c(0, 3600, 7200), origin = "1970-01-01")           
   )
  )
  
})

test_that("complement times uses fill_until correctly", {
  
  df <- data.frame(
    date_time = as.POSIXct(c(0, 7200), origin = "1970-01-01")
  )
  
  res <- complement_times(df, fill_until = as.POSIXct(10800, origin = "1970-01-01"))
  expect_equal(
    res,
    data.frame(
      date_time = as.POSIXct(c(0, 3600, 7200, 10800), origin = "1970-01-01")           
    )
  )
  
})




test_that("more_than_one_unique_value works correctly", {
  df <- data.frame(
    w = c(1, 1, 1),
    x = c(1, 1, NA),
    y = c(2, 2, 1),
    z = c(2, 1, NA)
  )
  
  expect_equal(
    more_than_one_unique_value(df),
    c(w = FALSE, x = FALSE, y = TRUE, z = TRUE)
    )
  
})

test_that("fill constant variables works as expected", {
  
  df <- data.frame(
    id = c(1, 1, NA),
    date_time = c(1, 2, 3),
    x = c(1, 2, NA)
  )
  
  expected <- data.frame(
    id = c(1, 1, 1),
    date_time = c(1, 2, 3),
    x = c(1, 2, NA)
  )
  
  res <- fill_constant_variables(df, "x")
  expect_equal(
    res,
    expected
  )
  
})

test_that("fill_constant_variables throws error when a non-unique variable would be filled", {
  
  df <- data.frame(
    id = c(1, 2, NA),
    date_time = c(1, 2, 3),
    x = c(1, 2, NA)
  )
  expect_error(
    fill_constant_variables(df, "x"),
    "The following columns have more than one unique value,
                 and cannot be simply filled: id"
  )
  
})
