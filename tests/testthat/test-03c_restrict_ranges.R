test_that("restrict_value_ranges works for usual case", {
  input_df <- data.frame(a = c(-1, 0, 1),
                         b = c(-2, 0, 2),
                         c = c(-3, 0, 3))
  allowed <- list(a = c(0, 10),
                  b = c(-10, 0))
  expected <- data.frame(a = c(0, 0, 1),
                         b = c(-2, 0, 0),
                         c = c(-3, 0, 3))
  res <- restrict_value_ranges(input_df, allowed)
  expect_equal(res, expected)
})


test_that("restrict_value_ranges works with NA in target variable", {
  input_df <- data.frame(a = c(-1, NA, 1))
  allowed <- list(a = c(0, 10))
  expected <- data.frame(a = c(0, NA, 1))
  res <- restrict_value_ranges(input_df, allowed)
  expect_equal(res, expected)
})


test_that("restrict_value_ranges works with NA in allowed ranges", {
  input_df <- data.frame(
    a = c(-1, 0, 1000),
    b = c(-Inf, 0, 2),
    c = c(-3, 0, 3)
  )
  allowed <- list(a = c(0, NA),
                  b = c(NA, 0))
  expected <- data.frame(a = c(0, 0, 1000),
                         b = c(-Inf, 0, 0),
                         c = c(-3, 0, 3))
  res <- restrict_value_ranges(input_df, allowed)
  expect_equal(res, expected)
})


test_that("allowed_ranges_filled_vars returns a meaningful list", {
  res <- allowed_ranges_filled_vars()
  expect_type(res, "list")
  expect_true(all(grepl("_filled$", names(res))))
})
