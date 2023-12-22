test_that("undelivered_values_back_to_na sets correct values back to NULL",
          {
            input_df <- data.frame(
              date_time = c(7, 8, 9),
              a = c(5, 77, 5),
              # assume the 77 was NULL in original data
              a_filled = c(5, 5, 5),
              a_was_null = c(0, 1, 0),
              b = c(77, 2, 99),
              # assume 77 and 99 were NULL in original data
              b_filled = c(1, 2, 3),
              b_was_null = c(NA, 0, 1) # NA must not be a problem
            )
            
            expected <- data.frame(
              date_time = c(7, 8, 9),
              a = c(5, NA, 5),
              a_filled = c(5, 5, 5),
              b = c(NA, 2, NA),
              b_filled = c(1, 2, 3)
            )
            
            res <-
              undelivered_values_back_to_na(input_df, c("a", "b"))
            
            expect_equal(res, expected)
          })

test_that("last_three_gaps_to_NA treats the last three gaps but not more",
          {
            # First gap at beginning of data, doesn't matter
            # Second gap is filled by "fill_small_gaps" and must stay filled bc
            # it's not among the last 3 gaps
            # Third gap is the third last gap and must be set back to NA
            # Fourth gap has length 3 and is ignored by "fill_small_gaps" anyway
            # Fifth gap must be set back to NA
            input_df <-
              data.frame(
                a = c(NA, 1, 2, 3, NA, NA, 4, NA, 5, 6, NA, NA, NA, 7, NA, NA),
                a_filled = c(NA, 1, 2, 3, 3, 3, 4, 4, 5, 6, 6, 6, 6, 7, 7, 7),
                a_was_null = c(1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1)
              )
            expected <-
              data.frame(
                a = c(NA, 1, 2, 3, NA, NA, 4, NA, 5, 6, NA, NA, NA, 7, NA, NA),
                a_filled = c(NA, 1, 2, 3, 3, 3, 4, NA, 5, 6, NA, NA, NA, 7, NA, NA),
                a_was_null = c(1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1)
              )
            res <- last_three_gaps_to_NA(input_df, "a")
            expect_equal(res, expected)
          })

test_that(
  "undelivered_values_back_to_na also works if a already filled value from before was used",
  {
    input_df <-
      data.frame(
        a = c(6, 6, 6, 6, 7, NA, NA),
        a_filled = c(6, 6, 6, 6, 7, 7, 7),
        a_was_null = c(0, 1, 1, 1, 0, 1, 1)
      )
    expected <- data.frame(
      a = c(6, 6, 6, 6, 7, NA, NA),
      a_filled = c(6, NA, NA, NA, 7, NA, NA),
      a_was_null = c(0, 1, 1, 1, 0, 1, 1)
    )
    res <- last_three_gaps_to_NA(input_df, "a")
    expect_equal(res, expected)
  }
)


test_that("undelivered_values_back_to_na also works if there are no gaps",
          {
            input_df <-
              data.frame(
                a = c(6, 6, 6, 6, 7, 7, 7),
                a_filled = c(6, 6, 6, 6, 7, 7, 7),
                a_was_null = c(0, 0, 0, 0, 0, 0, 0)
              )
            res <- last_three_gaps_to_NA(input_df, "a")
            expect_equal(res, input_df)
          })
