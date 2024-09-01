test_that("function returns currencies with standard params", {
  result <- EXR::get_available_currencies()

  # must be a table / data.frame
  expect_true(inherits(result, what = "data.frame"))

  # 2 columns
  expect_equal(dim(result)[2], 2)

  # some rows
  expect_gt(dim(result)[1], 10)

  # ISOCODE must be a correct 3-letter code
  expect_true(all(grepl("^[A-Z][A-Z][A-Z]$", result$ISOCODE)))

  # EUR must be in data, as well as USD
  expect_contains(result$ISOCODE, "EUR")
  expect_contains(result$TITLE, "Euro")
  expect_contains(result$ISOCODE, "USD")

})

test_that("'date' parameter works", {
  result1 <- EXR::get_available_currencies(date = as.Date("2024-07-31"))
  result2 <- EXR::get_available_currencies(date = as.Date("2024-08-01"))
  result3 <- EXR::get_available_currencies(date = as.Date("2002-08-01"))
  result4 <- EXR::get_available_currencies(date = as.Date("1965-08-01"), use_most_recent = TRUE)
  result5 <- EXR::get_available_currencies(date = as.Date("1965-08-01"), use_most_recent = FALSE)

  # 1 day difference, should be the same result
  expect_identical(result1, result2)

  # 20 years... some difference! Some countries got Euro after 2002
  expect_lt(nrow(result1), nrow(result3))

  # no data for old dates
  expect_equal(dim(result4)[1], 0)
  expect_equal(dim(result4)[2], 2)

  # param 'use_most_recent' has no effect for too old data
  expect_identical(result4, result5)

  # date must not be NULL or some weird data type
  expect_error(EXR::get_available_currencies(date = NULL))
  expect_error(EXR::get_available_currencies(date = 2))
  expect_error(EXR::get_available_currencies(date = "2022-02-02"))
  expect_error(EXR::get_available_currencies(date = c(as.Date("2024-07-31"), as.Date("2024-07-30"))))

  # date must not be in the future
  expect_error(EXR::get_available_currencies(date = as.Date("2099-07-31")))
  expect_error(EXR::get_available_currencies(date = Sys.Date() + 1))

  # date CAN be today though
  expect_no_error(EXR::get_available_currencies(date = Sys.Date()))
})

test_that("'use_most_recent' parameter works", {
  result_saturday_true <- EXR::get_available_currencies(date = as.Date("2024-08-03"), use_most_recent = TRUE)
  result_sunday_true <- EXR::get_available_currencies(date = as.Date("2024-08-03"), use_most_recent = TRUE)
  result_saturday_false <- EXR::get_available_currencies(date = as.Date("2024-08-03"), use_most_recent = FALSE)
  result_sunday_false <- EXR::get_available_currencies(date = as.Date("2024-08-03"), use_most_recent = FALSE)

  result_normal_day_true <- EXR::get_available_currencies(date = as.Date("2024-07-31"), use_most_recent = TRUE)
  result_normal_day_false <- EXR::get_available_currencies(date = as.Date("2024-07-31"), use_most_recent = FALSE)

  # use_most_recent must not have an effect if day has data
  expect_identical(result_normal_day_true, result_normal_day_false)

  # saturday and sunday data should get the same results
  expect_identical(result_saturday_true, result_sunday_true)
  expect_identical(result_saturday_false, result_sunday_false)

  # no results if weekend and use_most_recent = FALSE
  expect_equal(dim(result_saturday_false)[1], 0)
  expect_equal(dim(result_saturday_false)[2], 2)
  expect_equal(dim(result_sunday_false)[1], 0)
  expect_equal(dim(result_sunday_false)[2], 2)

  # results if weekend and use_most_recent = TRUE
  expect_gt(dim(result_saturday_true)[1], 10)
  expect_equal(dim(result_saturday_true)[2], 2)
  expect_gt(dim(result_sunday_true)[1], 10)
  expect_equal(dim(result_sunday_true)[2], 2)

  # use_most_recent must be logical
  expect_error(EXR::get_available_currencies(use_most_recent = 3))
  expect_error(EXR::get_available_currencies(use_most_recent = "jo"))
  expect_error(EXR::get_available_currencies(use_most_recent = NULL))
  expect_error(EXR::get_available_currencies(use_most_recent = c(TRUE, FALSE)))

  # correct formats -> no error
  expect_no_error(EXR::get_available_currencies(use_most_recent = 1))
  expect_no_error(EXR::get_available_currencies(use_most_recent = TRUE))
  expect_no_error(EXR::get_available_currencies(use_most_recent = FALSE))
})
