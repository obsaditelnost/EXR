test_that("error messages work for get_exchange_rate_history (abort)", {
  #
})

test_that("warning messages for get_exchange_rate_history work and apply the expected changes to the function call if needed", {
  #
})

test_that("get_exchange_rate_history works for only scalar currency cases", {
  #
})

test_that("get_exchange_rate_history works for mixed scalar/vector currency cases", {
  #
})

test_that("get_exchange_rate_history works for mixed vector currency cases", {
  #
})


test_that("multiple instances of a currency should have no effect on the result in get_exchange_rate_history", {
  # multiple instances of a currency should have no effect on the result
  expect_equal(
    EXR::get_exchange_rate_history(
      base_currency = c("USD", "USD"),
      price_currency = c("EUR", "EUR"),
      fill_missing_dates = TRUE,
      filter = list(endPeriod = Sys.Date(), startPeriod = Sys.Date())
    ),
    EXR::get_exchange_rate_history(
      base_currency = c("USD"),
      price_currency = c("EUR"),
      fill_missing_dates = TRUE,
      filter = list(endPeriod = Sys.Date(), startPeriod = Sys.Date())
    )
  )
})


test_that("fill_gaps works (internal function)", {
  # if series are complete, there is need to change anything
  data <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-02", 2L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-03", 3L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-04", 4L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-05", 5L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-07", 7L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-08", 8L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-01", 9L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-02", 10L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-03", 11L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-04", 12L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-05", 13L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-07", 15L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-08", 16L, TRUE
  )

  expect_equal(
    EXR:::fill_gaps(data, max_lookback_days = 0) %>%
      apply(2, sort),
    data %>%
      apply(2, sort)
  )

  expect_equal(
    EXR:::fill_gaps(data, max_lookback_days = 7) %>%
      apply(2, sort),
    data %>%
      apply(2, sort)
  )

  # if series are incomplete, max_lookback_days needs to be applied (day 3,4,5 are gone!)
  data <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-02", 2L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-07", 7L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-08", 8L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-01", 9L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-02", 10L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-07", 15L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-08", 16L, TRUE
  )

  # max_lookback_days for value 0
  data_expected <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-02", 2L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-03", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-04", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-05", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-07", 7L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-08", 8L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-01", 9L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-02", 10L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-03", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-04", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-05", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-07", 15L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-08", 16L, TRUE
  )

  expect_equal(
    EXR:::fill_gaps(data, max_lookback_days = 0) %>%
      apply(2, sort),
    data_expected %>%
      apply(2, sort)
  )

  # max_lookback_days for value 1
  data_expected <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-02", 2L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-03", 2L, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-04", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-05", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-07", 7L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-08", 8L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-01", 9L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-02", 10L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-03", 10L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-04", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-05", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-07", 15L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-08", 16L, TRUE
  )

  expect_equal(
    EXR:::fill_gaps(data, max_lookback_days = 1) %>%
      apply(2, sort),
    data_expected %>%
      apply(2, sort)
  )

  # max_lookback_days  for value 3
  data_expected <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-02", 2L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-03", 2L, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-04", 2L, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-05", 2L, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-07", 7L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-08", 8L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-01", 9L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-02", 10L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-03", 10L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-04", 10L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-05", 10L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-07", 15L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-08", 16L, TRUE
  )

  expect_equal(
    EXR:::fill_gaps(data, max_lookback_days = 3) %>%
      apply(2, sort),
    data_expected %>%
      apply(2, sort)
  )

  # if series are incomplete, max_lookback_days needs to be applied (day 3,4,5 are gone!)
  # especially if we use manual start and end dates
  data <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-02", 2L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-07", 7L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-08", 8L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-01", 9L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-02", 10L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-07", 15L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-08", 16L, TRUE
  )

  data_expected <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2023-12-31", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-02", 2L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-03", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-04", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-05", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-07", 7L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-08", 8L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-09", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-10", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2023-12-31", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-01", 9L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-02", 10L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-03", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-04", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-05", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-07", 15L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-08", 16L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-09", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-10", NA, FALSE
  )

  expect_equal(
    EXR:::fill_gaps(data, max_lookback_days = 0, start_date = "2023-12-31", end_date = "2024-01-10") %>%
      apply(2, sort),
    data_expected %>%
      apply(2, sort)
  )

  # max_lookback_days works in start_date / end_date scenario
  data_expected <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2023-12-31", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-02", 2L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-03", 2L, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-04", 2L, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-05", NA, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-07", 7L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-08", 8L, TRUE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-09", 8L, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-10", 8L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2023-12-31", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-01", 9L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-02", 10L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-03", 10L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-04", 10L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-05", NA, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-07", 15L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-08", 16L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-09", 16L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-10", 16L, FALSE
  )

  expect_equal(
    EXR:::fill_gaps(data, max_lookback_days = 2, start_date = "2023-12-31", end_date = "2024-01-10") %>%
      apply(2, sort),
    data_expected %>%
      apply(2, sort)
  )

  # start_date and end_date actually narrow down the result
  data_expected <- dplyr::tribble(
    ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-05", 2L, FALSE,
    "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-06", 6L, TRUE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-05", 10L, FALSE,
    "DGBPEURX", "D", "GBP", "EUR", "X", "2024-01-06", 14L, TRUE
  )

  expect_equal(
    fill_gaps(data, max_lookback_days = 3, start_date = "2024-01-05", end_date = "2024-01-06") %>%
      apply(2, sort),
    data_expected %>%
      apply(2, sort)
  )
})
