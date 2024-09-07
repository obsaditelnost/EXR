test_that("get_exchange_rate_history aborts for missing function call arguments", {
  # necessary parameters
  expect_error(EXR::get_exchange_rate_history())
  expect_error(EXR::get_exchange_rate_history(base_currency = "EUR"))
  expect_error(EXR::get_exchange_rate_history(price_currency = "EUR"))
})

test_that("get_exchange_rate_history params base_currency and price_currency work as expected", {
  # ========== errors and warnings ===========#

  # unknown currency
  expect_warning(EXR::get_exchange_rate_history(base_currency = "EUR", price_currency = c("USD", "ZZZ"), periodicity = "M"),
    regexp = ".*Currency.*ZZZ.*is missing the the result.*"
  )

  test_params <- list(NA, "423", "P", NULL, c("A", "E"), c("EEE", "123"), -3, 2, FALSE, Sys.Date(), "")

  # wrong format
  for (param in test_params) {
    # base_currency
    expect_error(EXR::get_exchange_rate_history(base_currency = param, price_currency = "EUR"),
      regexp = ".*All elements of 'base_currency' must be an ISO4217 three-letter code.*", label = paste("Configuration", param)
    )

    # price_currency
    expect_error(EXR::get_exchange_rate_history(price_currency = param, base_currency = "EUR"),
      regexp = ".*All elements of 'price_currency' must be an ISO4217 three-letter code.*", label = paste("Configuration", param)
    )
  }

  # ========== scalar parameters ===========#

  result <- EXR::get_exchange_rate_history(base_currency = "EUR", price_currency = "EUR", periodicity = "M")

  expect_true(nrow(result) > 9 && nrow(result) < 15)
  expect_true(ncol(result) >= 7)
  expect_true(all(result$value == 1))
  expect_true(all(result$raw == FALSE))
  expect_true(all(result$base_currency == "EUR"))
  expect_true(all(result$price_currency == "EUR"))

  result <- EXR::get_exchange_rate_history(base_currency = "USD", price_currency = "USD", periodicity = "M")

  expect_true(nrow(result) > 9 && nrow(result) < 15)
  expect_true(all(result$value == 1))
  expect_true(all(result$raw == FALSE))
  expect_true(all(result$base_currency == "USD"))
  expect_true(all(result$price_currency == "USD"))

  result <- EXR::get_exchange_rate_history(base_currency = "EUR", price_currency = "USD", periodicity = "M")
  result_eur_usd <- result

  expect_true(nrow(result) > 9 && nrow(result) < 15)
  expect_true(all(!is.na(result$value)))
  expect_true(all(result$raw == TRUE))
  expect_true(all(result$base_currency == "EUR"))
  expect_true(all(result$price_currency == "USD"))

  result <- EXR::get_exchange_rate_history(base_currency = "USD", price_currency = "EUR", periodicity = "M")

  expect_true(nrow(result) > 9 && nrow(result) < 15)
  expect_true(all(!is.na(result$value)))
  expect_true(all(result$raw == FALSE))
  expect_true(all(result$price_currency == "EUR"))
  expect_true(all(result$base_currency == "USD"))

  expect_equal(
    result_eur_usd,
    get_exchange_rate_history(base_currency = c("EUR", "EUR", "EUR"), price_currency = c("USD", "USD"), periodicity = "M")
  )

  # ========== vector parameters ===========#

  result <- EXR::get_exchange_rate_history(base_currency = "EUR", price_currency = c("USD", "THB"), periodicity = "M")
  expect_true(nrow(result) > 2 * 9 & nrow(result) < 2 * 15)
  expect_true(nrow(result[result$price_currency == "USD", ]) > 9 & nrow(result[result$price_currency == "USD", ]) < 15)
  expect_true(all(!is.na(result$value)))
  expect_true(all(result$raw == TRUE))
  expect_true(all(result$price_currency == "USD" | result$price_currency == "THB"))
  expect_true(all(result$base_currency == "EUR"))

  result <- EXR::get_exchange_rate_history(base_currency = "GBP", price_currency = c("USD", "THB"), periodicity = "M")
  expect_true(nrow(result) > 2 * 9 & nrow(result) < 2 * 15)
  expect_true(nrow(result[result$price_currency == "USD", ]) > 9 & nrow(result[result$price_currency == "USD", ]) < 15)
  expect_true(all(!is.na(result$value)))
  expect_true(all(result$raw == FALSE))
  expect_true(all(result$price_currency == "USD" | result$price_currency == "THB"))
  expect_true(all(result$base_currency == "GBP"))

  result <- EXR::get_exchange_rate_history(price_currency = "GBP", base_currency = c("USD", "THB"), periodicity = "M")
  expect_true(nrow(result) > 2 * 9 & nrow(result) < 2 * 15)
  expect_true(nrow(result[result$base_currency == "USD", ]) > 9 & nrow(result[result$base_currency == "USD", ]) < 15)
  expect_true(all(!is.na(result$value)))
  expect_true(all(result$raw == FALSE))
  expect_true(all(result$base_currency == "USD" | result$base_currency == "THB"))
  expect_true(all(result$price_currency == "GBP"))

  result <- EXR::get_exchange_rate_history(base_currency = c("GBP", "USD"), price_currency = c("EUR", "THB"), periodicity = "M")
  expect_true(nrow(result) > 4 * 9 & nrow(result) < 4 * 15)
  expect_true(nrow(result[result$base_currency == "USD", ]) > 2 * 9 & nrow(result[result$base_currency == "USD", ]) < 2 * 15)
  expect_true(nrow(result[result$price_currency == "THB", ]) > 2 * 9 & nrow(result[result$price_currency == "THB", ]) < 2 * 15)
  expect_true(all(!is.na(result$value)))
  expect_true(all(result$raw == FALSE))
  expect_true(all(result$price_currency == "EUR" | result$price_currency == "THB"))
  expect_true(all(result$base_currency == "GBP" | result$base_currency == "USD"))

  result <- EXR::get_exchange_rate_history(base_currency = "EUR", price_currency = c("EUR", "THB", "USD", "GBP"), periodicity = "M")
  expect_true(nrow(result) > 4 * 9 & nrow(result) < 4 * 15)
  expect_true(nrow(result[result$raw == FALSE, ]) > 9 & nrow(result[result$raw == FALSE, ]) < 15)
  expect_true(all(!is.na(result$value)))
  expect_equal(length(result$price_currency |> unique()), 4)
  expect_true(all(result$base_currency == "EUR"))
})

test_that("get_exchange_rate_history param periodicity works as expected", {
  test_params <- list(NA, "423", "P", NULL, c("A", "D"), c("B"), list("EEE", "123"), -3, 2, FALSE, Sys.Date(), "")

  for (param in test_params) {
    expect_error(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = param, show_metadata = TRUE),
      regexp = ".*You tried to set .*periodicity.*but it must be any of.*A, D, H, M, Q.*", label = paste("Configuration", param)
    )
  }

  result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M")
  expect_true(all(result$periodicity == "M"))
  expect_true(nrow(result) > 7)

  result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "D")
  expect_true(all(result$periodicity == "D"))
  expect_true(nrow(result) > 100)
})

test_that("get_exchange_rate_history param context works as expected", {
  # context, standard is "A"
  test_params <- list(NA, 423, "P", NULL, c("A", "E"), list("A", "E"), -3, FALSE, Sys.Date(), "")
  for (param in test_params) {
    expect_warning(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M", context = param, show_metadata = TRUE),
      regexp = ".*You tried to set.*context.*but it must be any of.*A.*E.*", label = paste("Configuration", param)
    )
    expect_gt(nrow(result), expected = 4)
    expect_true(all(result$context == "A"))
  }

  # context, only option is "A" if you choose daily returns
  expect_message(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "D", context = "E", show_metadata = TRUE),
    regexp = ".*You tried to set.*context.*to.*end-of-period.*but.*periodicity.*is 'D'.*daily.*therefore not a period.*"
  )
  expect_gt(nrow(result), expected = 20)
  expect_true(all(result$context == "A"))

  expect_no_error(result_a <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M", context = "A", show_metadata = TRUE))
  expect_no_error(result_e <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M", context = "E", show_metadata = TRUE))
  expect_gt(nrow(result_a), expected = 6)
  expect_equal(nrow(result_a), nrow(result_e))
  expect_false(isTRUE(all.equal(result_a$value, result_e$value)))
  expect_true(all(result_a$context == "A"))
  expect_true(all(result_e$context == "E"))
})

test_that("get_exchange_rate_history param fill_missing_dates works as expected", {
  test_params <- list(NA, "423", "P", NULL, c(TRUE, FALSE), c("B"), list("EEE", "123"), -3, 2, Sys.Date(), "")
  for (param in test_params) {
    expect_error(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "D", fill_missing_dates = param),
      regexp = ".*You tried to set.*fill_missing_dates.*but it must be any of.*TRUE.*FALSE.*", label = paste("Configuration", param)
    )
  }

  expect_equal(
    EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M", fill_missing_dates = TRUE),
    EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M", fill_missing_dates = FALSE)
  )

  result_fill <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "D", fill_missing_dates = TRUE)
  result_not_fill <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "D", fill_missing_dates = FALSE)

  expect_gt(nrow(result_fill), nrow(result_not_fill))

  expect_true(all(result_not_fill$raw == TRUE))
  expect_false(all(result_fill$raw == TRUE))

  # more tests, see fill_gaps
})

test_that("get_exchange_rate_history param max_lookback_days works as expected", {
  result_not_filled <- EXR::get_exchange_rate_history(
    price_currency = "USD",
    base_currency = "EUR",
    fill_missing_dates = FALSE,
    periodicity = "D",
    max_lookback_days = 0,
    context = "A",
    show_metadata = TRUE
  )

  result_filled_lookback_0 <- EXR::get_exchange_rate_history(
    price_currency = "USD",
    base_currency = "EUR",
    fill_missing_dates = TRUE,
    periodicity = "D",
    max_lookback_days = 0,
    context = "A",
    show_metadata = TRUE
  )

  result_filled_lookback_null <- EXR::get_exchange_rate_history(
    price_currency = "USD",
    base_currency = "EUR",
    fill_missing_dates = TRUE,
    periodicity = "D",
    max_lookback_days = NULL,
    context = "A",
    show_metadata = TRUE
  )

  expect_gt(nrow(result_filled_lookback_0[is.na(result_filled_lookback_0$value), ]), expected = 20)
  expect_gt(nrow(result_filled_lookback_0), nrow(result_not_filled))
  expect_equal(result_filled_lookback_0, result_filled_lookback_null)
  expect_equal(result_not_filled, result_filled_lookback_0[!is.na(result_filled_lookback_0$value), ])


  # 0 if weird things happen
  test_params <- list(NA, "423", "P", c(TRUE, FALSE), c("B"), list("EEE", "123"), -3, FALSE, Sys.Date(), "")
  for (param in test_params) {
    expect_error(
      result <- EXR::get_exchange_rate_history(
        price_currency = "USD",
        base_currency = "EUR",
        fill_missing_dates = TRUE,
        periodicity = "D",
        max_lookback_days = param,
        context = "A",
        show_metadata = TRUE
      ),
      regexp = ".*You tried to set.*max_lookback_days.*but it must be a numeric integer not smaller than 0.*", label = paste("Configuration", param)
    )
  }
})

test_that("get_exchange_rate_history param show_metadata works as expected", {
  # show_metadata, TRUE as standard if you use wrong inputs

  test_params <- list(NA, "423", "P", NULL, c(TRUE, FALSE), c("B"), list("EEE", "123"), -3, 2, Sys.Date(), "")
  for (param in test_params) {
    expect_error(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M", show_metadata = param),
      regexp = ".*You tried to set.*show_metadata.*but it must be any of.*TRUE.*FALSE.*", label = paste("Configuration", param)
    )
  }

  result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M", show_metadata = TRUE)

  expect_gt(nrow(result), expected = 6)
  expect_true("raw" %in% colnames(result))
  expect_true("periodicity" %in% colnames(result))
  expect_true("context" %in% colnames(result))

  result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "M", show_metadata = FALSE)

  expect_gt(nrow(result), expected = 6)
  expect_false("raw" %in% colnames(result))
  expect_false("periodicity" %in% colnames(result))
  expect_false("context" %in% colnames(result))
})

test_that("get_exchange_rate_history param filter works as expected", {
  # ========== errors and for filter itself ===========#

  # filter, must be list
  test_params <- list(NA, "423", "P", TRUE, c(TRUE, FALSE), c("B"), -3, 2.5, Sys.Date(), "")
  for (param in test_params) {
    expect_error(EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", filter = param),
      regexp = ".*You tried to set.*filter.*but only.*list.*allowed.*", label = paste("Configuration", param)
    )
  }

  # unknown filter option
  expect_warning(EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", filter = list(hahaha = 2)),
    regexp = ".*undefined option .*"
  )

  expect_warning(EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", filter = list(hahaha = 2, hihihi = "a")),
    regexp = ".*undefined options .*hahaha.*hihihi.*"
  )

  expect_equal(
    EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = NULL),
    EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list())
  )

  # ========== startPeriod ===========#

  # must be a date
  test_params <- list(NA, "423", "P", TRUE, c(TRUE, FALSE), c("B"), list("EEE", "123"), -3, 2, "")
  for (param in test_params) {
    expect_error(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list(startPeriod = param)),
      regexp = ".*You tried to set.*filter.*startPeriod.*but only.*dates.*allowed.*try to use.*", label = paste("Configuration", param)
    )
  }

  # not date equals some date long time ago
  expect_equal(
    EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list(startPeriod = as.Date("1950-01-01"))),
    EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list(startPeriod = NULL))
  )

  # must not be in future!
  expect_error(EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list(startPeriod = Sys.Date() + 10)),
    regexp = ".*filter.*startPeriod.*must not be in the future.*"
  )

  # check for correct time frames in endPeriod checks

  # ========== endPeriod ===========#

  # filter endPeriod
  test_params <- list(NA, "423", "P", TRUE, c(TRUE, FALSE), c("B"), list("EEE", "123"), -3, 2, "")
  for (param in test_params) {
    expect_error(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list(endPeriod = param)),
      regexp = ".*You tried to set.*filter.*endPeriod.*but only.*dates.*allowed.*try to use.*", label = paste("Configuration", param)
    )
  }

  # not date equals today
  expect_equal(
    EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list()),
    EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list(endPeriod = NULL))
  )


  # start must be before end
  expect_error(EXR::get_exchange_rate_history(price_currency = "EUR", base_currency = "EUR", filter = list(startPeriod = Sys.Date() - 1, endPeriod = Sys.Date() - 2)),
    regexp = ".*filter.*startPeriod.*\\([0-9]{4}-[0-9]{2}-[0-9]{2}\\).*must not be after.*filter.*endPeriod.*\\([0-9]{4}-[0-9]{2}-[0-9]{2}\\).*"
  )

  expect_no_message(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "D", filter = list(
    startPeriod = Sys.Date() - 100,
    endPeriod = Sys.Date() - 10
  )))
  expect_gte(min(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 100)
  expect_lte(max(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 10)
  expect_true(nrow(result) > 10 && nrow(result) < 85)

  # one day without data
  expect_message(
    EXR::get_exchange_rate_history(
      "EUR",
      "USD",
      filter = list(
        startPeriod = as.Date("2022-12-31"),
        endPeriod = as.Date("2022-12-31")
      )
    ),
    regexp = ".*Result is empty. Please check parameters.*"
  )


  # ========== firstNObservations ===========#

  test_params <- list(NA, "423", "P", TRUE, c(TRUE, FALSE), c("B"), list("EEE", "123"), -3, 0.8, "")

  for (param in test_params) {
    expect_error(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list(firstNObservations = param)),
      regexp = ".*You tried to set.*filter.*firstNObservations.*but it must be a numeric integer not smaller than 1.*", label = paste("Configuration", param)
    )

    expect_error(
      result <- EXR::get_exchange_rate_history(
        price_currency = "USD", base_currency = "EUR", periodicity = "D", fill_missing_dates = TRUE, filter = list(firstNObservations = param)
      ),
      regexp = ".*You tried to set.*filter.*firstNObservations.*but.*fill_missing_dates.*TRUE.*", label = paste("Configuration", param)
    )
  }

  # there must be firstNObservations results - scalar case
  expect_no_message(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "D", filter = list(
    startPeriod = Sys.Date() - 100,
    endPeriod = Sys.Date() - 10,
    firstNObservations = 2
  )))

  expect_gte(min(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 100)
  expect_lte(max(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 90)
  expect_true(nrow(result) == 2)


  # there must be firstNObservations results for each combination, vector case
  expect_no_message(result <- EXR::get_exchange_rate_history(price_currency = c("THB", "USD"), base_currency = c("NOK", "EUR"), periodicity = "D", filter = list(
    startPeriod = Sys.Date() - 100,
    endPeriod = Sys.Date() - 10,
    firstNObservations = 2
  )))

  expect_gte(min(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 100)
  expect_lte(max(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 90)
  expect_true(nrow(result) == 4 * 2)


  # ========== lastNObservations ===========#

  test_params <- list(NA, "423", "P", TRUE, c(TRUE, FALSE), c("B"), list("EEE", "123"), -3, 0.8, "")

  for (param in test_params) {
    expect_error(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "A", filter = list(lastNObservations = param)),
      regexp = ".*You tried to set.*filter.*lastNObservations.*but it must be a numeric integer not smaller than 1.*", label = paste("Configuration", param)
    )

    expect_error(
      result <- EXR::get_exchange_rate_history(
        price_currency = "USD", base_currency = "EUR", periodicity = "D", fill_missing_dates = TRUE, filter = list(lastNObservations = param)
      ),
      regexp = ".*You tried to set.*filter.*lastNObservations.*but.*fill_missing_dates.*TRUE.*", label = paste("Configuration", param)
    )
  }


  # there must be lastNObservations results - scalar case
  expect_no_message(result <- EXR::get_exchange_rate_history(price_currency = "USD", base_currency = "EUR", periodicity = "D", filter = list(
    startPeriod = Sys.Date() - 100,
    endPeriod = Sys.Date() - 10,
    lastNObservations = 2
  )))

  expect_gte(min(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 20)
  expect_lte(max(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 10)
  expect_true(nrow(result) == 2)


  # there must be lastNObservations results for each combination, vector case
  expect_no_message(result <- EXR::get_exchange_rate_history(price_currency = c("THB", "USD"), base_currency = c("NOK", "EUR"), periodicity = "D", filter = list(
    startPeriod = Sys.Date() - 100,
    endPeriod = Sys.Date() - 10,
    lastNObservations = 2
  )))

  expect_gte(min(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 20)
  expect_lte(max(EXR::sdmx_character_to_date(result$period)), Sys.Date() - 10)
  expect_true(nrow(result) == 4 * 2)


  expect_error(EXR::get_exchange_rate_history(price_currency = "EUR", base_currency = "EUR", filter = list(firstNObservations = 1, lastNObservations = 1)),
    regexp = ".*You must not use both.*filter\\$firstNObservations.*and.*filter\\$lastNObservations.*"
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
