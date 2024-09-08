test_that("get_all_cross_rates w/o parameters work", {
  result <- EXR::get_all_cross_rates()
  expect_gt(nrow(result), 300)
  expect_equal(ncol(result), 3)
  expect_equal(result$base_currency |> unique(), result$price_currency |> unique())

  # check for one exchange rate and inverse
  expect_equal(
    1 * (result %>% .[.$base_currency == "EUR" & .$price_currency == "USD", 3]),
    1 / (result %>% .[.$base_currency == "USD" & .$price_currency == "EUR", 3])
  )

  # EUR/EUR is 1, same for all other currencies
  expect_true(
    all((result %>% dplyr::filter(base_currency == price_currency))$value == 1)
  )
})

test_that("get_all_cross_rates parameter 'currencies' works", {
  test_params <- list(NA, "423", "P", c("A", "E"), c("eee", "AAA"), list(a = "EEE", v = "123"), -3, 2, FALSE, Sys.Date(), "")

  # wrong format
  for (param in test_params) {
    expect_error(EXR::get_all_cross_rates(currencies = param),
      regexp = ".*All elements of .*must be an ISO4217 three-letter code.*", label = paste("Configuration", param)
    )
  }

  expect_no_error(result_eur <- EXR::get_all_cross_rates(currencies = "EUR"))
  expect_no_error(result_eurv <- EXR::get_all_cross_rates(currencies = c("EUR")))
  expect_no_error(result_eur_usd_gbp <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP")))
  expect_no_error(result_eur_usd_gbp_mult <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP", "EUR", "USD", "GBP", "EUR", "USD", "GBP", "EUR", "USD", "GBP")))

  expect_equal(result_eur, result_eurv)
  expect_equal(result_eur_usd_gbp, result_eur_usd_gbp_mult)
  expect_equal(nrow(result_eur), 1)
  expect_equal(nrow(result_eur_usd_gbp), 3^2)
})

test_that("get_all_cross_rates parameter 'date' works", {
  test_params <- list(NA, "423", "P", c("A", "E"), c("eee", "AAA"), list(a = "EEE", v = "123"), -3, 2, FALSE, "2023-12-12", "")

  # wrong format
  for (param in test_params) {
    expect_error(EXR::get_all_cross_rates(currencies = c("EUR", "USD"), date = param),
      regexp = ".*must be of date datatype, see as.Date.*, it is.*", label = paste("Configuration", param)
    )
  }

  expect_error(EXR::get_all_cross_rates(
    currencies = c("EUR", "USD"), date = c(Sys.Date() - 10, Sys.Date() - 20)),
    regexp = ".*must be scalar, not a vector.*"
  )

  expect_error(EXR::get_all_cross_rates(currencies = c("EUR", "USD"), date = Sys.Date() + 1), regexp = ".must not be in the future.*")
  expect_error(EXR::get_all_cross_rates(currencies = c("EUR", "USD"), date = Sys.Date() + 10), regexp = ".must not be in the future.*")

  # there should be a difference between 1 day but no difference for weekend exchange rates (because they are not published and lookback is applied)
  expect_no_error(result_20240816 <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-16")))
  expect_no_error(result_20240823 <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-23")))
  expect_no_error(result_20240824 <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-24")))

  expect_false(isTRUE(all.equal(result_20240816, result_20240823)))
  expect_equal(result_20240823, result_20240824)

  # check on lower level that result for a specific day is correct
  expect_equal(
    (result_20240816 %>% .[.$base_currency == "EUR" & .$price_currency == "USD", 3])[[1]],
    EXR::get_exchange_rate_history("EUR", "USD", periodicity = "D", filter = list(
      endPeriod = as.Date("2024-08-16"),
      lastNObservations = 1
    ))$value
  )
})


test_that("get_all_cross_rates parameter 'max_lookback_days' works", {
  test_params <- list(NA, "423", "P", c(1, 2), c("eee", "AAA"), list(a = "EEE", v = "123"), -3, FALSE, "2023-12-12", "")

  # wrong format
  for (param in test_params) {
    expect_error(EXR::get_all_cross_rates(currencies = c("EUR", "USD"), max_lookback_days = param),
      regexp = ".*You tried to set.*max_lookback_days.*to.*", label = paste("Configuration", param)
    )
  }

  # don't look back in history for more than x days

  expect_no_error(EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-25"), max_lookback_days = NULL))

  expect_no_error(result_20240823 <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-23")))
  expect_no_error(result_20240824_1lookback <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-24"), max_lookback_days = 1))
  expect_message(result_20240824_0lookback <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-24"), max_lookback_days = 0),
    regexp = ".*Result is empty. Please check your date configuration. You may want to use a higher value for.*"
  )
  expect_message(result_20240825_1lookback <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-25"), max_lookback_days = 1),
    regexp = ".*Result is empty. Please check your date configuration. You may want to use a higher value for.*"
  )
  expect_no_error(result_20240825_2lookback <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP"), date = as.Date("2024-08-25"), max_lookback_days = 2))

  expect_equal(nrow(result_20240823), 9)
  expect_equal(nrow(result_20240824_1lookback), 9)
  expect_equal(nrow(result_20240824_0lookback), 0)
  expect_equal(nrow(result_20240825_1lookback), 0)
  expect_equal(nrow(result_20240825_2lookback), 9)

  expect_equal(result_20240825_2lookback, result_20240824_1lookback)
  expect_equal(result_20240825_2lookback, result_20240823)
  expect_equal(result_20240824_0lookback, result_20240825_1lookback)
})

test_that("get_all_cross_rates parameter 'output_method' works", {
  test_params <- list(NA, "423", "P", c(1, 2), c("eee", "AAA"), list(a = "EEE", v = "123"), -3, FALSE, "2023-12-12", "")

  # wrong format
  for (param in test_params) {
    expect_error(EXR::get_all_cross_rates(currencies = c("EUR", "USD"), output_method = param),
      regexp = ".*You tried to set.*output_method.*to.*but it must be one of.*", label = paste("Configuration", param)
    )
  }

  expect_no_error(result_long <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP", "JPY"), date = as.Date("2024-08-23"), output_method = "long"))
  expect_no_error(result_deep <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP", "JPY"), date = as.Date("2024-08-23"), output_method = "deep"))
  expect_no_error(result_wide <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP", "JPY"), date = as.Date("2024-08-23"), output_method = "wide"))
  expect_no_error(result_pivot <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP", "JPY"), date = as.Date("2024-08-23"), output_method = "pivot"))


  expect_equal(result_long, result_deep)
  expect_equal(result_wide, result_pivot)

  expect_equal(nrow(result_long), 16)
  expect_equal(nrow(result_wide), 4)
  expect_equal(ncol(result_long), 3)
  expect_equal(ncol(result_wide), 4)

  expect_equal(
    (result_long %>% .[.$base_currency == "EUR" & .$price_currency == "USD", 3])[[1]],
    result_wide["EUR", "USD"]
  )

  expect_message(result_empty_long <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP", "JPY"), date = as.Date("2024-08-24"), max_lookback_days = 0, output_method = "long"),
    regexp = ".*Result is empty. Please check your date configuration. You may want to use a higher value for.*"
  )

  expect_message(result_empty_wide <- EXR::get_all_cross_rates(currencies = c("EUR", "USD", "GBP", "JPY"), date = as.Date("2024-08-24"), max_lookback_days = 0, output_method = "wide"),
    regexp = ".*Result is empty. Please check your date configuration. You may want to use a higher value for.*"
  )

  expect_equal(nrow(result_empty_long), 0)
  expect_equal(nrow(result_empty_wide), 0)
  expect_equal(ncol(result_empty_long), 3)
  expect_equal(ncol(result_empty_wide), 0)
})
