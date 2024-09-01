test_that("multiplication works", {
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
  expect_equal(2 * 2, 4)
})
