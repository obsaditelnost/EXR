test_that("perform_ecb_api_request works", {
  val_before <- getOption("EXR.http.timeout")

  # malformed string
  expect_error(EXR::perform_ecb_api_request(""))
  expect_error(EXR::perform_ecb_api_request(NULL))
  expect_error(EXR::perform_ecb_api_request("fjrejghroe"))

  # too long
  expect_error(EXR::perform_ecb_api_request("D..EUR.SP00.A?format=csvdata&startPeriod=2000-01-01&detail=dataonly", timeout = 1))

  # too long (by using option)
  options("EXR.http.timeout" = 1)
  expect_error(EXR::perform_ecb_api_request("D..EUR.SP00.A?format=csvdata&startPeriod=2000-01-01&detail=dataonly"))

  # too long
  options("EXR.http.timeout" = 30)
  expect_error(EXR::perform_ecb_api_request("D..EUR.SP00.A?format=csvdata&startPeriod=2000-01-01&detail=dataonly", timeout = 1))

  options("EXR.http.timeout" = NULL)

  expect_no_error(EXR::perform_ecb_api_request("A.USD.EUR.SP00.A?format=csvdata&startPeriod=2018&detail=dataonly"))
  expect_no_error(EXR::perform_ecb_api_request("A.USD.EUR.SP00.A?format=jsondata&startPeriod=2018&detail=dataonly"))

  retval <- EXR::perform_ecb_api_request("A.USD.EUR.SP00.A?format=csvdata&startPeriod=2018&detail=dataonly")

  # must be a string
  expect_type(retval, "character")
  expect_gt(nchar(retval), 100)

  # must be able to parse as CSV and contain content for properly formed query
  expect_no_error(readr::read_csv(retval, show_col_types = FALSE))

  retval <- readr::read_csv(retval, show_col_types = FALSE)

  # dimensions should be quite well defined and stable!
  expect_equal(ncol(retval), 8)
  expect_gt(nrow(retval), 2)

  # NULL if result is empty
  expect_null(EXR::perform_ecb_api_request("D.USD.EUR.SP00.A?format=csvdata&startPeriod=2024-08-03&endPeriod=2024-08-03&detail=dataonly"))

  # error if request is malformed and API detects it
  expect_error(EXR::perform_ecb_api_request("D.EUR.EUR.SP00.A?format=csvdata&startPeriod=2024-08-03&endPeriod=2024-08-03&detail=dataonly"))

  options("EXR.http.timeout" = val_before)
})
