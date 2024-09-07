test_that("sdmx_date_to_character works with atomic values", {
  # wrong inputs w.r.t. frequency
  expect_error(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = "?"))
  expect_error(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = "X"))
  expect_error(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = 1))
  expect_error(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = NULL))
  expect_error(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = c()))

  # wrong inputs w.r.t. date
  expect_error(EXR::sdmx_date_to_character(date = "2022-01-01", frequency = "M"))
  expect_error(EXR::sdmx_date_to_character(date = 1, frequency = "M"))
  expect_error(EXR::sdmx_date_to_character(date = NULL, frequency = "M"))
  expect_error(EXR::sdmx_date_to_character(date = c("jfvopeds", 2, as.Date("2022-01-01")), frequency = "M"))

  # function works for single values

  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = "D"), "2022-01-01")
  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-03-02"), frequency = "D"), "2022-03-02")
  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = "H"), "2022-S1")
  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-12-01"), frequency = "H"), "2022-S2")
  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = "A"), "2022")
  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = "Q"), "2022-Q1")
  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-09-01"), frequency = "Q"), "2022-Q3")
  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-01-01"), frequency = "M"), "2022-01")
  expect_equal(EXR::sdmx_date_to_character(date = as.Date("2022-12-01"), frequency = "M"), "2022-12")
})

test_that("sdmx_date_to_character works with vectors", {
  # 2 vectors of size n return vector of size n... NA allowed, returns NA
  expect_equal(
    EXR::sdmx_date_to_character(
      date = as.Date(c("2024-12-31", "2024-08-20", "2023-12-31", "2022-07-31", "2021-12-31", NA)),
      frequency = c("A", "D", "H", "M", "Q", "D")
    ),
    c("2024", "2024-08-20", "2023-S2", "2022-07", "2021-Q4", NA)
  )

  # different vector size not allowed
  expect_error(
    EXR::sdmx_date_to_character(
      date = as.Date(c("2024-12-31", "2024-08-20", "2023-12-31", "2022-07-31", "2021-12-31", NA)),
      frequency = c("A", "D", "H", "M", "Q")
    )
  )

  expect_error(
    EXR::sdmx_date_to_character(
      date = as.Date(c("2024-12-31", "2024-08-20", "2023-12-31", "2022-07-31")),
      frequency = c("A", "D", "H", "M", "Q")
    )
  )

  # different vector size only allowed if the one parameter is scalar
  expect_equal(EXR::sdmx_date_to_character(
    date = as.Date(c("2024-12-31", "2024-08-20", "2023-12-31", "2022-07-31")),
    frequency = "A"
  ), c("2024", "2024", "2023", "2022"))

  expect_equal(EXR::sdmx_date_to_character(
    date = as.Date(c("2024-12-31")),
    frequency = c("A", "D", "H", "M", "Q")
  ), c("2024", "2024-12-31", "2024-S2", "2024-12", "2024-Q4"))

  # NA not allowed for freq

  expect_error(
    EXR::sdmx_date_to_character(
      date = as.Date(c("2024-12-31", "2024-08-20", "2023-12-31", "2022-07-31")),
      frequency = NA
    )
  )
})

test_that("sdmx_character_to_date works", {
  # day_in_period must be atomic: either first or last
  expect_error(EXR::sdmx_character_to_date(char = "2023", day_in_period = NULL))
  expect_error(EXR::sdmx_character_to_date(char = "2023", day_in_period = 1))
  expect_error(EXR::sdmx_character_to_date(char = "2023", day_in_period = c("first", "first")))

  expect_error(EXR::sdmx_character_to_date(char = "203", day_in_period = c("first", "first")))
  expect_error(EXR::sdmx_character_to_date(char = c("2023", "202"), day_in_period = c("first", "first")))

  # wrong datatype, but package will take care of it and assume yyyy-mm-dd
  expect_equal(
    EXR::sdmx_character_to_date(char = c(as.Date("2024-12-31"), as.Date("2024-08-20"), NA), day_in_period = "first"),
    EXR::sdmx_character_to_date(char = c("2024-12-31", "2024-08-20", NA), day_in_period = "first")
  )

  # day_in_period must be atomic: either first or last
  expect_no_error(EXR::sdmx_character_to_date(char = "2023", day_in_period = "first"))
  expect_no_error(EXR::sdmx_character_to_date(char = "2023", day_in_period = c("last")))

  # char must be a character (vector), error if not
  expect_error(EXR::sdmx_character_to_date(char = 2023))
  expect_error(EXR::sdmx_character_to_date(char = NULL))

  # NA as single value allowed though
  expect_true(is.na(EXR::sdmx_character_to_date(char = NA)))

  # all conversions are supposed to work
  expect_equal(
    EXR::sdmx_character_to_date(
      char = c(NA, "2024", "2024-08-20", "2023-S2", "2023-S1", "2022-07", "2022-02", "2021-Q4", "2021-Q2", NA),
      day_in_period = "first"
    ),
    as.Date(c(
      NA, as.Date("2024-01-01"), as.Date("2024-08-20"),
      as.Date("2023-07-01"), as.Date("2023-01-01"), as.Date("2022-07-01"),
      as.Date("2022-02-01"), as.Date("2021-10-01"), as.Date("2021-04-01"), NA
    ))
  )

  expect_equal(
    EXR::sdmx_character_to_date(
      char = c(NA, "2024", "2024-08-20", "2023-S2", "2023-S1", "2022-07", "2022-02", "2021-Q4", "2021-Q2", NA),
      day_in_period = "last"
    ),
    as.Date(c(
      NA, as.Date("2024-12-31"), as.Date("2024-08-20"),
      as.Date("2023-12-31"), as.Date("2023-06-30"), as.Date("2022-07-31"),
      as.Date("2022-02-28"), as.Date("2021-12-31"), as.Date("2021-06-30"), NA
    ))
  )
})


test_that("sdmx_character_to_date works hand in hand with sdmx_date_to_character", {
  testdata <- as.Date(c(NA, "2024-12-31", "2024-08-30", NA, "2023-12-31", "2022-07-31", "2022-03-31"))
  expect_equal(
    EXR::sdmx_character_to_date(EXR::sdmx_date_to_character(
      date = testdata,
      frequency = c("A", "A", "D", "A", "H", "M", "Q")
    )),
    testdata
  )
})

test_that("assert_is_df works", {
  expect_error(assert_is_df("hfio", "This is not a df"))
  expect_error(assert_is_df(NULL, "This is not a df"))
  expect_error(assert_is_df(1, "This is not a df"))
  expect_no_error(assert_is_df(data.frame(a = 1), "This is not a df"))
  expect_no_error(assert_is_df(data.frame(a = NULL), "This is not a df"))
  expect_no_error(assert_is_df(dplyr::tibble(1), "This is not a df"))
  expect_no_error(assert_is_df(dplyr::tibble(NULL), "This is not a df"))
})
