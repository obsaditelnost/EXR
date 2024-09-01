test_that("EXR", {
  expect_equal(
    dplyr::starwars %>% head(1),
    dplyr::starwars |> head(1)
  )
})
