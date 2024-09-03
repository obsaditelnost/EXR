#' @keywords internal
#' @noRd
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr tibble
#' @importFrom dplyr tribble
#' @importFrom tidyr %>%
## usethis namespace: end

# get rid of R CMD check "no visible binding for global variable"
utils::globalVariables(c(
  "CURRENCY", "CURRENCY_DENOM", "EXR_SUFFIX", "FREQ", "KEY", "OBS_VALUE",
  "OBS_VALUE.denominator", "OBS_VALUE.numerator", "TIME_PERIOD", "period",
  "required_base_currency", "required_price_currency", "value", "."
))
