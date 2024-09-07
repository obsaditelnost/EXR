#' returns the cross-rates (or exchange rates) for a specific day
#'
#' @description
#' get_all_cross_rates will take a number of currencies as input and return
#' all exchange rates between them. If the ECB does not publish them, cross-rates
#' will be calculated based on EUR-based currency pairs
#'
#' @param currencies  *currency or currency list*
#'
#'   `scalar<character>` or `vector<character>` // *default:* `NULL` for all (`optional`)
#'
#'   If you want specific currencies, provide them to the function as a vector, such
#'   as c("USD","THB")
#'
#'
#' @param date *the as-of-date*
#'
#'   `scalar<Date>` //  *default:* `Sys.Date()-1` (`optional`)
#'
#'   Data will be displayed for this date. If there is no data for this sopecific date,
#'   the package will go back in history and look for most recent exchange rates. You
#'   can limit this by `max_lookback_days`
#'
#' @param max_lookback_days *look back if missing for not more than X days*
#'
#'   `scalar<numeric>` //  *default:* `7` (`optional`)
#'
#'   At `date` there might be missing data because ECB will only publish data on working days.
#'   The values in `EXR::get_all_cross_rates` are taken from the most recent day before. If
#'   data is too old, you might not want to use outdated exchange rates though. Set the maximum
#'   number of calender days you want to go back with `max_lookback_days`. If you don't want to use
#'   data of previous days, set this parameter to `0` or NULL
#'
#' @param output_method *how do you want the output to be formatted*
#'
#'   `scalar<character>` //  *default:* `long` (`optional`)
#'
#'    \itemize{
#'      \item **long**/**deep**: output will be a deep table with columns base_currency, price_currency, value
#'      \item **wide**/**pivot**: output will be a wide (pivot) table with base_currency as rows and price_currency as columns, value as combination
#'    }
#'
#' @export
#'
#' @examples
#' # get all possible cross-rates / exchange rates as of yesterday
#' EXR::get_all_cross_rates()
#'
#' # get all combinations for a set of currencies as of end 2023 and show as pivot table
#' EXR::get_all_cross_rates(currencies = c("EUR", "USD", "JPY", "GBP", "CNY"), date = as.Date("2023-12-31"), output_method = "pivot")
#'
get_all_cross_rates <- function(currencies = NULL, date = Sys.Date() - 1, max_lookback_days = 7, output_method = "long") {
  # ====================================
  # ======= Parameter checks ===========
  # ====================================

  # currencies

  if (is.null(currencies)) currencies <- EXR::get_available_currencies(date)$ISOCODE

  if (!(all(grepl("^[A-Z]{3}$", currencies)) && is.character(currencies) && length(currencies) > 0)) {
    cli::cli_abort(c("All elements of 'currencies' must be an ISO4217 three-letter code.",
      "x" = "Check {?this/these} code{?s}: {dplyr::coalesce(currencies,\"NULL\")[!grepl(\"^[A-Z]{3}$\", dplyr::coalesce(currencies,\"NULL\"))]}"
    ))
  }

  # date

  if (!inherits(date, what = "Date")) {
    cli::cli_abort(c("x" = "{.field date} must be of date datatype, see as.Date(), it is {.cls class(date)} though!"))
  }

  if (!(length(date) == 1)) cli::cli_abort(c("x" = "{.field date} must be scalar, not a vector!"))
  if (!(date <= Sys.Date())) cli::cli_abort(c("x" = "{.field date}  must not be in the future!"))

  # max_lookback_days

  if (!is.numeric(max_lookback_days) || length(max_lookback_days) != 1 || max_lookback_days < 0 || round(max_lookback_days) != max_lookback_days) {
    cli::cli_warn(c(
      "x" = "You tried to set {.field max_lookback_days} to '{max_lookback_days}' {.cls {class(max_lookback_days)}}
              but it must be a numeric integer not smaller than 0",
      "i" = "{.field max_lookback_days} is set to '{.strong 0}' instead."
    ))
    max_lookback_days <- 0
  }

  # output_method

  if (is.null(output_method) || !is.scalar(output_method) || !(output_method %in% c("wide", "pivot", "long", "deep"))) {
    cli::cli_abort(c(
      "x" = "You tried to set {.field output_method} to '{output_method}' {.cls {class(output_method)}}
              but it must be one of '{.strong c('wide','pivot','long','deep')}'"
    ))
  }

  # ====================================
  # ======= execute request  ===========
  # ====================================

  result <- EXR::get_exchange_rate_history(
    base_currency = currencies,
    price_currency = currencies,
    periodicity = "D",
    context = "A",
    show_metadata = FALSE,
    filter = list(
      startPeriod = date - max_lookback_days,
      endPeriod = date,
      firstNObservations = NULL,
      lastNObservations = 1
    )
  )
}
