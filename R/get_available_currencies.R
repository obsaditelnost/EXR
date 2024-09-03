#' returns a `tibble` of available currencies and their English names
#'
#' @description
#' The available currencies at a specific date. For a full list of all currencies,
#' have a look at \href{https://www.iso.org/iso-4217-currency-codes.html}{ISO 4217}
#' or [ISOcodes::ISO_4217]
#'
#' @param date *the 'as of'-date*
#'
#'   `scalar<Date>` // *default:* `Sys.Date()-1` (`optional`)
#'
#'   The API will be searched for available currencies at this specific date. Therefore it is not
#'   allowed to query data in the future. If you are not sure whether there is any data at all for
#'   this specific date (e.g. weekend), use param `use_most_recent` to get a non-empty result
#'   (if `date` is not out of minimum date boundary of the database). Yesterday was chosen as
#'   default because during a day, the results are more likely to be the same (ECB updates on CET
#'   afternoon).
#'
#' @param use_most_recent *if no data is available, should most recent data be used?*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   `date` might be a Saturday, Sunday or the current day before EXR publication. If `use_most_recent`
#'   is `TRUE`, the API will be asked for the most recent date before the given date
#'
#' @returns a `tibble` with columns 'ISOCODE' and 'TITLE'
#'
#' @export
#'
#' @examples
#'
#' # get all available currencies for 2nd of Jan 2024
#' # (and don't look into historical data if there are no information available)
#' EXR::get_available_currencies(as.Date("2024-01-02"), use_most_recent = FALSE)
#'
#' # get all currencies for the most recent date in the database
#' EXR::get_available_currencies(Sys.Date())
#'
#' # get a vector of available ISO codes
#' as.vector(EXR::get_available_currencies()$ISOCODE)
#'
#' # show only the changes in available currencies between 2014 and 2024
#' library(dplyr)
#' dplyr::full_join(
#'   EXR::get_available_currencies(as.Date("2014-01-02")),
#'   EXR::get_available_currencies(as.Date("2024-01-02")),
#'   suffix = c("_2014", "_2024"),
#'   keep = TRUE
#' ) %>% .[!stats::complete.cases(.), ]
#'
get_available_currencies <- function(date = Sys.Date() - 1, use_most_recent = TRUE) {
  # check for correct inputs


  if (!inherits(date, what = "Date")) {
    cli::cli_abort(c("x" = "{.field date} must be of date datatype, see as.Date(), it is {.cls class(date)} though!"))
  }

  if (!(length(date) == 1)) cli::cli_abort(c("x" = "{.field date} must be scalar, not a vector!"))
  if (!(date <= Sys.Date())) cli::cli_abort(c("x" = "{.field date}  must not be in the future!"))

  if (!(use_most_recent == TRUE || use_most_recent == FALSE)) {
    cli::cli_abort(c("x" = "{.field use_most_recent} must be TRUE or FALSE, it is {.strong use_most_recent} though!"))
  }

  # max 7 days
  date_start <- format(date - use_most_recent * 7, "%Y-%m-%d")
  date_end <- format(date, "%Y-%m-%d")

  # get API response
  result <- EXR::perform_ecb_api_request(paste0("D..EUR.SP00.A?format=csvdata&startPeriod=", date_start, "&endPeriod=", date_end, "&lastNObservations=1"))

  # return results
  if (is.null(result)) {
    # valid request but no data
    tibble(ISOCODE = c(""), TITLE = c(""))[0, ]
  } else {
    result <- result |>
      readr::read_csv(show_col_types = FALSE)

    assert_is_df(result)

    tibble(
      ISOCODE = result$CURRENCY,
      TITLE = sub("/Euro", "", result$TITLE)
    ) |>
      dplyr::union(
        tibble(
          ISOCODE = c("EUR"),
          TITLE = c("Euro")
        )
      )
  }
}
