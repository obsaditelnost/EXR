#' vectorised transformation from date to character (sdmx Reporting Periods)
#'
#' @param date *date value*
#'
#'   `scalar<Date>` or `vector<Date>` // **required**
#'
#'   Date values (`as.Date`). If length of vector is > 1 then it must be the same
#'   size as param `frequency`. If length of vector is 1, then it will be scaled to
#'   the size of frequency vector. sdmx notation (Statistical Data and Metadata eXchange)
#'
#' @param frequency *time related frequency of data*
#'
#'   `scalar<character>` or `vector<character>` // **required**
#'
#'    allowed values are either A, D, H, M, Q
#'    \itemize{
#'      \item **A**...nnualy
#'      \item **D**...aily
#'      \item **H**...alf-Yearly
#'      \item **M**...onthly
#'      \item **Q**...uarterly
#'    }
#'
#'   If length of `frequency` is > 1 then it must be either the same size as
#'   param `date` or `date` must be scalar. If length of vector is 1, then it will
#'   be scaled to the size of `date` vector
#'
#' @note intended for internal use
#'
#' @returns `scalar<character>` or `vector<character>`, depending on the input.
#'     Vector is compliant to sdmx notation (Statistical Data and Metadata eXchange)
#'
#' @references
#'    \itemize{
#'      \item \href{https://data.ecb.europa.eu/help/api/data}{ECB API Documentation}
#'      \item \href{https://sdmx.org/wp-content/uploads/sdmx_2-1_SECTION_6_TechnicalNotes.pdf}{sdmx Standards, technical notes}
#'    }
#'
#'
#' @export
#'
#' @seealso [sdmx_character_to_date()]
#'
#' @examples
#' # corresponding vectors
#' EXR::sdmx_date_to_character(
#'   date = as.Date(c("2024-12-31", "2024-08-20", "2023-12-31", "2022-07-31", "2021-12-31", NA)),
#'   frequency = c("A", "D", "H", "M", "Q", "D")
#' )
#'
#' # today's quarter
#' EXR::sdmx_date_to_character(Sys.Date(), "Q")
#'
#' # multiple formats for today
#' EXR::sdmx_date_to_character(Sys.Date(), c("A", "D", "H", "M", "Q"))
#'
#' # date vector with static conversion
#' EXR::sdmx_date_to_character(
#'   date = as.Date(c("2024-12-31", "2024-08-20", "2023-12-31", "2022-07-31", "2021-12-31", NA)),
#'   frequency = "A"
#' )
#'
sdmx_date_to_character <- function(date, frequency) {
  # ======= Parameter checks ===========

  if (!inherits(date, what = "Date")) {
    cli::cli_abort(c("x" = "All elements of {.field date} must be a Date, it is {.cls class(data)} though!"))
  }

  unrecognised <- frequency |>
    setdiff(c("A", "D", "H", "M", "Q")) |>
    unique()

  if (!(length(unrecognised) == 0)) {
    cli::cli_abort(c(
      "x" = "{.field frequency} must be be any of A, D, H, M, Q.",
      "i" = "Check: {.strong unrecognised}!"
    ))
  }

  if (!(length(date) == 1 || length(frequency) == 1 || length(date) == length(frequency))) {
    cli::cli_abort(c("x" = "Vector size of {.field date} (currently {length(date)}) must be 1 or the same as {.field frequency} (currently length(frequency))"))
  }

  # ======= / Parameter checks ===========

  # clean up vectors - stretch them to correct size if necessary
  if (length(date) == 1 && length(frequency) > 1) date <- rep.int(date, times = length(frequency))
  if (length(date) > 1 && length(frequency) == 1) frequency <- rep.int(frequency, times = length(date))

  dplyr::case_match(
    frequency,
    "A" ~ format(date, "%Y"),
    "D" ~ format(date, "%Y-%m-%d"),
    "H" ~ paste0(format(date, "%Y-S"), floor((as.integer(format(date, "%m")) - 1) / 6) + 1),
    "M" ~ format(date, "%Y-%m"),
    "Q" ~ paste0(format(date, "%Y-Q"), floor((as.integer(format(date, "%m")) - 1) / 3) + 1)
  )
}

#' vectorised transformation from character (sdmx Reporting Periods) to date
#'
#' @description
#' The function determines the format automatically and returns the corresponding date.
#' In case of a time period such as quarters or years, you can choose to return the first
#' or last day of that period. sdmx notation (Statistical Data and Metadata eXchange)
#'
#' @param char *date strings*
#'
#'   `scalar<character>` or `vector<character>` // **required**
#'
#'   Date representations such as "2012-Q2" for 2nd quarter of 2012.
#'
#' @param day_in_period *which day to return for a time period*
#'
#'   `scalar<character>` //  *default:* `last` (`optional`)
#'
#'   which day to return for a time period. Can be either "first" or "last" for
#'   the first day of a period or the last day. Calculation does not care about
#'   working days.
#'
#' @note intended for internal use
#'
#' @returns `scalar<Date>` or `vector<Date>`, depending on the input
#'
#' @references
#' \itemize{
#'      \item \href{https://data.ecb.europa.eu/help/api/data}{ECB API Documentation}
#'      \item \href{https://sdmx.org/wp-content/uploads/sdmx_2-1_SECTION_6_TechnicalNotes.pdf}{sdmx Standards, technical notes}
#' }
#'
#' @export
#'
#' @seealso [sdmx_date_to_character()]
#'
#' @examples
#' # different values
#' EXR::sdmx_character_to_date(c("2024", "2024-08-20", "2023-S2", "2022-07", "2021-Q4", NA))
#'
#' # different values, but return first day of period instead of last day
#' EXR::sdmx_character_to_date(
#'   char = c("2024", "2024-08-20", "2023-S2", "2022-07", "2021-Q4", NA),
#'   day_in_period = "first"
#' )
#'
#' # one single transformation
#' EXR::sdmx_character_to_date("2023")
sdmx_character_to_date <- function(char, day_in_period = "last") {
  if (length(char) == 0) NULL

  if(inherits(char, what = "Date")) char <- format(char,"%Y-%m-%d")

  if (!(inherits(char, what = "character") || all(is.na(char)))) {
    cli::cli_abort(c("x" = "All elements of {.field char} must be a character, it is {.cls class(char)} though!"))
  }
  if (!(all(nchar(char[!is.na(char)]) >= 4))) {
    cli::cli_abort(c("x" = "All elements of {.field char} must be be at least 4 character long!"))
  }
  if (!(is.atomic(day_in_period) && day_in_period %in% c("first", "last"))) {
    cli::cli_abort(c("x" = "{.field day_in_period} must be must be atomic and either 'first' or 'last', it is {.strong day_in_period} though!"))
  }

  day_in_period <- rep.int(day_in_period, times = length(char))

  dplyr::case_when(
    # A...nnualy
    grepl("^[0-9]{4}$", char) ~ dplyr::if_else(day_in_period == "last",
      as.Date(paste0(char, "-12-31"), "%Y-%m-%d", optional = TRUE),
      as.Date(paste0(char, "-01-01"), "%Y-%m-%d", optional = TRUE)
    ),
    # D..aily
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", char) ~ as.Date(char, format = "%Y-%m-%d"),
    # H...alf-yearly (1st half)
    grepl("^[0-9]{4}-S1$", char) ~ dplyr::if_else(day_in_period == "last",
      as.Date(paste0(substr(char, 1, 4), "-06-30"), "%Y-%m-%d", optional = TRUE),
      as.Date(paste0(substr(char, 1, 4), "-01-01"), "%Y-%m-%d", optional = TRUE)
    ),
    # H...alf-yearly (2nd half)
    grepl("^[0-9]{4}-S2$", char) ~ dplyr::if_else(day_in_period == "last",
      as.Date(paste0(substr(char, 1, 4), "-12-31"), "%Y-%m-%d", optional = TRUE),
      as.Date(paste0(substr(char, 1, 4), "-07-01"), "%Y-%m-%d", optional = TRUE)
    ),
    # M...onthly
    grepl("^[0-9]{4}-[0-9]{2}$", char) ~ zoo::as.Date.yearmon(zoo::as.yearmon(char, format = "%Y-%m"),
      frac = dplyr::if_else(day_in_period == "last", 1, 0)
    ),
    # Q...uarterly
    grepl("^[0-9]{4}-Q[1-4]$", char) ~ zoo::as.Date.yearqtr(zoo::as.yearqtr(char, format = "%Y-Q%q"),
      frac = dplyr::if_else(day_in_period == "last", 1, 0)
    ),
    TRUE ~ NA
  )
}


#' assert_is_df aborts program if object isn't a data.frame-like class
#'
#' @param object some object
#'
#' @param error_msg *message to show when program aborts*
#'
#'   `scalar<character>`  //  *default:* `ECB Data API response is not a valid CSV file. Something must have gone very wrong. Most likely this is not your fault.` (`optional`)
#'
#'    in this package this function is only used for ECB Data API response... therefore the standard message
#'
#' @returns nothing, but may abort if object is not a data.frame-like class
#' @keywords internal
#' @noRd
#'
#' @examples
#' assert_is_df("hfio", "This is not a df")
assert_is_df <- function(object, error_msg = "ECB Data API response is not a valid CSV file. Something must have gone very wrong. Most likely this is not your fault.") {
  if (!is.data.frame(object)) {
    cli::cli_abort(c(
      "x" = error_msg,
      "i" = "Object: {.code {object}}"
    ))
  }
}
