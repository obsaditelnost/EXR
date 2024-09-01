#' performs a GET-request against the ECB Data API
#'
#' @description
#' the functionality is constrained to EXR-data. It's primarily intended for
#' internal use. For access to the whole API with all ECB data but without any
#' further functionality have a look at the package [ecb::get_data].
#'
#' @param query *the EXR request with both key and parameters*
#'
#'   `scalar<character>` // **required**
#'
#'   A query compliant to the \href{https://data.ecb.europa.eu/help/api/data}{official documentation}.
#'   A genereic form of the whole API-request is
#'    \itemize{
#'      \item \code{protocol://wsEntryPoint/resource/flowRef/key?parameters}
#'    }
#'   but for this function, everything but `key?parameters` is already set automatically.
#'
#'   Example requests might be:
#'    \itemize{
#'      \item \code{A.USD.EUR.SP00.A?format=csvdata&startPeriod=2018&detail=dataonly}
#'      \item \code{D.USD.EUR.SP00.A}
#'    }
#'
#' @param timeout *https request timeout in seconds*
#'
#'   `scalar<numeric>` // *default:* NULL (`optional`)
#'
#'   The free API is not very fast, if you query large amounts of data, you might have
#'   to increase the number. The actual amount of data is not limited but timeout is set to
#'   \code{getOption("EXR.http.timeout", default = 30)} by this package. Therefore
#'   you may set \code{options("EXR.http.timeout" = x)} to override the default value.
#'
#'   Parameter `timeout` has highest priority though and overrides global options
#'
#' @return `scalar<character>`, the return value of [httr2::resp_body_string()]. If body
#'    is empty, NULL will be returned.
#' @export
#'
#' @references \href{https://data.ecb.europa.eu/help/api/data}{https://data.ecb.europa.eu/help/api/data}
#'
#' @examples
#'
#' # get annual average USD/EUR exchange rates in CSV-format, starting from 2018
#' EXR::perform_ecb_api_request("A.USD.EUR.SP00.A?format=csvdata&startPeriod=2018&detail=dataonly")
#'
#' # same request but constrained to 1 second
#' EXR::perform_ecb_api_request("A.USD.EUR.SP00.A?format=csvdata&startPeriod=2018&detail=dataonly", 1)
perform_ecb_api_request <- function(query, timeout = NULL) {

  stopifnot("Please provide a properly formatted query!" = grepl("^[A-Z]\\..+[?].*$", query) && !is.null(query))

  timeout <- dplyr::coalesce(timeout, as.integer(getOption("EXR.http.timeout", default = 30)))

  entry_point <- "https://data-api.ecb.europa.eu"
  resource <- "data"
  flow_ref <- "EXR"

  url <- paste(entry_point, "service", "data", flow_ref, query, sep = "/")
  result <- httr2::request(url) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform()

  if (httr2::resp_has_body(result)) {
    httr2::resp_body_string(result)
  } else {
    NULL
  }
}
