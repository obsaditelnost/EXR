#' returns the exchange rate history of one or more currency pairs
#'
#' @description
#' Time series in a deep table for each currency combination derived from defined
#' base and price currency. The `periodicity` and date-related filters can be
#' applied. This function is a wrapper around the official API and actually returns
#' error- and warning-messages instead of just a 404 error.
#'
#' @details
#' Central function for some other convenience-functions in this package..
#' `get_exchange_rate_history()` may return raw data or data calculated by this
#' package based on raw data. The ECB reference rates are usually updated at
#' around 16:00 CET every working day, except on TARGET closing days. They are
#' based on the daily concertation procedure between central banks across Europe,
#' which normally takes place around 14:10 CET. The reference rates are published
#' for information purposes only. Using the rates for transaction purposes is
#' strongly discouraged.
#'
#' The exchange rate \eqn{USD/EUR = 1.1} with EUR as denominator (base currency)
#' shows that you'll get 1.1 USD-units (price currency) for 1 unit of EUR (base currency).
#' This package uses this exchange rate logic without arguing about direct or indirect
#' quotations. Also, there are no intraday, mid, ask or bid prices - only reference rates.
#'
#' The actual amount of data is not limited but timeout is set to
#' \code{getOption("EXR.http.timeout", default = 30)} by this package. Therefore
#' you may set \code{options("EXR.http.timeout" = x)} to override the default value.
#'
#' @param base_currency  *base currency*
#'
#'   `scalar<character>` or `vector<character>` // **required**
#'
#'   Must be an 'ISO_4217' 3-Letter-Code for currencies. Can be either a single
#'   value or multiple values in a vector. If multiple values are provided in
#'   either `base_currency` or `price_currency`, all combinations will be
#'   calculated.
#'
#' @param price_currency *price currency*
#'
#'   `scalar<character>` or `vector<character>` // **required**
#'
#'   Must be an 'ISO_4217' 3-Letter-Code for currencies. Can be either a single
#'   value or multiple values in a vector. If multiple values are provided in
#'   either `base_currency` or `price_currency`, all combinations will be
#'   calculated.
#'
#' @param periodicity *time related frequency/periodicity of data*
#'
#'   `scalar<character>` //  *default:* `D` for daily (`optional`)
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
#' @param context *in a period, what kind of value to chose*
#'
#'   `scalar<character>` //  *default:* `A` (`optional`)
#'
#'    allowed values are either A or E:
#'    \itemize{
#'      \item **A**...verage (can be used for any periodicity)
#'      \item **E**...nd-Of-Period (must not be used for daily periodicity)
#'    }
#'
#' @param fill_missing_dates *add missing days at daily frequency?*
#'
#'   `scalar<logical>` //  *default:* `FALSE` (`optional`)
#'
#'   If `periodicity`=`D`, there might be missing days because ECB will only publish
#'   data on working days. TRUE fills the missing public holidays, weekends, ...
#'   The values are taken from the most recent day before. If data is too old, you
#'   might not want to use outdated exchange rates though. See: param `max_lookback_days`
#'
#'   If `periodicity`!=`D`, the parameter `fill_missing_dates` has no effect
#'
#' @param max_lookback_days *look back if missing for not more than X days*
#'
#'   `scalar<numeric>` //  *default:* `7` (`optional`)
#'
#'   If `periodicity`=`D`, there might be missing days because ECB will only publish
#'   data on working days. `fill_missing_dates`=TRUE fills the missing public holidays, weekends, ...
#'   The values are taken from the most recent day before. If data is too old, you
#'   might not want to use outdated exchange rates though. Set the maximum number of calender
#'   days you want to go back with `max_lookback_days`. If you don't want to use
#'   data of previous days, set this parameter to `0` or NULL
#'
#'   This parameter only has an effect if both `periodicity`=`D` and `fill_missing_dates`=`TRUE`
#'
#' @param show_metadata *return additional columns?*
#'
#'   `scalar<logical>` //  *default:* `TRUE` (`optional`)
#'
#'    \itemize{
#'      \item **TRUE**: the requested periodicity, context and information about being
#'          raw or calculated will be shown
#'      \item **FALSE**: only necessary columns will be returned
#'    }
#'
#' @param filter *list of filter arguments to reduce data*
#'
#'   `list` //  *default:* \code{ list( startPeriod = Sys.Date() - 370, endPeriod = Sys.Date() - 1,
#'                                firstNObservations = NULL, lastNObservations = NULL) } (`optional`)
#'
#'   You can define the following arguments:
#'    \itemize{
#'      \item **startPeriod** `<Date>` first date of data.
#'          If you set `periodicity`!=`D`, just choose one date in your starting period,
#'          (e.g. the last day) it will be converted *automatically* to:
#'         \itemize{
#'           \item YYYY for annual data (e.g. 2013)
#'           \item YYYY-S\[1-2\] for semi-annual data (e.g. 2013-S1)
#'           \item YYYY-Q\[1-4\] for quarterly data (e.g. 2013-Q1)
#'           \item YYYY-MM for monthly data (e.g. 2013-01)
#'           \item YYYY-MM-DD for daily data (e.g. 2013-01-01)
#'         }
#'         This means that the whole period will be covered, even if some days in that
#'         period are before `startPeriod`. This is only relevant if `context` == `A` though
#'      \item **endPeriod** `<Date>` last date of data.
#'          If you set `periodicity`!=`D`, just choose one date in your starting period,
#'          (e.g. the last day) it will be converted *automatically* (see startPeriod)
#'      \item **firstNObservations** `<numeric>` (`optional`) get the first N observations.
#'          Must be `NULL` (=all observations) if `fill_missing_dates`=`TRUE`
#'      \item **lastNObservations** `<character>` (`optional`) get the last N observations.
#'          Must be `NULL` (=all observations) if `fill_missing_dates`=`TRUE`. Don't use
#'          together with 'firstNObservations'
#'    }
#'
#'  Set filter to NULL for no filters at all
#'
#' @returns a `tibble` with the following columns
#'    \itemize{
#'      \item **base_currency** `<character>`⁠ (\href{https://www.iso.org/iso-4217-currency-codes.html}{ISO 4217}) 3-Letter currency code
#'      \item **price_currency** `<character>`⁠ (\href{https://www.iso.org/iso-4217-currency-codes.html}{ISO 4217}) 3-Letter currency code
#'      \item **period** `<character>` period of exchange rate
#'         \itemize{
#'           \item YYYY for annual data (e.g. 2013)
#'           \item YYYY-S\[1-2\] for semi-annual data (e.g. 2013-S1)
#'           \item YYYY-Q\[1-4\] for quarterly data (e.g. 2013-Q1)
#'           \item YYYY-MM for monthly data (e.g. 2013-01)
#'           \item YYYY-MM-DD for daily data (e.g. 2013-01-01)
#'         }
#'         If you want to transform this string to da date value, have a look at [EXR::sdmx_character_to_date()]
#'      \item **value** `<numeric>` the exchange rate, might be NA
#'    }
#'    and these columns if `show_metadata` = `TRUE`
#'    \itemize{
#'      \item **periodicity** `<character>` the requested periodicity A, D, H, M, Q
#'      \item **context** `<character>` the requested context E or A
#'      \item **raw** `<logical>`
#'         \itemize{
#'           \item \code{TRUE} if data was published as returned.
#'           \item \code{FALSE} if cross-rate or inverse was calculated by the tool or missing data was filled
#'         }
#'
#'    }
#'
#' @export
#'
#' @examples
#' # get semi annual data for the last year and a total of 8 currency combinations
#' EXR::get_exchange_rate_history(
#'   base_currency = c("EUR", "THB"),
#'   price_currency = c("EUR", "USD", "GBP", "THB"),
#'   periodicity = "H"
#' )
#'
#' # get USD/EUR exchange rates for the last year,
#' # fill missing dates and look for missing values max 1 day
#' # so there won't be exchange rates on Sundays or subsequent holidays
#' EXR::get_exchange_rate_history(
#'   base_currency = "EUR",
#'   price_currency = "USD",
#'   periodicity = "D",
#'   fill_missing_dates = TRUE,
#'   max_lookback_days = 1
#' )
#'
#' # get most recent EXR for USD/EUR - filtered directly by the API
#' # EUR/USD won't work that way because the API doesn't know that combination
#' EXR::get_exchange_rate_history(
#'   base_currency = "EUR",
#'   price_currency = "USD",
#'   show_metadata = FALSE,
#'   filter = list(
#'     endPeriod = Sys.Date(),
#'     lastNObservations = 1
#'   )
#' )
#'
#' # get most recent EXR for EUR/USD and SGD/USD - filtered by the package
#' EXR::get_exchange_rate_history(
#'   base_currency = c("USD", "SGD"),
#'   price_currency = "EUR",
#'   fill_missing_dates = TRUE,
#'   filter = list(endPeriod = Sys.Date(), startPeriod = Sys.Date())
#' )
#'
#' # trying to get an EXR for a holiday
#' EXR::get_exchange_rate_history(
#'   "EUR",
#'   "USD",
#'   filter = list(
#'     startPeriod = as.Date("2022-12-31"),
#'     endPeriod = as.Date("2022-12-31")
#'   )
#' )
#'
get_exchange_rate_history <- function(base_currency, price_currency, periodicity = "D", context = "A",
                                      fill_missing_dates = FALSE, max_lookback_days = 7, show_metadata = TRUE,
                                      filter = list(
                                        startPeriod = Sys.Date() - 370,
                                        endPeriod = Sys.Date() - 1,
                                        firstNObservations = NULL,
                                        lastNObservations = NULL
                                      )) {
  # ====================================
  # ======= Parameter checks ===========
  # ====================================

  # base_currency

  if (!(all(grepl("^[A-Z]{3}$", base_currency)) && is.character(base_currency) && length(base_currency) > 0)) {
    cli::cli_abort(c("All elements of 'base_currency' must be an ISO4217 three-letter code.",
      "x" = "Check {?this/these} code{?s}: {dplyr::coalesce(as.character(base_currency),\"NULL\")[!grepl(\"^[A-Z]{3}$\", dplyr::coalesce(as.character(base_currency),\"NULL\"))]}"
    ))
  }

  # price_currency
  if (!(all(grepl("^[A-Z]{3}$", price_currency)) && is.character(price_currency) && length(price_currency) > 0)) {
    cli::cli_abort(c("All elements of 'price_currency' must be an ISO4217 three-letter code.",
      "x" = "Check {?this/these} code{?s}: {dplyr::coalesce(as.character(price_currency),\"NULL\")[!grepl(\"^[A-Z]{3}$\", dplyr::coalesce(as.character(price_currency),\"NULL\"))]}"
    ))
  }

  # periodicity
  if (!is.scalar(periodicity) || !all(periodicity %in% c("A", "D", "H", "M", "Q"))) {
    cli::cli_abort(c(
      "x" = "You tried to set {.field periodicity} to '{periodicity}' but it must be any of {.strong A, D, H, M, Q}"
    ))
  }

  # context
  if (!is.scalar(context) || !(context %in% c("A", "E"))) {
    cli::cli_warn(c(
      "x" = "You tried to set {.field context} to '{context}' but it must be any of: {.strong A, E}",
      "i" = "{.field context} is set to 'average' ('{.strong A}')"
    ))
    context <- "A"
  }

  if (context == "E" && periodicity == "D") {
    cli::cli_inform(c(
      "i" = "You tried to set {.field context} to 'E' (end-of-period) but {.field periodicity} is 'D' (daily), therefore not a period",
      "i" = "{.field context} is set to 'average' ('A')"
    ))
    context <- "A"
  }

  # fill_missing_dates
  if (!is.logical(fill_missing_dates) || !is.scalar(fill_missing_dates) || is.na(fill_missing_dates)) {
    cli::cli_abort(c(
      "x" = "You tried to set {.field fill_missing_dates} to '{fill_missing_dates}' {.cls {class(fill_missing_dates)}}
              but it must be any of logical: {.strong TRUE, FALSE}"
    ))
  }

  if (is.null(max_lookback_days)) max_lookback_days <- 0

  # max_lookback_days
  # only check if `periodicity`=`D` and `fill_missing_dates`=`TRUE`
  if (periodicity == "D" && fill_missing_dates) {
    if (!is.numeric(max_lookback_days) || length(max_lookback_days) != 1 || max_lookback_days < 0 || round(max_lookback_days) != max_lookback_days) {
      cli::cli_abort(c(
        "x" = "You tried to set {.field max_lookback_days} to '{max_lookback_days}' {.cls {class(max_lookback_days)}}
              but it must be a numeric integer not smaller than 0"
      ))
    }
  }

  if (periodicity != "D" || !fill_missing_dates) max_lookback_days <- 0

  # show_metadata
  if (!is.logical(show_metadata) || length(show_metadata) != 1 || is.na(show_metadata)) {
    cli::cli_abort(c(
      "x" = "You tried to set {.field show_metadata} to '{show_metadata}' {.cls {class(show_metadata)}}
              but it must be any of logical: {.strong TRUE, FALSE}"
    ))
  }

  if (is.null(filter)) filter <- list()

  # filter
  if (!is.list(filter)) {
    cli::cli_abort(c("x" = "You tried to set {.field filter} to a value of class {.cls {class(filter)}} but only `<list>` is allowed."))
  } else {
    # unknown options
    if (length(names(filter) |> setdiff(c("startPeriod", "endPeriod", "firstNObservations", "lastNObservations"))) > 0) {
      unrecognised <- names(filter) |> setdiff(c("startPeriod", "endPeriod", "firstNObservations", "lastNObservations"))
      cli::cli_warn(c(
        "!" = "For {.field filter} list you tried to use the undefined option{?s} '{unrecognised}'",
        "i" = "'{unrecognised}' {?is/are} discarded."
      ))
    }

    # filter$startPeriod
    if (!is.null(filter$startPeriod) && (!inherits(filter$startPeriod, what = "Date") || !is.scalar(filter$startPeriod))) {
      cli::cli_abort(c("!" = "You tried to set {.field filter$startPeriod} to {filter$startPeriod} but only dates are allowed (try to use as.Date)"))
    }

    # filter$endPeriod
    if (!is.null(filter$endPeriod) && (!inherits(filter$endPeriod, what = "Date") || !is.scalar(filter$endPeriod))) {
      cli::cli_abort(c("!" = "You tried to set {.field filter$endPeriod} to {filter$endPeriod} but only dates are allowed (try to use as.Date)"))
    }

    if (!is.null(filter$startPeriod) && !is.null(filter$endPeriod) && filter$startPeriod > filter$endPeriod) {
      cli::cli_abort(c("!" = "{.field filter$startPeriod} ({filter$startPeriod}) must not be after {.field filter$endPeriod} ({filter$endPeriod})"))
    }

    if (!is.null(filter$startPeriod) && filter$startPeriod > Sys.Date()) {
      cli::cli_abort(c("!" = "{.field filter$startPeriod} ({filter$startPeriod}) must not be in the future!"))
    }

    # filter$firstNObservations
    if (!is.null(filter$firstNObservations)) {
      if (fill_missing_dates) {
        cli::cli_abort(c(
          "x" = "You tried to set {.field filter$firstNObservations} to {filter$firstNObservations} but {.field fill_missing_dates} is TRUE",
          "i" = "These parameters must not be used together because {.field filter$firstNObservations} invokes an API-sided filtering/limitation but
          {.field fill_missing_dates} performs a package-sided fill-in of missing dates after the API-request"
        ))
      } else if (!is.scalar(filter$firstNObservations) || is.na(filter$firstNObservations) || !is.scalar(filter$firstNObservations)) {
        cli::cli_abort(c(
          "x" = "You tried to set {.field filter$firstNObservations} to '{filter$firstNObservations}' {.cls {class(filter$firstNObservations)}}
              but it must be a numeric integer not smaller than 1"
        ))
      } else if (!is.null(filter$firstNObservations) && (!is.numeric(filter$firstNObservations) || filter$firstNObservations < 1 || ceiling(filter$firstNObservations) != filter$firstNObservations)) {
        cli::cli_abort(c(
          "x" = "You tried to set {.field filter$firstNObservations} to '{filter$firstNObservations}' {.cls {class(filter$firstNObservations)}}
              but it must be a numeric integer not smaller than 1"
        ))
      }
    }

    # filter$lastNObservations
    if (!is.null(filter$lastNObservations)) {
      if (fill_missing_dates) {
        cli::cli_abort(c(
          "x" = "You tried to set {.field filter$lastNObservations} to {filter$lastNObservations} but {.field fill_missing_dates} is TRUE",
          "i" = "These parameters must not be used together because {.field filter$lastNObservations} invokes an API-sided filtering/limitation but
          {.field fill_missing_dates} performs a package-sided fill-in of missing dates after the API-request"
        ))
      } else if (!is.scalar(filter$lastNObservations) || is.na(filter$lastNObservations) || !is.scalar(filter$lastNObservations)) {
        cli::cli_abort(c(
          "x" = "You tried to set {.field filter$lastNObservations} to '{filter$lastNObservations}' {.cls {class(filter$lastNObservations)}}
              but it must be a numeric integer not smaller than 1"
        ))
      } else if (!is.null(filter$lastNObservations) && (!is.numeric(filter$lastNObservations) || filter$lastNObservations < 1 || ceiling(filter$lastNObservations) != filter$lastNObservations)) {
        cli::cli_abort(c(
          "x" = "You tried to set {.field filter$lastNObservations} to '{filter$lastNObservations}' {.cls {class(filter$lastNObservations)}}
              but it must be a numeric integer not smaller than 1"
        ))
      }
    }

    # param combinations
    if (!(is.null(filter$firstNObservations) || is.null(filter$lastNObservations))) {
      cli::cli_abort(c("!" = "You must not use both {.field filter$firstNObservations} and {.field filter$lastNObservations}."))
    }
  }

  # ====================================
  # ===== build request string =========
  # ====================================

  # ECB only supports EUR as base currency, so we have to request *all* required currencies as price currency
  req_string_currencies <- union(base_currency, price_currency) |>
    setdiff("EUR") |>
    paste(collapse = "+")
  # for the rare case of "only EUR vs EUR" currency, we need a workaround to get the periods
  # because ECB doesn't support EUR to EUR conversion (which is 1 obviously)
  if (all(base_currency == "EUR") && all(price_currency == "EUR")) req_string_currencies <- "USD"

  # request string with the key
  req_string <- paste(periodicity, req_string_currencies, "EUR", "SP00", context, sep = ".")

  # request string with key + params
  req_string_params <- c(
    # get CSV instead of json or xml (standard)
    "format=csvdata",
    # remove informative columns
    "detail=dataonly",
    # don't sent firstNObservations if it is NULL
    ifelse(is.null(filter$firstNObservations), NA, paste0("firstNObservations=", filter$firstNObservations)),
    # don't sent lastNObservations if it is NULL
    ifelse(is.null(filter$lastNObservations), NA, paste0("lastNObservations=", filter$lastNObservations)),
    # if periodicity == "D" and max_lookback_days is a positive value, we might want to fetch
    # more data than we acutally want to display! If periodicity != "D" just use startPeriod as is
    ifelse(is.null(filter$startPeriod), NA,
      paste0(
        "startPeriod=",
        ifelse(periodicity == "D",
          # transform date into sdmx format
          EXR::sdmx_date_to_character(filter$startPeriod - dplyr::coalesce(max_lookback_days, 0), periodicity),
          EXR::sdmx_date_to_character(filter$startPeriod, periodicity)
        )
      )
    ),
    ifelse(is.null(filter$endPeriod), NA,
      # transform date into sdmx format
      paste0("endPeriod=", EXR::sdmx_date_to_character(filter$endPeriod, periodicity))
    )
  )

  req_string <- paste0(
    req_string,
    "?",
    # remove NA values in parameters
    paste(req_string_params[!is.na(req_string_params)],
      collapse = "&"
    )
  )

  result <- EXR::perform_ecb_api_request(req_string)

  # ====================================
  # ===== format and calculate =========
  # ====================================

  # (i) uppercase columns are raw columns (sent by API), lowercase columns are internal columns

  # init empty return tibble
  return_tibble <- readr::read_csv(
    file = "a,a,a,1,a,a,T\n",
    col_names = c("base_currency", "price_currency", "period", "value", "periodicity", "context", "raw"),
    col_types = "cccnccl"
  )[0, ]

  if (!is.null(result)) {
    result <- result |>
      readr::read_csv(show_col_types = FALSE, )

    assert_is_df(result)

    result <- result |>
      dplyr::mutate(EXR_TYPE = NULL, raw = TRUE) |>
      dplyr::filter(CURRENCY_DENOM == "EUR")

    # if fill_missing_dates is TRUE for daily frequency, fill gaps in data! Additionally apply max_lookback_days
    if (fill_missing_dates && periodicity == "D") {
      result <- fill_gaps(result, max_lookback_days, filter$startPeriod, filter$endPeriod)
    }

    # EBC only supports EUR as denominator, so there are 4 possible variations now:
    # 1.) currency pair already exists and can be used as-is (raw)
    # 2.) currency pair already exists, but in wrong direction - needs to be inversed
    # 3.) currency pair (base and price are different) needs to be calculated by a cross-rate via EUR
    # 4.) base and price currency are the same, so exchange rate is 1

    required_combinations <- expand.grid(
      "required_base_currency" = base_currency,
      "required_price_currency" = price_currency, stringsAsFactors = FALSE
    ) %>% unique()

    # --> 1.) currency pair already exists and can be used as-is (raw)

    return_tibble <- rbind(
      return_tibble,
      result %>%
        dplyr::inner_join(required_combinations,
          by = c("CURRENCY_DENOM" = "required_base_currency", "CURRENCY" = "required_price_currency"), keep = TRUE
        ) %>%
        dplyr::rename(
          base_currency = required_base_currency,
          price_currency = required_price_currency,
          period = TIME_PERIOD,
          value = OBS_VALUE,
          periodicity = FREQ, context = EXR_SUFFIX
        ) %>%
        dplyr::select(base_currency, price_currency, period, value, periodicity, context, raw)
    )

    # --> 2.) currency pair already exists, but in wrong direction - needs to be inversed

    return_tibble <- rbind(
      return_tibble,
      result %>%
        dplyr::inner_join(required_combinations,
          by = c("CURRENCY_DENOM" = "required_price_currency", "CURRENCY" = "required_base_currency"), keep = TRUE
        ) %>%
        dplyr::rename(
          base_currency = required_base_currency,
          price_currency = required_price_currency,
          period = TIME_PERIOD,
          value = OBS_VALUE,
          periodicity = FREQ, context = EXR_SUFFIX
        ) %>%
        dplyr::mutate(value = 1 / value, raw = FALSE) %>%
        dplyr::select(base_currency, price_currency, period, value, periodicity, context, raw)
    )


    # --> 3.) currency pair (base and price are different) needs to be calculated by a cross-rate via EUR

    return_tibble <- rbind(
      return_tibble,
      dplyr::inner_join(
        # denominator for x-rate
        result %>%
          dplyr::inner_join(
            required_combinations %>%
              dplyr::filter(required_base_currency != required_price_currency & required_base_currency != "EUR" & required_price_currency != "EUR"),
            by = c("CURRENCY" = "required_base_currency"), keep = TRUE, relationship = "many-to-many"
          ),
        # numerator for x-rate
        result %>%
          dplyr::inner_join(
            required_combinations %>%
              dplyr::filter(
                required_base_currency != required_price_currency &
                  required_base_currency != "EUR" &
                  required_price_currency != "EUR"
              ),
            by = c("CURRENCY" = "required_price_currency"), keep = TRUE, relationship = "many-to-many"
          ),
        by = dplyr::join_by(required_base_currency, required_price_currency, TIME_PERIOD, EXR_SUFFIX, FREQ),
        suffix = c(".denominator", ".numerator")
      ) %>%
        dplyr::mutate(value = OBS_VALUE.numerator / OBS_VALUE.denominator, raw = FALSE) %>%
        dplyr::rename(
          base_currency = required_base_currency,
          price_currency = required_price_currency,
          context = EXR_SUFFIX,
          period = TIME_PERIOD,
          periodicity = FREQ
        ) %>%
        dplyr::select(base_currency, price_currency, period, value, periodicity, context, raw)
    )

    # --> 4.) base and price currency are the same, so exchange rate is 1

    return_tibble <- rbind(
      return_tibble,
      result %>%
        dplyr::rename(period = TIME_PERIOD, periodicity = FREQ, context = EXR_SUFFIX) %>%
        dplyr::select(period, periodicity, context) %>%
        unique() %>%
        dplyr::cross_join(
          required_combinations %>%
            dplyr::filter(required_base_currency == required_price_currency)
        ) %>%
        dplyr::rename(base_currency = required_base_currency, price_currency = required_price_currency) %>%
        dplyr::mutate(value = 1, raw = FALSE) %>%
        dplyr::select(base_currency, price_currency, period, value, periodicity, context, raw)
    )
  }

  # delete metadata if user doesn't want it
  if (!show_metadata) return_tibble <- return_tibble |> dplyr::mutate(periodicity = NULL, context = NULL, raw = NULL)

  # if daily data, delete rows out of date bounds
  if (periodicity == "D" && nrow(return_tibble) > 0) {
    return_tibble <- return_tibble %>%
      dplyr::filter(period >= dplyr::coalesce(filter$startPeriod, period) &
        period <= dplyr::coalesce(filter$endPeriod, period)) # nolint
  }

  # finally check for existence of each currency... if one is missing, give a warning
  missing_currencies <- base_currency |> setdiff(return_tibble$base_currency)
  missing_currencies <- missing_currencies |> union(price_currency |> setdiff(return_tibble$price_currency))

  if (length(missing_currencies) > 0) {
    available_currencies <- EXR::get_available_currencies(date = dplyr::coalesce(filter$endPeriod, Sys.Date() - 1))$ISOCODE
    available_currencies_formatted <- paste(available_currencies, collapse = ", ")

    if (length(missing_currencies |> setdiff(available_currencies)) == 0 && nrow(return_tibble) == 0) {
      cli::cli_inform(c("i" = "Result is empty. Please check your date configuration. You may want to use a higher value for {.field max_lookback_days}.
                               Now it is '{max_lookback_days}'"))
    } else {
      cli::cli_warn(c(
        "x" = "Currenc{?y/ies} '{.strong {missing_currencies}}' {?is/are} missing the the result.",
        "i" = "Check {.fn EXR::get_available_currencies}, because only these curriencies are available as of
            {dplyr::coalesce(filter$endPeriod, Sys.Date() - 1)}: '{.strong {available_currencies_formatted}}'"
      ))
    }
  }

  return_tibble
}

#' fills gaps in daily data
#'
#' @param data tibble with columns `KEY, FREQ, CURRENCY, CURRENCY_DENOM, EXR_SUFFIX, TIME_PERIOD, OBS_VALUE, raw`
#'             uppercase columns are raw columns (sent by API), lowercase columns are internal columns
#' @param max_lookback_days number of max dates to go back for filling values
#' @param start_date first day, might be NULL
#' @param end_date last date, might be NULL
#'
#' @returns same tibble structure
#' @keywords internal
#' @noRd
#'
#' @examples
#' library(dplyr)
#' fill_gaps(dplyr::tribble(
#'   ~KEY, ~FREQ, ~CURRENCY, ~CURRENCY_DENOM, ~EXR_SUFFIX, ~TIME_PERIOD, ~OBS_VALUE, ~raw,
#'   "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-01", 1L, TRUE,
#'   "DUSDEURX", "D", "USD", "EUR", "X", "2024-01-03", 3L, TRUE
#' ), 1)
fill_gaps <- function(data, max_lookback_days, start_date = NULL, end_date = NULL) {
  # just to be sure
  if (!is.null(start_date)) start_date <- as.Date(start_date)
  if (!is.null(end_date)) end_date <- as.Date(end_date)

  data %>%
    dplyr::mutate(TIME_PERIOD = as.Date(TIME_PERIOD)) %>%
    dplyr::group_by(KEY, FREQ, CURRENCY, CURRENCY_DENOM, EXR_SUFFIX) %>%
    # if start_date or end_date is NULL then just use the first or last existing date
    tidyr::complete(TIME_PERIOD = seq.Date(as.Date(dplyr::coalesce(start_date, min(TIME_PERIOD))),
      as.Date(dplyr::coalesce(end_date, max(TIME_PERIOD))),
      by = "day"
    )) %>%
    dplyr::arrange(TIME_PERIOD, .by_group = TRUE) %>%
    # only apply vec_fill_missing if max_lookback_days has a positive value
    {
      if (dplyr::coalesce(max_lookback_days, 0) >= 1) {
        dplyr::mutate(., OBS_VALUE = vctrs::vec_fill_missing(OBS_VALUE, direction = "down", max_fill = max_lookback_days))
      } else {
        .
      }
    } %>%
    dplyr::ungroup() %>%
    dplyr::filter(TIME_PERIOD >= dplyr::coalesce(start_date, TIME_PERIOD) & TIME_PERIOD <= dplyr::coalesce(end_date, TIME_PERIOD)) %>%
    dplyr::mutate(raw = tidyr::replace_na(raw, FALSE)) %>%
    dplyr::select(KEY, FREQ, CURRENCY, CURRENCY_DENOM, EXR_SUFFIX, TIME_PERIOD, OBS_VALUE, raw)
}
