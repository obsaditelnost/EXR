
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EXR

<!-- badges: start -->

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/obsaditelnost/EXR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/obsaditelnost/EXR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/obsaditelnost/EXR/graph/badge.svg)](https://app.codecov.io/gh/obsaditelnost/EXR)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
<!-- badges: end -->

Get currency exchange rates (‘EXR’) or directly apply currency
conversion on a tibble from one to another currency based on spot prices
(avg) provided by ‘ECB Data Portal’. If neither currency nor currency
denominator is EUR, unofficial cross-rates based on EUR will be
calculated. The main purpose of this package is to offer an unlimited
and free way to get (potentially approximate) important exchange rates.
No API-key required. Data can be requested by periods with days as
shortest period. No ask, bid, mid or intra-day data.

## Installation

You can install the development version of EXR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("obsaditelnost/EXR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(EXR)
# get firsr 10 rows of USD/EUR exchange rates for the last year,
# fill missing dates and look for missing values up to 3 days in the past
# so there won't be exchange rates on very long weekends that include holidays
EXR::get_exchange_rate_history(
 base_currency = "EUR",
 price_currency = "USD",
 periodicity = "D",
 fill_missing_dates = TRUE,
 max_lookback_days = 3
)[1:10, ]
#> # A tibble: 10 × 7
#>    base_currency price_currency period     value periodicity context raw  
#>    <chr>         <chr>          <date>     <dbl> <chr>       <chr>   <lgl>
#>  1 EUR           USD            2023-08-28  1.08 D           A       TRUE 
#>  2 EUR           USD            2023-08-29  1.08 D           A       TRUE 
#>  3 EUR           USD            2023-08-30  1.09 D           A       TRUE 
#>  4 EUR           USD            2023-08-31  1.09 D           A       TRUE 
#>  5 EUR           USD            2023-09-01  1.08 D           A       TRUE 
#>  6 EUR           USD            2023-09-02  1.08 D           A       FALSE
#>  7 EUR           USD            2023-09-03  1.08 D           A       FALSE
#>  8 EUR           USD            2023-09-04  1.08 D           A       TRUE 
#>  9 EUR           USD            2023-09-05  1.07 D           A       TRUE 
#> 10 EUR           USD            2023-09-06  1.07 D           A       TRUE
```
