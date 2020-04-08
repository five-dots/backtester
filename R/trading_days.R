#' Get all trading days by IBM
#'
#' @param start_date Date or character of start date by "yyyy-mm-dd" format
#' @param end_date Date or character of end date by "yyyy-mm-dd" format
#'
#' @return Date vector
#' @export
get_trading_days <- function(start_date = "1962-01-02", end_date = Sys.Date()) {
  ## Check if the args are coercible to Date
  start_date <- lubridate::ymd(start_date)
  end_date   <- lubridate::ymd(end_date)

  ## Check if the start date does not exceed the IBM's first date
  first_date <- as.Date("1962-01-02")
  if (start_date < first_date)
    stop("Start date must be greater than or equal to 1962-01-02.")

  if (end_date < start_date)
    stop("End date must be greater than or equal to start date.")

  ## SPY starts from 1993-01-29
  ## spy <- tidyquant::tq_get("SPY", get = "stock.prices", from = start_date)
  ## IBM starts from 1962-01-02
  ibm <- tidyquant::tq_get("IBM", get = "stock.prices", from = start_date)

  ## tdays <- sort(unique(c(spy$date, ibm$date)))
  tdays <- sort(ibm$date)
  tdays[tdays <= end_date]
}

#' Get previous trading date of specified date
#'
#' @inheritParams get_trading_days
#'
#' @return date
#' @export
get_previous_trading_day <- function(end_date) {
  stopifnot(length(end_date) == 1)

  # Load 5 days
  end_date <- lubridate::ymd(end_date)
  start_date <- end_date - 5

  # Return one day head date
  tday <- get_trading_days(start_date, end_date)

  utils::tail(tday[tday < end_date], 1)
}

#' Get trading range
#'
#' @inheritParams get_trading_days
#' @param unit Floor date unit by "day", "week", "month" or "year"
#' @param lookback Lookback period by unit size
#'
#' @return data.frame of start and end date
#' @export
get_trading_range <- function(start_date, end_date, unit, lookback = Inf) {
  stopifnot(unit %in% c("day", "week", "month", "year"),
            length(unit) == 1,
            (is.infinite(lookback) | is.numeric(lookback)),
            length(lookback) == 1)

  all_tdays <- get_trading_days("1993-01-29", end_date)

  # Floor by unit
  floor_by_unit <- data.frame(
    tdays = all_tdays,
    floored_date = lubridate::floor_date(all_tdays, unit)
  )

  # data.frame of start_date and end_date by unit
  dates <- floor_by_unit %>%
    dplyr::group_by(floored_date) %>%
    dplyr::summarise(
      first_date = dplyr::first(tdays),
      last_date = dplyr::last(tdays)) %>%
    dplyr::select(-floored_date)

  # select result
  if (lookback == Inf) {
    dates %>%
      dplyr::filter(first_date >= start_date) %>%
      dplyr::mutate(first_date = dplyr::first(first_date))

  } else {
    dates %>%
      dplyr::mutate(first_date = dplyr::lag(first_date, lookback - 1)) %>%
      dplyr::filter(first_date >= start_date)
  }
}

#' Get trading range by type
#'
#' @inheritParams get_trading_days
#' @param type Range type like "ExpandingDaily", "Rolling3mDaily" etc.
#' @param expand_min_period Minimum period for expading type
#'
#' @return data.frame of start and end date
#' @export
get_ranges_by_type <- function(type, start_date, end_date, expand_min_period = 10) {
  stopifnot(is.character(type), length(type) == 1,
            is.numeric(expand_min_period), expand_min_period > 0)

  tdays_in_month <- 21
  switch(type,
    "ExpandingDaily"  = get_trading_range(start_date, end_date, "day", Inf) %>%
                          dplyr::slice(expand_min_period:dplyr::n()),
    "ExpandingWeekly" = get_trading_range(start_date, end_date, "week", Inf) %>%
                          dplyr::slice(expand_min_period:dplyr::n()),
    "Rolling1mDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month),
    "Rolling3mDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 3),
    "Rolling6mDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 6),
    "Rolling1yDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 12),
    "Rolling2yDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 24),
    "Rolling3yDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 36),
    "Rolling3mWeekly" = get_trading_range(start_date, end_date, "week", 13),
    "Rolling6mWeekly" = get_trading_range(start_date, end_date, "week", 26),
    "Rolling1yWeekly" = get_trading_range(start_date, end_date, "week", 52),
    stop(glue::glue("{type} is not supported.")))
}
