#' Get all trading days
#'
#' @param start_date Start date by "yyyy-mm-dd" format
#' @param end_date End date by "yyyy-mm-dd" format
#'
#' @return Date vector
#' @export
get_trading_days <- function(start_date, end_date) {

  first_date <- lubridate::ymd("1996-01-02")

  if (lubridate::ymd(start_date) < first_date)
    stop("Start date must be greater than or equal to 1996-01-02")

  if (lubridate::ymd(end_date) < lubridate::ymd(start_date))
    stop("End date must be greater than or equal to start date.")

  ibm <- MarketData::get_iqfeed_daily("IBM", start_date, end_date)
  spy <- MarketData::get_iqfeed_daily("SPY", start_date, end_date)

  sort(unique(c(ibm$date, spy$date)))
}

#' Get previous trading date of specified date
#'
#' @param date Date
#'
#' @return date
#' @export
get_previous_trading_day <- function(date) {

  # Load 5 days
  end_date <- lubridate::ymd(date)
  start_date <- end_date - 5

  # Return one day head date
  tday <- get_trading_days(start_date, end_date)

  #tday[match(end_date, tday) - 1]
  utils::tail(tday[tday < end_date], 1)
}

#' Get trading range
#'
#' @param start_date Start date by "yyyy-mm-dd" format
#' @param end_date End date by "yyyy-mm-dd" format
#' @param unit Floor date unit like "day", "week" or "month"
#' @param lookback Lookback period by unit size
#'
#' @return data.frame of start and end date
#' @export
get_trading_range <- function(start_date, end_date, unit, lookback = Inf) {

  all_tdays <- get_trading_days("1996-01-02", end_date)

  # Floor by unit
  unit <- unit
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
#' @param type Range type like "ExpandingDaily", "Rolling3mDaily" etc.
#' @param start_date Start date by "yyyy-mm-dd" format
#' @param end_date End date by "yyyy-mm-dd" format
#' @param expand_start_period Minimum period for expading type
#'
#' @return data.frame of start and end date
#' @export
get_ranges_by_type <- function(type, start_date, end_date, expand_start_period = 10) {
  tdays_in_month <- 21

  switch(type,
    "ExpandingDaily"  = get_trading_range(start_date, end_date, "day", Inf) %>%
                          dplyr::slice(expand_start_period:dplyr::n()),
    "ExpandingWeekly" = get_trading_range(start_date, end_date, "week", Inf) %>%
                          dplyr::slice(expand_start_period:dplyr::n()),
    "Rolling1mDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month),
    "Rolling3mDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 3),
    "Rolling6mDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 6),
    "Rolling1yDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 12),
    "Rolling2yDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 24),
    "Rolling3yDaily"  = get_trading_range(start_date, end_date, "day", tdays_in_month * 36),
    "Rolling3mWeekly" = get_trading_range(start_date, end_date, "week", 13),
    "Rolling6mWeekly" = get_trading_range(start_date, end_date, "week", 26),
    "Rolling1yWeekly" = get_trading_range(start_date, end_date, "week", 52))
}
