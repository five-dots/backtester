
add_drawdown <- function(ec) {
  ec$peak <- 0
  for (i in 1:nrow(ec)) {
    ec[i, "peak"] <- max(ec[1:i, "cum_pnl"])
  }
  ec$drawdown <- ec$cum_pnl - ec$peak
  ec$peak <- NULL
  ec
}

add_benchmark_return <- function(ec, benchmark = "SPY") {
  # Load one day ahead
  start_date <- get_previous_trading_day(min(ec$date))
  end_date <- max(ec$date)

  # Bechmark data
  base <- MarketData::get_iqfeed_daily(benchmark, start_date, end_date) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(ret_b = (adj_close - dplyr::lag(adj_close)) / dplyr::lag(adj_close)) %>%
    dplyr::select(date, ret_b)

  # Add cum_ret
  base[is.na(base)] <- 0
  base$cum_ret_b <- cumsum(base$ret_b)

  # Merge
  ec <- dplyr::right_join(ec, base, by = "date")
  ec[is.na(ec)] <- 0
  ec
}

#' Generate equity curve from transactions.
#'
#' @param trans Transactions which contains date and pnl.
#' @param benchmark Benchmark symbol.
#'
#' @return data.frame
#' @export
generate_equity_curve <- function(trans, benchmark = "SPY") {
  ec <- trans %>%
    dplyr::filter(!is.na(pnl)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(trades = n(),
                     cost = sum(cost, na.rm = TRUE),
                     pnl = sum(pnl, na.rm = TRUE)) %>%
    dplyr::mutate(ret = pnl / cost) %>%
    # Add benchmark before calculating cumulative values
    add_benchmark_return(benchmark) %>%
    dplyr::mutate(cum_pnl = cumsum(pnl),
                  cum_ret = cumsum(ret)) %>%
    # Add drawdown after calculating cum pnl
    add_drawdown() %>%
    dplyr::select(date, trades, pnl, cum_pnl, drawdown,
                  ret, ret_b, cum_ret, cum_ret_b)
}
