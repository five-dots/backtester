#' Calculate mininum performance metrics from equity curve.
#'
#' @param ec Equity curve
#'
#' @return data.frame
#' @export
calc_min_performance <- function(ec) {

  # Omit first row
  ec <- ec[-1, ]

  # Win/Lose
  win_trades  <- ec[ec$pnl > 0, ]$pnl
  lose_trades <- ec[ec$pnl <= 0, ]$pnl

  # Total performance
  ttl_win   <- round(sum(win_trades), 2)
  ttl_lose  <- round(sum(lose_trades), 2)
  drawdown  <- round(min(ec$drawdown), 2)
  win_days  <- length(win_trades)
  lose_days <- length(lose_trades)

  data.frame(
    ttl_win   = ttl_win,
    ttl_lose  = ttl_lose,
    drawdown  = drawdown,
    win_days  = win_days,
    lose_days = lose_days
  )
}

#' Calculate performance metrics from equity curve.
#'
#' @param ec Equity curve
#'
#' @return data.frame
#' @export
calc_performance <- function(ec) {

  p <- calc_min_performance(ec)

  net_pnl   <- p$ttl_win + p$ttl_lose
  days      <- p$win_days + p$lose_days
  win_ratio <- p$win_days / days
  avg_win   <- p$ttl_win / p$win_days
  avg_lose  <- p$ttl_lose / p$lose_days
  pf        <- p$ttl_win / abs(p$ttl_lose)

  data.frame(
    net_pnl   = net_pnl,
    drawdown  = p$drawdown,
    days      = days,
    win_ratio = win_ratio,
    avg_win   = avg_win,
    avg_lose  = avg_lose,
    pf        = pf
  )
}

#' Calculate minimum performance based on portfolio return
#'
#' @param ec Equity curve
#' @param clean Whether clean return by clean.boudt
#'
#' @importFrom PerformanceAnalytics StdDev
#'
#' @return data.frame
#' @export
calc_min_ret_performance <- function(ec, clean = FALSE) {

  # xts return
  ra <- timetk::tk_xts(ec, select = ret, date_var = date)
  rb <- timetk::tk_xts(ec, select = ret_b, date_var = date)

  if (clean) {
    ra <- PerformanceAnalytics::Return.clean(ra, method = "boudt")
    rb <- PerformanceAnalytics::Return.clean(rb, method = "boudt")
  }

  # APR
  apr <- PerformanceAnalytics::Return.annualized(ra)[1, ]

  # Risk/Reward metrics
  sharpe  <- PerformanceAnalytics::SharpeRatio(ra, FUN = "StdDev", annualize = TRUE)[1, ]
  info    <- PerformanceAnalytics::InformationRatio(ra, rb)
  upsidep <- PerformanceAnalytics::UpsidePotentialRatio(ra)[1, ]

  data.frame(
    apr     = apr,
    sharpe  = sharpe,
    info    = info,
    upsidep = upsidep
  )
}

#' Calculate performance based on portfolio return
#'
#' @param ec Equity curve
#' @param clean Whether clean return by clean.boudt
#'
#' @importFrom PerformanceAnalytics StdDev
#'
#' @return data.frame
#' @export
calc_ret_performance <- function(ec, clean = FALSE) {

  # xts return
  ra <- timetk::tk_xts(ec, select = ret, date_var = date)
  rb <- timetk::tk_xts(ec, select = ret_b, date_var = date)

  if (clean) {
    ra <- PerformanceAnalytics::Return.clean(ra, method = "boudt")
    rb <- PerformanceAnalytics::Return.clean(rb, method = "boudt")
  }

  # Risk/Reward metrics
  sortino  <- PerformanceAnalytics::SortinoRatio(ra)[1, ]
  calmar   <- PerformanceAnalytics::CalmarRatio(ra)[1, ]
  sterling <- PerformanceAnalytics::SterlingRatio(ra)[1, ]
  burke    <- PerformanceAnalytics::BurkeRatio(ra)

  # Kelly ratio (Half)
  half_kelly <- PerformanceAnalytics::KellyRatio(ra, method = "half")[1,]

  # Risk metrics
  sd     <- as.numeric(PerformanceAnalytics::StdDev(ra))
  semi_d <- PerformanceAnalytics::SemiDeviation(ra)[1, ]
  var95  <- PerformanceAnalytics::VaR(ra, p = 0.95, method = "modified")[1, ]
  var99  <- PerformanceAnalytics::VaR(ra, p = 0.99, method = "modified")[1, ]
  es95   <- PerformanceAnalytics::ETL(ra, p = 0.95, method = "modified")[1, ]
  es99   <- PerformanceAnalytics::ETL(ra, p = 0.99, method = "modified")[1, ]

  # Skewness and Kurtosis
  skewness <- PerformanceAnalytics::skewness(ra, method = "moment")
  kurtosis <- PerformanceAnalytics::kurtosis(ra, method = "moment")

  # Combine minimum return peformance
  min_perf <- calc_min_ret_performance(ec, clean)
  cbind(min_perf, data.frame(
    sortino    = sortino,
    calmar     = calmar,
    sterling   = sterling,
    burke      = burke,
    half_kelly = half_kelly,
    sd         = sd,
    semi_d     = semi_d,
    var95      = var95,
    var99      = var99,
    es95       = es95,
    es99       = es99,
    skewness   = skewness,
    kurtosis   = kurtosis
  ))
}

#' Format performance for printing purpose
#'
#' @param ec Equity curve
#' @param col_name Column name
#' @param clean Whether to clean returns by clean.boudt
#'
#' @return data.frame
#' @export
print_performance <- function(ec, col_name, clean = FALSE) {

  perf <- cbind(calc_performance(ec),
                calc_ret_performance(ec, clean))

  columns <- c("net_pnl", "drawdown", "days", "win_ratio",
               "avg_win", "avg_lose", "pf", "apr",
               "sharpe", "info", "upsidep", "sortino",
               "calmar", "sterling", "burke", "half_kelly",
               "sd", "semi_d", "var95", "var99", "es95", "es99",
               "skewness", "kurtosis")

  perf <- perf[, columns]

  perf$net_pnl    <- as.character(formattable::accounting(perf$net_pnl))
  perf$drawdown   <- as.character(formattable::accounting(perf$drawdown))
  perf$days       <- as.character(perf$days)
  perf$win_ratio  <- as.character(formattable::percent(perf$win_ratio))
  perf$avg_win    <- as.character(formattable::accounting(perf$avg_win))
  perf$avg_lose   <- as.character(formattable::accounting(perf$avg_lose))
  perf$pf         <- as.character(round(perf$pf, 2))
  perf$apr        <- as.character(formattable::percent(perf$apr))
  perf$sharpe     <- as.character(round(perf$sharpe, 2))
  perf$info       <- as.character(round(perf$info, 2))
  perf$sortino    <- as.character(round(perf$sortino, 2))
  perf$upsidep    <- as.character(round(perf$upsidep, 2))
  perf$calmar     <- as.character(round(perf$calmar, 2))
  perf$sterling   <- as.character(round(perf$sterling, 2))
  perf$burke      <- as.character(round(perf$burke, 2))
  perf$half_kelly <- as.character(round(perf$half_kelly, 2))
  perf$sd         <- as.character(formattable::percent(perf$sd))
  perf$semi_d     <- as.character(formattable::percent(perf$semi_d))
  perf$var95      <- as.character(formattable::percent(perf$var95))
  perf$var99      <- as.character(formattable::percent(perf$var99))
  perf$es95       <- as.character(formattable::percent(perf$es95))
  perf$es99       <- as.character(formattable::percent(perf$es99))
  perf$skewness   <- as.character(round(perf$skewness, 2))
  perf$kurtosis   <- as.character(round(perf$kurtosis, 2))

  t_perf <- data.frame(t(perf))
  rownames(t_perf) <- NULL
  colnames(t_perf) <- col_name

  t_perf$Item <- c("Net PnL", "Max Drawdown", "Days", "Win Ratio",
                   "Average Win", "Average Lose",
                   "Profit Factor", "Annual Return",
                   "Sharpe Ratio", "Information Ratio",
                   "Upside Potential Ratio", "Sortino Ratio",
                   "Calmar Ratio", "Sterling Ratio", "Burke Ratio",
                   "Half Kelly",
                   "Standard Deviation", "Semi Deviation",
                   "VaR (95%)", "VaR (99%)", "ES (95%)", "ES (99%)",
                   "Skewness", "Kurtosis")

  t_perf[, c("Item", col_name)]
}

#' Parallely calculate performance from equity curve
#'
#' @param ec Equity curve
#' @param parallel Whether to calc parallely
#'
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @return data.frame
#' @export
calc_performances <- function(ec, parallel = TRUE) {

  if (parallel) {
    # Prepare parallel
    pkgs <- devtools::loaded_packages()$package
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))

    foreach::foreach(result = ec, .combine = "rbind", .packages = pkgs) %dopar% {
      cbind(result$param,
            calc_performance(result$ec),
            calc_min_ret_performance(result$ec))
    }

  } else {
    foreach::foreach(result = ec, .combine = "rbind") %do% {
      cbind(result$param,
            calc_performance(result$ec),
            calc_min_ret_performance(result$ec))
    }
  }
}
