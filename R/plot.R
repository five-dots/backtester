#' Build equity curve and drawdown plot.
#'
#' @param ec Equity curve
#'
#' @return ggplot
#' @export
plot_equity_curve <- function(ec) {

  # Equity curve subtitle
  perf <- cbind(calc_performance(ec), calc_ret_performance(ec))

  subtitle <- paste0(
    "Return = ", formattable::accounting(perf$net_pnl, digits = 0),
    ", Drawdown = ", formattable::accounting(perf$drawdown, digits = 0),
    ", Sharpe ratio = ", round(perf$sharpe, digits = 2),
    ", PF = ", round(perf$pf, digits = 2),
    ", Win ratio = ", formattable::percent(perf$win_ratio)
  )

  ec %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = cum_pnl)) +
    ggplot2::ggtitle(label = "Equity curve", subtitle = subtitle) +
    ggplot2::geom_hline(yintercept = 0) +
    # Equity curve
    ggplot2::geom_line(ggplot2::aes(y = cum_pnl), color = "darkgreen") +
    #ggplot2::geom_area(ggplot2::aes(y = cum_pnl), fill = "darkgreen", alpha = 0.3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymax = cum_pnl, ymin = 0),
                         fill = "darkgreen", alpha = 0.3) +
    # Drawdown
    ggplot2::geom_line(ggplot2::aes(y = drawdown), color = "red") +
    ggplot2::geom_area(ggplot2::aes(y = drawdown), fill = "red", alpha = 0.3) +
    rutils::gg_theme()
}

#' Build boxplot
#'
#' @param df data.frame
#' @param x X-axis parameter
#' @param y Y-axis parameter
#'
#' @return ggplot
#' @export
boxplot_performance <- function(df, x, y) {

  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)

  # Build title
  x_name <- dplyr::quo_name(x)
  y_name <- dplyr::quo_name(y)
  title <- glue::glue("Parameter ({x_name}) vs. Performance ({y_name})")

  df %>%
    # Convert x to factor
    dplyr::mutate_at(x_name, dplyr::funs(as.factor)) %>%
    ggplot2::ggplot(ggplot2::aes(x = !!x, y = !!y)) +
    ggplot2::ggtitle(label = title) +
    ggplot2::geom_boxplot() +
    rutils::gg_theme()
}

#' Build histogram of returns from equity curve
#'
#' @param ec Equity curve
#' @param binwidth Bin width
#'
#' @return ggplot
#' @export
plot_return_histogram <- function(ec, binwidth = 0.002) {

  ec %>%
    ggplot2::ggplot(ggplot2::aes(x = ret)) +
    ggplot2::ggtitle(label = "Returns histogram") +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                                         binwidth = binwidth,
                                         boundary = 0, # Center = 0
                                         fill = "white",
                                         color = "black") +
    ggplot2::geom_density(fill = "black", alpha = 0.3) +
    rutils::gg_theme()
}
