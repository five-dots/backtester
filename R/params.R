#' Generate strategy parameters
#'
#' @param ... Vector of parameters
#' @param constraints Constraints of perameters
#'
#' @return data.frame
#' @export
generate_params <- function(..., constraints = NULL) {
  params <- expand.grid(..., stringsAsFactors = FALSE)
  colnames(params) <- sapply(substitute(list(...))[-1], deparse)

  if (!is.null(constraints)) {
    constraints <- dplyr::enquo(constraints)
    params %>%
      dplyr::filter(!!constraints)
  } else {
    params
  }
}

#' Get best parameter value based on median performance
#'
#' @param df data.frame of performance
#' @param param_col Parameter column
#' @param perf_col Performance column
#' @param select_fun Select function if there are multiple results
#'
#' @return scalar
#' @export
get_best_param_by_median <- function(df, param_col, perf_col, select_fun = min) {
  param_col <- dplyr::enquo(param_col)
  perf_col <- dplyr::enquo(perf_col)

  # Unique param vector
  params <- df %>% dplyr::pull(!!param_col) %>% unique() %>% sort()

  # Get median value using boxplot.stats (the same logic as geom_boxplot)
  result <- purrr::map_dfr(params, ~ {
    perf <- df %>%
      dplyr::filter(!!param_col == .x) %>%
      dplyr::pull(!!perf_col)
    median <- grDevices::boxplot.stats(perf)$stats[3]
    data.frame(param = .x, value = median)
  })

  # Return param with max median value
  result %>%
    # Convert factor to numeric to work with max/min function
    dplyr::filter(value == max(value, na.rm = TRUE)) %>%
    dplyr::pull(param) %>%
    select_fun()
}

#' Select parameter columns from performance data.frame
#'
#' @param df data.frame of performance
#'
#' @return data.frame
#' @export
select_param_cols <- function(df) {
  is_parameter <- function(x) unique(x) %>% length() > 1

  # Remove perf colnames
  all_cols <- colnames(df)
  perf_idx <- min(which(all_cols == "ttl_win"),
                  which(all_cols == "net_pnl"),
                  which(all_cols == "apr"))
  param_cols <- all_cols[1:(perf_idx - 1)]

  # Select params that's value is not unique
  df %>%
    dplyr::select(dplyr::one_of(param_cols)) %>%
    dplyr::select_if(is_parameter)
}

#' Get all best parameter values based on median performance
#'
#' @param df data.frame of performance
#' @param perf_col Performance column
#' @param select_fun Select function if there are multiple results
#'
#' @return list
#' @export
get_all_best_params_by_median <- function(df, perf_col, select_fun = min) {

  perf_col <- dplyr::enquo(perf_col)

  # Extract param cols and symbols
  params_df <- select_param_cols(df)
  params_symbols <- params_df %>% colnames() %>% dplyr::syms()

  # Iterate all symbols
  purrr::map_dfc(params_symbols, ~ {
    param <- get_best_param_by_median(df, !!.x, !!perf_col, select_fun)
    colname <- dplyr::quo_name(.x)
    tibble::tibble(!!colname := param)
  })
}
