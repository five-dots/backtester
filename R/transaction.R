#' Add cost and Pnl
#'
#' @param trades data.frame of trades contains "entry" and "exit" price and qty
#'
#' @return data.frame
#' @export
add_pnl <- function(trades) {
  dplyr::mutate(trades,
                cost = dplyr::if_else(is.na(.data$entry), NA_real_,
                                      abs(.data$entry * .data$qty) +
                                      abs(.data$entry_comm) + abs(.data$exit_comm)),

                pnl = dplyr::if_else(is.na(.data$entry), NA_real_,
                (.data$exit * .data$qty) -
                (.data$entry * .data$qty) +
                .data$entry_comm + .data$exit_comm),
                ret = .data$pnl / .data$cost)
}
