#' Calculate IB's stock commission of fixed plan.
#'
#' @param qty Quantity
#' @param price Price
#'
#' @return Numeric scalar
#' @export
calc_ib_commission_fixed <- function(qty, price) {
  stopifnot((is.numeric(qty) | is.na(qty)),
           ## Price > 0 or NA
           ((is.numeric(price) & price > 0) | is.na(price)))

  # Shares * 0.005 (min $1, max 1% of trade value)
  # https://www.interactivebrokers.com/en/index.php?f=1590
  qty <- abs(qty)
  lot <- -0.005
  min <- -1
  max <- qty * price * -0.01

  comm <- qty * lot
  comm <- dplyr::case_when(
    is.na(qty) | is.na(price) ~ NA_real_,
    comm > min ~ min,
    comm < max ~ max,
    TRUE ~ comm
  )
  round(comm, 2)
}

#' Add IB's stock commission of fixed plan.
#'
#' @param data data.frame contains "entry" and "exit" price and "qty"
#' @param comm_fun commission functions whose args are "qty" and "price"
#'
#' @return data.frame
#' @export
add_commission <- function(data, comm_fun = calc_ib_commission_fixed) {
  stopifnot(is.data.frame(data),
            all(colnames(data) %in% c("entry", "exit", "qty")),
            is.function(comm_fun),
            ## Check function args are qty and price
            all(names(formals(calc_ib_commission_fixed)) == c("qty", "price")))

  data %>%
    dplyr::mutate(entry_comm = comm_fun(.data$qty, .data$entry),
                  exit_comm = comm_fun(.data$qty, .data$exit))
}
