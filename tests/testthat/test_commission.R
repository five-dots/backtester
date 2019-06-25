context("Test commission functions")

test_that("calc_ib_commission_fixed() returns correct data.", {

  ## Prepare test data
  qty <- c(200, 300, 400, 10000)
  prices <- c(10, 20, 30, 0.1)
  test_data <- calc_ib_commission_fixed(qty, prices)

  ## Type check
  expect_is(test_data, "numeric")
  ## Dimention check
  expect_equal(length(test_data), 4)
  ## Value check
  expect_equal(test_data, c(-1, -1.5, -2, -10))
  expect_true(is.na(calc_ib_commission_fixed(NA, 100)))
  expect_true(is.na(calc_ib_commission_fixed(100, NA)))

  ## Error check
  ## expect_error(calc_ib_commission_fixed(NA, NA))
  expect_error(calc_ib_commission_fixed(NULL, NULL))
  expect_error(calc_ib_commission_fixed("hoge", "fuga"))
  expect_error(calc_ib_commission_fixed(10, -100))
})

test_that("add_commission() returns correct data.", {

  ## Prepare test data
  prices <- c(10, 20, 30, 0.1, NA, 40)
  qty <- c(200, 300, 400, 10000, 500, NA)
  data <- data.frame(entry = prices, exit = prices, qty = qty)
  test_data <- add_commission(data)

  ## Type check
  expect_is(test_data, "data.frame")
  ## Dimention check
  expect_equal(ncol(test_data), 5)
  ## Value check
  asn <- c(-1, -1.5, -2, -10, NA, NA)
  expect_equal(test_data$entry_comm, asn)
  expect_equal(test_data$exit_comm, asn)

  ## Error check
  expect_error(add_commission(1))
  expect_error(add_commission(data.frame(x = 1:10, y = 1:10)))
  expect_error(add_commission(data, comm_fun = mean))
})
