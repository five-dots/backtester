context("Test trading tdays functions")

test_that("get_trading_days() returns correct data.", {

  ## Prepare test data
  test_data <- get_trading_days("2018-01-02", "2018-12-31")

  ## Type check
  expect_is(test_data, "Date")
  ## Dimention check
  expect_equal(length(test_data), 251)
  ## Value check
  expect_equal(head(test_data, 1), as.Date("2018-01-02"))
  expect_equal(tail(test_data, 1), as.Date("2018-12-31"))

  ## Error check
  expect_error(get_trading_days("1993-01-28", Sys.Date()))
  expect_error(get_trading_days("2018-01-02", "2018-01-01"))
  expect_error(get_trading_days(NULL, NULL))
  expect_error(get_trading_days(NA, NA))
})
