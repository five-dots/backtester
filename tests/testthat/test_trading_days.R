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

test_that("get_previous_trading_day() returns correct data.", {

  ## Prepare test data
  dates <- list("2018-01-02", "20180102", lubridate::ymd("2018-01-02"))
  test_data <- purrr::map(dates, get_previous_trading_day)

  purrr::walk(test_data, ~ {
    ## Type check
    expect_is(.x, "Date")
    ## Dimention check
    expect_equal(length(.x), 1)
    ## Value check
    expect_equal(.x, lubridate::ymd("2017-12-29"))
  })

  ## Error check
  expect_error(get_previous_trading_day(NULL))
  expect_error(get_previous_trading_day(NA))
  expect_error(get_previous_trading_day("1993-01-28"))
})

test_that("get_trading_range() returns correct data.", {

  ## Prepare test data
  start_date <- "2018-01-01"
  end_date <- "2018-12-31"
  unit <- "day"
  test_data <- get_trading_range(start_date, end_date, unit, lookback = Inf)

  ## Type check
  expect_is(test_data, "data.frame")
  ## Dimention check
  expect_equal(nrow(test_data), 251)
  expect_equal(ncol(test_data), 2)
  ## Value check
  expect_equal(head(test_data$last_date, 1), as.Date("2018-01-02"))
  expect_equal(tail(test_data$last_date, 1), as.Date("2018-12-31"))
  expect_true(all(test_data$first_date == as.Date("2018-01-02")))

  ## Error check
  expect_error(get_trading_range(start_date, end_date, "date"))
  expect_error(get_trading_range(start_date, end_date, "day", "10"))
  expect_error(get_trading_range(start_date, end_date, "day", NA))
  expect_error(get_trading_range(start_date, end_date, "day", NULL))
})

test_that("get_ranges_by_type() returns correct data.", {

  ## Prepare test data
  type <- "ExpandingDaily"
  start_date <- "2018-01-01"
  end_date <- "2018-12-31"
  test_data <- get_ranges_by_type(type, start_date, end_date)

  ## Type check
  expect_is(test_data, "data.frame")
  ## Dimention check
  expect_equal(nrow(test_data), 242)
  expect_equal(ncol(test_data), 2)
  ## Value check
  expect_equal(head(test_data$last_date, 1), as.Date("2018-01-16"))
  expect_equal(tail(test_data$last_date, 1), as.Date("2018-12-31"))
  expect_true(all(test_data$first_date == as.Date("2018-01-02")))

  ## Error check
  expect_error(get_ranges_by_type("hoge", start_date, end_date))
  expect_error(get_ranges_by_type(type, start_date, end_date, -1))
})
