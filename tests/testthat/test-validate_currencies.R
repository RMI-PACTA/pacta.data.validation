test_that("errors if not a data frame", {
  msg <- "Must be of type 'data.frame'"
  expect_error(validate_currencies("a"), regexp = msg)
  expect_error(validate_currencies(1), regexp = msg)
  expect_error(validate_currencies(list(1)), regexp = msg)
})

test_that("errors if `currency` contains invalid data", {
  msg <- 'column "currency" must exist'
  data <- fake_currencies()
  data$currency <- NULL
  expect_error(validate_currencies(data), regexp = msg)

  msg <- "Must be of type 'character'"

  data <- fake_currencies(currency = 8)
  expect_error(validate_currencies(data), regexp = msg)

  data <- fake_currencies(currency = TRUE)
  expect_error(validate_currencies(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_currencies(currency = NA)
  expect_error(validate_currencies(data), regexp = msg)

  msg <- "must contain only valid iso4217c currency"

  data <- fake_currencies(currency = "XXX")
  expect_error(validate_currencies(data), regexp = msg)

  data <- fake_currencies(currency = "usd")
  expect_error(validate_currencies(data), regexp = msg)

  data <- fake_currencies(currency = "")
  expect_error(validate_currencies(data), regexp = msg)
})

test_that("errors if `exchange_rate` contains invalid data", {
  msg <- 'column "exchange_rate" must exist'
  data <- fake_currencies()
  data$exchange_rate <- NULL
  expect_error(validate_currencies(data), regexp = msg)

  msg <- "Must be of type 'numeric'"

  data <- fake_currencies(exchange_rate = "222")
  expect_error(validate_currencies(data), regexp = msg)

  data <- fake_currencies(exchange_rate = TRUE)
  expect_error(validate_currencies(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_currencies(exchange_rate = NA)
  expect_error(validate_currencies(data), regexp = msg)

  msg <- "Element 1 is not >= 0"
  data <- fake_currencies(exchange_rate = -0.5)
  expect_error(validate_currencies(data), regexp = msg)
})
