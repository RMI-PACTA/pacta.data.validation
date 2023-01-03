test_that("errors if not a data frame", {
  msg <- "Must be of type 'data.frame'"
  expect_error(validate_financial_data("a"), regexp = msg)
  expect_error(validate_financial_data(1), regexp = msg)
  expect_error(validate_financial_data(list(1)), regexp = msg)
})

test_that("errors if columns are missing", {
  col_names <-
    c(
      "isin",
      "unit_share_price",
      "current_shares_outstanding_all_classes",
      "asset_type",
      "factset_entity_id"
    )

  for (col_name in col_names) {
    data <- fake_financial_data()
    data[col_name] <- NULL
    msg <- paste0('column "', col_name, '".*must exist.')
    expect_error(validate_financial_data(data), regexp = msg)
  }
})

test_that("errors if `isin` contains invalid data", {
  msg <- "must contain only valid ISINs"

  data <- fake_financial_data(isin = 123)
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(isin = "XXX")
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(isin = NA_character_)
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(isin = TRUE)
  expect_error(validate_financial_data(data), regexp = msg)
})

test_that("errors if `unit_share_price` contains invalid data", {
  msg <- "Must be of type 'numeric'"

  data <- fake_financial_data(unit_share_price = "123")
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(unit_share_price = TRUE)
  expect_error(validate_financial_data(data), regexp = msg)
})

test_that("errors if `current_shares_outstanding_all_classes` contains invalid data", {
  msg <- "Must be of.*type 'numeric'"

  data <- fake_financial_data(current_shares_outstanding_all_classes = "123")
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(current_shares_outstanding_all_classes = TRUE)
  expect_error(validate_financial_data(data), regexp = msg)
})

test_that("errors if `asset_type` contains invalid data", {
  msg <- "must contain only valid asset type"

  data <- fake_financial_data(asset_type = 123)
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(asset_type = TRUE)
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(asset_type = "XXX")
  expect_error(validate_financial_data(data), regexp = msg)
})

test_that("errors if `factset_entity_id` contains invalid data", {
  msg <- "must contain only valid FactSet entity"

  data <- fake_financial_data(factset_entity_id = 123)
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(factset_entity_id = TRUE)
  expect_error(validate_financial_data(data), regexp = msg)

  data <- fake_financial_data(factset_entity_id = "XXX")
  expect_error(validate_financial_data(data), regexp = msg)
})
