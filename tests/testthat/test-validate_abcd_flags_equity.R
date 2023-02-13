test_that("errors if not a data frame", {
  msg <- "Must be of type 'data.frame'"
  expect_error(validate_abcd_flags_equity("a"), regexp = msg)
  expect_error(validate_abcd_flags_equity(1), regexp = msg)
  expect_error(validate_abcd_flags_equity(list(1)), regexp = msg)
})

test_that("errors if columns are missing", {
  data <- fake_abcd_flags_equity()
  data$isin <- NULL
  msg <- 'column "isin" must exist.'
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity()
  data$has_asset_level_data <- NULL
  msg <- 'column "has_asset_level_data" must exist.'
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity()
  data$has_ald_in_fin_sector <- NULL
  msg <- 'column "has_ald_in_fin_sector" must exist.'
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity()
  data$sectors_with_assets <- NULL
  msg <- 'column "sectors_with_assets" must exist.'
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity()
  data$isin <- NULL
  data$has_asset_level_data <- NULL
  msg <- 'columns "isin" and "has_asset_level_data" must'
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity()
  data$isin <- NULL
  data$has_asset_level_data <- NULL
  data$has_ald_in_fin_sector <- NULL
  msg <- 'columns "isin", "has_asset_level_data", and'
  expect_error(validate_abcd_flags_equity(data), regexp = msg)
  msg <- '"has_ald_in_fin_sector" must exist'
  expect_error(validate_abcd_flags_equity(data), regexp = msg)
})

test_that("errors if `isin` contains invalid data", {
  msg <- "must contain only valid ISINs"

  data <- fake_abcd_flags_equity(isin = 8)
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity(isin = TRUE)
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity(isin = NA_character_)
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity(isin = "XXX")
  expect_error(validate_abcd_flags_equity(data), regexp = msg)
})

test_that("errors if `has_asset_level_data` contains invalid data", {
  msg <- "Must be of type 'logical'"

  data <- fake_abcd_flags_equity(has_asset_level_data = 8)
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity(has_asset_level_data = "XXX")
  expect_error(validate_abcd_flags_equity(data), regexp = msg)
})

test_that("errors if `has_ald_in_fin_sector` contains invalid data", {
  msg <- "Must be of type 'logical'"

  data <- fake_abcd_flags_equity(has_ald_in_fin_sector = 8)
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity(has_ald_in_fin_sector = "XXX")
  expect_error(validate_abcd_flags_equity(data), regexp = msg)
})

test_that("errors if `sectors_with_assets` contains invalid data", {
  msg <- "must be a character vector"
  data <- fake_abcd_flags_equity(sectors_with_assets = 8)
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  msg <- "must contain only valid sector"

  data <- fake_abcd_flags_equity(sectors_with_assets = "XXX")
  expect_error(validate_abcd_flags_equity(data), regexp = msg)

  data <- fake_abcd_flags_equity(sectors_with_assets = "Power Coal")
  expect_error(validate_abcd_flags_equity(data), regexp = msg)
})
