test_that("errors if not a data frame", {
  msg <- "Must be of type 'data.frame'"
  expect_error(validate_abcd_flags_bonds("a"), regexp = msg)
  expect_error(validate_abcd_flags_bonds(1), regexp = msg)
  expect_error(validate_abcd_flags_bonds(list(1)), regexp = msg)
})

test_that("errors if columns are missing", {
  data <- fake_abcd_flags_bonds()
  data$credit_parent_id <- NULL
  msg <- 'column "credit_parent_id" must exist.'
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds()
  data$has_asset_level_data <- NULL
  msg <- 'column "has_asset_level_data" must exist.'
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds()
  data$has_ald_in_fin_sector <- NULL
  msg <- 'column "has_ald_in_fin_sector" must exist.'
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds()
  data$sectors_with_assets <- NULL
  msg <- 'column "sectors_with_assets" must exist.'
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds()
  data$credit_parent_id <- NULL
  data$has_asset_level_data <- NULL
  msg <- 'columns "credit_parent_id" and'
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)
  msg <- '"has_asset_level_data" must exist'
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds()
  data$credit_parent_id <- NULL
  data$has_asset_level_data <- NULL
  data$has_ald_in_fin_sector <- NULL
  msg <- 'columns "credit_parent_id", "has_asset_level_data",'
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)
  msg <- 'and "has_ald_in_fin_sector" must exist'
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)
})

test_that("errors if `credit_parent_id` contains invalid data", {
  msg <- "must contain only valid FactSet entity IDs"

  data <- fake_abcd_flags_bonds(credit_parent_id = 8)
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds(credit_parent_id = TRUE)
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds(credit_parent_id = NA_character_)
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds(credit_parent_id = "XXX")
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)
})

test_that("errors if `has_asset_level_data` contains invalid data", {
  msg <- "Must be of type 'logical'"

  data <- fake_abcd_flags_bonds(has_asset_level_data = 8)
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds(has_asset_level_data = "XXX")
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)
})

test_that("errors if `has_ald_in_fin_sector` contains invalid data", {
  msg <- "Must be of type 'logical'"

  data <- fake_abcd_flags_bonds(has_ald_in_fin_sector = 8)
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds(has_ald_in_fin_sector = "XXX")
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)
})

test_that("errors if `sectors_with_assets` contains invalid data", {
  msg <- "must be a character vector"
  data <- fake_abcd_flags_bonds(sectors_with_assets = 8)
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  msg <- "must contain only valid sector"

  data <- fake_abcd_flags_bonds(sectors_with_assets = "XXX")
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)

  data <- fake_abcd_flags_bonds(sectors_with_assets = "Power Coal")
  expect_error(validate_abcd_flags_bonds(data), regexp = msg)
})
