test_that("errors if not a data frame", {
  msg <- "Must be of type 'data.frame'"
  expect_error(validate_masterdata_debt_datastore("a"), regexp = msg)
  expect_error(validate_masterdata_debt_datastore(1), regexp = msg)
  expect_error(validate_masterdata_debt_datastore(list(1)), regexp = msg)
})

test_that("errors if `id` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_masterdata_debt_datastore(id = 8)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(id = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Must comply to pattern '^[0-9]+$'"

  data <- fake_masterdata_debt_datastore(id = "x")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg, fixed = TRUE)

  data <- fake_masterdata_debt_datastore(id = "")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg, fixed = TRUE)

  data <- fake_masterdata_debt_datastore(id = "-8")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg, fixed = TRUE)

  msg <- "Contains missing values"
  data <- fake_masterdata_debt_datastore(id = NA)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `id_name` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_masterdata_debt_datastore(id_name = 8)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(id_name = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_masterdata_debt_datastore(id_name = NA)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `ald_sector` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_masterdata_debt_datastore(ald_sector = 8)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_sector = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_masterdata_debt_datastore(ald_sector = NA)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "must contain only valid sector names"

  data <- fake_masterdata_debt_datastore(ald_sector = "coal")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_sector = "")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `ald_location` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_masterdata_debt_datastore(ald_location = 8)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_location = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "must contain only valid iso2c country"

  data <- fake_masterdata_debt_datastore(ald_location = "xx")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_location = "us")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_location = "")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `technology` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_masterdata_debt_datastore(technology = 8)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(technology = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_masterdata_debt_datastore(technology = NA)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "must contain only valid technology"

  data <- fake_masterdata_debt_datastore(technology = "coal")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(technology = "")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `year` contains invalid data", {
  msg <- "Must be of type 'integerish'"

  data <- fake_masterdata_debt_datastore(year = "2021")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(year = 2021.5)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(year = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_masterdata_debt_datastore(year = NA)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Element 1 is not >= 2000"

  data <- fake_masterdata_debt_datastore(year = 21)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(year = 1999)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Element 1 is not <= 2100"
  data <- fake_masterdata_debt_datastore(year = 2101)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `ald_production` contains invalid data", {
  msg <- "Must be of type 'numeric'"

  data <- fake_masterdata_debt_datastore(ald_production = "222")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_production = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_masterdata_debt_datastore(ald_production = NA)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Element 1 is not >= 0"
  data <- fake_masterdata_debt_datastore(ald_production = -222)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `ald_production_unit` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_masterdata_debt_datastore(ald_production_unit = 1)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_production_unit = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_masterdata_debt_datastore(ald_production_unit = NA)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "must contain only valid.*production units"

  data <- fake_masterdata_debt_datastore(ald_production_unit = "x")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_production_unit = "")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `ald_emissions_factor` contains invalid data", {
  msg <- "Must be of type 'numeric'"

  data <- fake_masterdata_debt_datastore(ald_emissions_factor = "2")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_emissions_factor = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "Element 1 is not >= 0"
  data <- fake_masterdata_debt_datastore(ald_emissions_factor = -2)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `ald_emissions_factor_unit` contains invalid data", {
  msg <- "Must be of type.*'character'"

  data <- fake_masterdata_debt_datastore(ald_emissions_factor_unit = 1)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_emissions_factor_unit = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "must contain only valid.*emissions factor units"

  data <- fake_masterdata_debt_datastore(ald_emissions_factor_unit = "x")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(ald_emissions_factor_unit = "")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})

test_that("errors if `country_of_domicile` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_masterdata_debt_datastore(country_of_domicile = 8)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(country_of_domicile = TRUE)
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  msg <- "must contain only valid iso2c.*country"

  data <- fake_masterdata_debt_datastore(country_of_domicile = "xx")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(country_of_domicile = "us")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)

  data <- fake_masterdata_debt_datastore(country_of_domicile = "")
  expect_error(validate_masterdata_debt_datastore(data), regexp = msg)
})
