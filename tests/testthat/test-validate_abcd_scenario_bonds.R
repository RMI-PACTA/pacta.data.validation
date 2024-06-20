test_that("errors if not a data frame", {
  msg <- "Must be of type 'data.frame'"
  expect_error(validate_abcd_scenario_bonds("a"), regexp = msg)
  expect_error(validate_abcd_scenario_bonds(1), regexp = msg)
  expect_error(validate_abcd_scenario_bonds(list(1)), regexp = msg)
})

test_that("errors if columns are missing", {
  data <- fake_abcd_scenario_bonds()
  data$scenario_source <- NULL
  msg <- 'column "scenario_source" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$scenario <- NULL
  msg <- 'column "scenario" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$id_name <- NULL
  msg <- 'column "id_name" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$id <- NULL
  msg <- 'column "id" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$ald_company_sector_id <- NULL
  msg <- 'column "ald_company_sector_id" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$equity_market <- NULL
  msg <- 'column "equity_market" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$scenario_geography <- NULL
  msg <- 'column "scenario_geography" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$ald_sector <- NULL
  msg <- 'column "ald_sector" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$technology <- NULL
  msg <- 'column "technology" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$year <- NULL
  msg <- 'column "year" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$plan_tech_prod <- NULL
  msg <- 'column "plan_tech_prod" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$plan_br_wt_factor <- NULL
  msg <- 'column "plan_br_wt_factor" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$plan_br_wt_techshare <- NULL
  msg <- 'column "plan_br_wt_techshare" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$plan_emission_factor <- NULL
  msg <- 'column "plan_emission_factor" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$scen_tech_prod <- NULL
  msg <- 'column "scen_tech_prod" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$scen_br_wt_factor <- NULL
  msg <- 'column "scen_br_wt_factor" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$scen_br_wt_techshare <- NULL
  msg <- 'column "scen_br_wt_techshare" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$scen_emission_factor <- NULL
  msg <- 'column "scen_emission_factor" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$current_plan_row <- NULL
  msg <- 'column "current_plan_row" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds()
  data$scenario_exists <- NULL
  msg <- 'column "scenario_exists" must exist.'
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)
})

test_that("errors if `scenario_source` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_abcd_scenario_bonds(scenario_source = 8)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds(scenario_source = TRUE)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_abcd_scenario_bonds(scenario_source = NA_character_)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)
})

test_that("errors if `scenario` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_abcd_scenario_bonds(scenario = 8)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds(scenario = TRUE)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_abcd_scenario_bonds(scenario = NA_character_)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)
})

test_that("errors if `id_name` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_abcd_scenario_bonds(id_name = 8)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds(id_name = TRUE)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_abcd_scenario_bonds(id_name = NA_character_)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)
})

test_that("errors if `id` contains invalid data", {
  msg <- "must be a character vector"

  data <- fake_abcd_scenario_bonds(id = 8)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds(id = TRUE)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  msg <- "must contain only valid AI company IDs"
  data <- fake_abcd_scenario_bonds(id = NA_character_)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)
})

test_that("errors if `ald_company_sector_id` contains invalid data", {
  msg <- "Must be of type 'character'"

  data <- fake_abcd_scenario_bonds(ald_company_sector_id = 8)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds(ald_company_sector_id = TRUE)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_abcd_scenario_bonds(ald_company_sector_id = NA_character_)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)
})

test_that("errors if `equity_market` contains invalid data", {
  msg <- "valid equity market"

  data <- fake_abcd_scenario_bonds(equity_market = 8)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  data <- fake_abcd_scenario_bonds(equity_market = TRUE)
  expect_error(validate_abcd_scenario_bonds(data), regexp = msg)

  # msg <- "Contains missing values"
  # data <- fake_abcd_scenario_bonds(equity_market = NA_character_)
  # expect_error(validate_abcd_scenario_bonds(data), regexp = msg)
})
