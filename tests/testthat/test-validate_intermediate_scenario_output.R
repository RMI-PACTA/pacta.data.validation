test_that("errors if not a data frame", {
  msg <- "Must be of type 'data.frame'"
  expect_error(validate_intermediate_scenario_output("a"), regexp = msg)
  expect_error(validate_intermediate_scenario_output(1), regexp = msg)
  expect_error(validate_intermediate_scenario_output(list(1)), regexp = msg)
})

test_that("errors if `source` contains invalid data", {
  msg <- 'column "source" must exist'
  data <- fake_intermediate_scenario_data()
  data$source <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'character'"

  data <- fake_intermediate_scenario_data(source = 8)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(source = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_intermediate_scenario_data(source = NA)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `scenario` contains invalid data", {
  msg <- 'column "scenario" must exist'
  data <- fake_intermediate_scenario_data()
  data$scenario <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'character'"

  data <- fake_intermediate_scenario_data(scenario = 8)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(scenario = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_intermediate_scenario_data(scenario = NA)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `scenario_geography` contains invalid data", {
  msg <- 'column "scenario_geography" must exist'
  data <- fake_intermediate_scenario_data()
  data$scenario_geography <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'character'"

  data <- fake_intermediate_scenario_data(scenario_geography = 8)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(scenario_geography = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_intermediate_scenario_data(scenario_geography = NA)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

})

test_that("errors if `sector` contains invalid data", {
  msg <- 'column "sector" must exist'
  data <- fake_intermediate_scenario_data()
  data$sector <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'character'"

  data <- fake_intermediate_scenario_data(sector = 8)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "must contain only valid sector names"

  data <- fake_intermediate_scenario_data(sector = "power")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `technology` contains invalid data", {
  msg <- 'column "technology" must exist'
  data <- fake_intermediate_scenario_data()
  data$technology <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'character'"

  data <- fake_intermediate_scenario_data(technology = 8)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(technology = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "must contain only valid technology"

  data <- fake_intermediate_scenario_data(technology = "renewablescap")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(technology = "")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `indicator` contains invalid data", {
  msg <- 'column "indicator" must exist'
  data <- fake_intermediate_scenario_data()
  data$indicator <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'character'"

  data <- fake_intermediate_scenario_data(indicator = 8)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(indicator = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_intermediate_scenario_data(indicator = NA)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "must contain only valid indicator names"

  data <- fake_intermediate_scenario_data(indicator = "Capacity - installed")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(indicator = "")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `units` contains invalid data", {
  msg <- 'column "units" must exist'
  data <- fake_intermediate_scenario_data()
  data$units <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'character'"

  data <- fake_intermediate_scenario_data(units = 8)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(units = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_intermediate_scenario_data(units = NA)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "must contain only valid unit names"

  data <- fake_intermediate_scenario_data(units = "MW")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(units = "")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `year` contains invalid data", {
  msg <- 'column "year" must exist'
  data <- fake_intermediate_scenario_data()
  data$year <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'integerish'"

  data <- fake_intermediate_scenario_data(year = "2021")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(year = 2021.5)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(year = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Contains missing values"
  data <- fake_intermediate_scenario_data(year = NA)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Element 1 is not >= 2000"

  data <- fake_intermediate_scenario_data(year = 21)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(year = 1999)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Element 1 is not <= 2100"
  data <- fake_intermediate_scenario_data(year = 2101)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `value` contains invalid data", {
  msg <- 'column "value" must exist'
  data <- fake_intermediate_scenario_data()
  data$value <- NULL
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Must be of type 'numeric'"

  data <- fake_intermediate_scenario_data(value = "2")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(value = TRUE)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Element 1 is not >= 0"
  data <- fake_intermediate_scenario_data(value = -2)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `technology` contains invalid data for a given `sector`", {
  msg <- "must contain only valid technology names.*for"

  data <- fake_intermediate_scenario_data(sector = "Automotive", technology = "Passenger")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Aviation", technology = "Electric")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Cement", technology = "Passenger")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Coal", technology = "Passenger")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "HDV", technology = "Passenger")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Oil&Gas", technology = "Passenger")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Power", technology = "Passenger")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Steel", technology = "Passenger")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `indicator` contains invalid data for a given `sector`", {
  msg <- "must contain only valid indicator names.*for"

  data <- fake_intermediate_scenario_data(sector = "Automotive", indicator = "Capacity")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Aviation", indicator = "Capacity")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Cement", indicator = "Capacity")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Coal", indicator = "Capacity")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "HDV", indicator = "Capacity")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Oil&Gas", indicator = "Capacity")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Power", indicator = "Sales")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Steel", indicator = "Capacity")
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})

test_that("errors if `value` contains invalid data for a given combination of `sector` and `units`", {
  msg <- "Element 1 is not >= 0"

  data <- fake_intermediate_scenario_data(value = -2)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  msg <- "Element 1 is not <="

  data <- fake_intermediate_scenario_data(sector = "Automotive", units = "# (in million)", value = 1000)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Automotive", units = "k*veh", value = 1000000)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Aviation", units = "tCO2/pkm", value = 0.1)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Cement", units = "tCO2/t Cement", value = 5)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Coal", units = "Mtce", value = 50000)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Coal", units = "mtoe", value = 20000)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "HDV", units = "k*veh", value = 150000)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Oil&Gas", units = "bcm", value = 10000)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Oil&Gas", units = "mb/d", value = 500)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Oil&Gas", units = "mtoe", value = 10000)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Power", units = "GW", value = 200000)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)

  data <- fake_intermediate_scenario_data(sector = "Steel", units = "tCO2/t Steel", value = 5)
  expect_error(validate_intermediate_scenario_output(data), regexp = msg)
})
