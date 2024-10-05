test_that("returns a valid `abcd_scenario_equity` obj", {
  expect_no_error(validate_abcd_scenario_equity(fake_abcd_scenario_equity()))
})
