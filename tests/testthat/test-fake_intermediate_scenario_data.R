test_that("returns a valid `fake_intermediate_scenario_data` obj", {
  expect_no_error(validate_intermediate_scenario_output(fake_intermediate_scenario_data()))
})
