test_that("returns a valid `abcd_scenario_bonds` obj", {
  expect_no_error(validate_abcd_scenario_bonds(fake_abcd_scenario_bonds()))
})
