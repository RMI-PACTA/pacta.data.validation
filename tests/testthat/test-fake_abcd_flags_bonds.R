test_that("returns a valid `abcd_flags_bond` obj", {
  expect_no_error(validate_abcd_flags_bonds(fake_abcd_flags_bonds()))
})
