test_that("returns a valid `abcd_flags_equity` obj", {
  expect_no_error(validate_abcd_flags_equity(fake_abcd_flags_equity()))
})
