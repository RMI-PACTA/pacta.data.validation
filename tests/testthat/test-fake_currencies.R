test_that("returns a valid `currencies` obj", {
  expect_no_error(validate_currencies(fake_currencies()))
})
