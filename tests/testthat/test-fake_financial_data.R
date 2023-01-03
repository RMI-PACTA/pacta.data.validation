test_that("returns a valid `financial_data` obj", {
  expect_no_error(validate_financial_data(fake_financial_data()))
})
