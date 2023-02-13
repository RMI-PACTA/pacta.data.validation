test_that("returns a valid `masterdata_debt_datastore` obj", {
  expect_no_error(validate_masterdata_debt_datastore(fake_masterdata_debt_datastore()))
})
