test_that("returns a valid `masterdata_ownership_datastore` obj", {
  expect_no_error(validate_masterdata_ownership_datastore(fake_masterdata_ownership_datastore()))
})
