test_that("no error when columns exist", {
  data <- fake_masterdata_debt_datastore()
  expect_no_error(assert_columns_exists(data, "id"))
  expect_no_error(assert_columns_exists(data, c("id", "id_name")))
})

test_that("error when columns do not exist", {
  data <- fake_masterdata_debt_datastore()
  expect_error(assert_columns_exists(data, "x"))
  expect_error(assert_columns_exists(data, c("x", "y")))
})

test_that("error when some columns do not exist", {
  data <- fake_masterdata_debt_datastore()
  expect_error(assert_columns_exists(data, c("id", "x")))
})

test_that("appropriate error when columns do not exist", {
  data <- fake_masterdata_debt_datastore()
  expect_error(assert_columns_exists(data, "x"), regexp = 'column "x" must exist')
  expect_error(assert_columns_exists(data, c("x", "y")), regexp = 'columns "x" and "y" must exist')
  expect_error(assert_columns_exists(data, c("x", "y", "z")), regexp = 'columns "x", "y", and "z" must exist')
})

