test_that("errors if invalid ISIN found", {
  msg <- "must contain only valid ISIN"

  isins <-
    c(
      invalid_isin = "XXX",
      wrong_luhn = "US0378331009",
      na = NA_character_,
      valid_isin = "US0378331005"
    )

  # invalid and valid codes
  expect_error(assert_valid_isin(isins[1L]), regexp = msg)
  expect_error(assert_valid_isin(isins[2L]), regexp = msg)
  expect_error(assert_valid_isin(isins[3L]), regexp = msg)
  expect_error(assert_valid_isin(isins), regexp = msg)

  # expected possible uses
  isins_df <- data.frame(isin = isins)
  expect_error(assert_valid_isin(isins_df$isin), regexp = msg)
  expect_error(assert_valid_isin(isins_df[["isin"]]), regexp = msg)
  expect_error(assert_valid_isin(isins_df[[1L]]), regexp = msg)

  # unexpected input types
  expect_error(assert_valid_isin(1L), regexp = msg)
  expect_error(assert_valid_isin(1L:2L), regexp = msg)
  expect_error(assert_valid_isin(TRUE), regexp = msg)
  expect_error(assert_valid_isin(FALSE), regexp = msg)
  expect_error(assert_valid_isin(c(TRUE, FALSE)), regexp = msg)
  expect_error(assert_valid_isin(NA), regexp = msg)
  expect_error(assert_valid_isin(c(NA, NA)), regexp = msg)
  expect_error(assert_valid_isin(NA_character_), regexp = msg)
  expect_error(assert_valid_isin(c(NA_character_, NA_character_)), regexp = msg)
})

test_that("does not error if only valid ISINs found", {
  isins <- c("US0378331005", "CH0228531437")
  isins_df <- data.frame(isin = isins)
  expect_no_error(assert_valid_isin(isins))
  expect_no_error(assert_valid_isin(isins_df[["isin"]]))
  expect_no_error(assert_valid_isin(isins_df[[1L]]))

  isins_wna <- c("US0378331005", "CH0228531437", NA_character_)
  isins_wna_df <- data.frame(isin = isins_wna)
  expect_no_error(assert_valid_isin(isins_wna, any.missing = TRUE))
  expect_no_error(assert_valid_isin(isins_wna_df[["isin"]], any.missing = TRUE))
  expect_no_error(assert_valid_isin(isins_wna_df[[1L]], any.missing = TRUE))
})
