test_that("errors if unmatched strings found", {
  strings <- c(matches = "XXX", no_match = "AAA", blank = "", na = NA_character_)
  regex <- "XXX"

  msg <- "all values must match regex"

  # invalid and valid codes
  expect_error(assert_regex(strings["no_match"], regex), regexp = msg)
  expect_error(assert_regex(strings["blank"], regex), regexp = msg)
  expect_error(assert_regex(strings["na"], regex), regexp = msg)
  expect_error(assert_regex(strings, regex), regexp = msg)

  # expected possible uses
  strings_df <- data.frame(strings = strings)
  expect_error(assert_regex(strings_df$strings, regex), regexp = msg)
  expect_error(assert_regex(strings_df[["strings"]], regex), regexp = msg)
  expect_error(assert_regex(strings_df[[1L]], regex), regexp = msg)

  # unexpected input types
  expect_error(assert_regex(1L, regex), regexp = msg)
  expect_error(assert_regex(1L:2L, regex), regexp = msg)
  expect_error(assert_regex(TRUE, regex), regexp = msg)
  expect_error(assert_regex(FALSE, regex), regexp = msg)
  expect_error(assert_regex(c(TRUE, FALSE), regex), regexp = msg)
  expect_error(assert_regex(NA, regex), regexp = msg)
  expect_error(assert_regex(c(NA, NA), regex), regexp = msg)
  expect_error(assert_regex(NA_character_, regex), regexp = msg)
  expect_error(assert_regex(c(NA_character_, NA_character_), regex), regexp = msg)
})

test_that("does not error if only matching strings found", {
  strings <- c("XX-2", "AB-3", "CD-4")
  regex <- "[[:alpha:]]{2}-[[:digit:]]{1}"
  strings_df <- data.frame(strings = strings)

  expect_no_error(assert_regex(strings, regex))
  expect_no_error(assert_regex(strings_df[["strings"]], regex))
  expect_no_error(assert_regex(strings_df[[1L]], regex))

  # with allowed NAs
  strings_wna <- c("XX-2", "AB-3", "CD-4", NA_character_)
  regex <- "[[:alpha:]]{2}-[[:digit:]]{1}"
  strings_wna_df <- data.frame(strings = strings_wna)

  expect_no_error(assert_regex(strings_wna, regex, any.missing = TRUE))
  expect_no_error(assert_regex(strings_wna_df[["strings"]], regex, any.missing = TRUE))
  expect_no_error(assert_regex(strings_wna_df[[1L]], regex, any.missing = TRUE))
})
