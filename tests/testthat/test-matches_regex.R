test_that("always outputs a logical vector", {
  strings <- c(matches = "XXX", no_match = "AAA", blank = "", na = NA_character_)
  regex <- "XXX"
  expect_vector(matches_regex(strings[1L], regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(strings[1L], regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(strings[1L], regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(strings[1L], regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(strings, regex), ptype = logical(), size = 4L)
  expect_vector(matches_regex(character(0), regex), ptype = logical(), size = 0L)

  # expected possible uses
  strings_df <- data.frame(strings = strings)

  out <- dplyr::mutate(strings_df, matches_regex = matches_regex(strings, regex))$matches_regex
  expect_vector(out, ptype = logical(), size = 4L)

  out <- matches_regex(strings_df$strings, regex)
  expect_vector(out, ptype = logical(), size = 4L)

  out <- matches_regex(strings_df["strings"], regex)
  expect_vector(out, ptype = logical(), size = 4L)

  out <- matches_regex(strings_df[1L], regex)
  expect_vector(out, ptype = logical(), size = 4L)

  out <- matches_regex(strings_df[["strings"]], regex)
  expect_vector(out, ptype = logical(), size = 4L)

  out <- matches_regex(strings_df[[1L]], regex)
  expect_vector(out, ptype = logical(), size = 4L)

  # unexpected input types
  expect_vector(matches_regex(1L, regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(1L:2L, regex), ptype = logical(), size = 2L)
  expect_vector(matches_regex(TRUE, regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(FALSE, regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(c(TRUE, FALSE), regex), ptype = logical(), size = 2L)
  expect_vector(matches_regex(NA, regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(c(NA, NA), regex), ptype = logical(), size = 2L)
  expect_vector(matches_regex(NA_character_, regex), ptype = logical(), size = 1L)
  expect_vector(matches_regex(c(NA_character_, NA_character_), regex), ptype = logical(), size = 2L)
})

test_that("returns expected values", {
  strings <- c(matches = "XXX", no_match = "AAA", blank = "", NA_character_)
  regex <- "XXX"

  # typical usage
  expect_identical(matches_regex(strings, regex), c(TRUE, FALSE, FALSE, FALSE))

  # unexpected input types
  expect_identical(matches_regex(NA, regex), FALSE)
  expect_identical(matches_regex(NA_character_, regex), FALSE)
  expect_identical(matches_regex(c(TRUE, FALSE), regex), c(FALSE, FALSE))
  expect_identical(matches_regex(1L:2L, regex), c(FALSE, FALSE))
})
