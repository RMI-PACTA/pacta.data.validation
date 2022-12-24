test_that("returns expected value", {
  expect_equal(set_collapse(NA), "`<NA>`")
  expect_equal(set_collapse(""), '""')
  expect_equal(set_collapse("xxx"), '"xxx"')
  expect_equal(set_collapse(c(NA, "")), '`<NA>`, ""')
  expect_equal(set_collapse(c(NA, "xxx")), '`<NA>`, "xxx"')
  expect_equal(set_collapse(c("", "xxx")), '"", "xxx"')
  expect_equal(set_collapse(c(NA, "", "xxx")), '`<NA>`, "", "xxx"')
})
