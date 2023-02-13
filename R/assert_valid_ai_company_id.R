assert_valid_ai_company_id <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    if (!is.character(x)) {
      return(checkmate::makeAssertion(x, "must be a character vector", .var.name, add))
    }

    msg <- "must contain only valid AI company IDs, but has additional element{?s} {.val {misses}}"
    regex <- "[[:digit:]]+"
    assert_regex(x, regex, msg, any.missing = any.missing, .var.name = .var.name, add = NULL)
  }
