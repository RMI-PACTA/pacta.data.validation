assert_valid_ai_company_id <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    msg <- "must contain only valid AI company IDs, but has additional elements %s"
    regex <- "[[:digit:]]+"
    assert_regex(x, regex, msg, any.missing = any.missing, .var.name = .var.name, add = NULL)
  }
