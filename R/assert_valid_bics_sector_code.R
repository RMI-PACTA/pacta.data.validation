assert_valid_bics_sector_code <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    msg <- "must contain only valid BICS sector codes, but has additional elements %s"
    regex <- "[[:digit:]]{4}"
    assert_regex(x, regex, msg, any.missing = any.missing, .var.name = .var.name, add = NULL)
  }
