assert_valid_bics_subgroup_code <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    msg <- "must contain only valid BICS subgroup codes, but has additional element{?s} {.val {misses}}"
    regex <- "[[:digit:]]{4}"
    assert_regex(x, regex, msg, any.missing = any.missing, .var.name = .var.name, add = NULL)
  }
