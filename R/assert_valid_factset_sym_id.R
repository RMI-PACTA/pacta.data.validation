assert_valid_factset_sym_id <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    msg <- "must contain only valid FactSet sym IDs, but has additional element{?s} {.val {misses}}"
    regex <- "[[:alnum:]]{6}-S"
    assert_regex(x, regex, msg, any.missing = any.missing, .var.name = .var.name, add = NULL)
  }
