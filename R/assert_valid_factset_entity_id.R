assert_valid_factset_entity_id <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    msg <- "must contain only valid FactSet entity IDs, but has additional elements %s"
    regex <- "[[:alnum:]]{6}-E"
    assert_regex(x, regex, msg, any.missing = any.missing, .var.name = .var.name, add = NULL)
  }
