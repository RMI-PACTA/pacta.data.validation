assert_valid_scenario_geography <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "Global",
        "GlobalAggregate",
        "NonOECD",
        "OECD"
      )

    msg <- "must contain only valid scenario geography names, but has additional elements %s"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
