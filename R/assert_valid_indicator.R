assert_valid_indicator <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    # TODO: the allowed indicators vary slightly by scenario and should be standardized where possible
    allowed_strings <-
      c(
        "Capacity",
        "Capacity: installed",
        "Emission Intensity",
        "Production",
        "Sales",
        "Supply"
      )

    msg <- "must contain only valid indicator names, but has additional element{?s} {.val {misses}}"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
