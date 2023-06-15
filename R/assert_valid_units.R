assert_valid_units <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "# (in million)",
        "bcm",
        "gCO2/pkm",
        "GW",
        "k*veh",
        "mb/d",
        "Mtce",
        "mtoe",
        "t CO2/ t Steel",
        "tCO2/t Cement",
        "tCO2/t Steel"
      )

    msg <- "must contain only valid technology names, but has additional element{?s} {.val {misses}}"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
