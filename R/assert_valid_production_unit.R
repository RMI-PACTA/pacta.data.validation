assert_valid_production_unit <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "DWT km",
        "GJ",
        "MW",
        "pkm",
        "tkm",
        "tonnes of cement",
        "tonnes of coal",
        "tonnes of steel",
        "vehicles produced"
      )

    msg <- "must contain only valid production units, but has additional elements %s"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
