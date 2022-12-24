assert_valid_emissions_factor_unit <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "tonnes of CO2 per DWT km",
        "tonnes of CO2 per GJ",
        "tonnes of CO2 per km",
        "tonnes of CO2 per MWh per year",
        "tonnes of CO2 per pkm",
        "tonnes of CO2 per tkm",
        "tonnes of CO2 per tonnes of cement",
        "tonnes of CO2 per tonnes of coal",
        "tonnes of CO2 per tonnes of steel"
      )

    msg <- "must contain only valid emissions factor units, but has additional elements %s"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
