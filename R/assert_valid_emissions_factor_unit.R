assert_valid_emissions_factor_unit <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "tonnes of CO2 per DWT km",
        "tCO2/dwt km",
        "tonnes of CO2 per GJ",
        "tCO2e/GJ",
        "tonnes of CO2 per km",
        "tCO2/km",
        "tonnes of CO2 per MWh per year",
        "tCO2e/MWh",
        "tonnes of CO2 per pkm",
        "tCO2/pkm",
        "tonnes of CO2 per tkm",
        "tCO2/tkm",
        "tonnes of CO2 per tonnes of cement",
        "tCO2e/t cement",
        "tonnes of CO2 per tonnes of coal",
        "tCO2e/t coal",
        "tonnes of CO2 per tonnes of steel",
        "tCO2e/t steel",
      )

    msg <- "must contain only valid emissions factor units, but has additional element{?s} {.val {misses}}"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
