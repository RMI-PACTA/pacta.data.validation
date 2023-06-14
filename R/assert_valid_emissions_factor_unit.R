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

assert_valid_emissions_factor_unit_ai <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "tCO2/dwt km",
        "tCO2/km",
        "tCO2/pkm",
        "tCO2/tkm",
        "tCO2e/GJ",
        "tCO2e/MWh",
        "tCO2e/t cement",
        "tCO2e/t coal",
        "tCO2e/t steel"
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
