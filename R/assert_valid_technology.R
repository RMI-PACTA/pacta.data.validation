assert_valid_technology <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "Basic Oxygen Furnace",
        "Coal",
        "CoalCap",
        "Electric",
        "Electric Arc Furnace",
        "Electric_HDV",
        "Freight",
        "Fuel Cell_HDV",
        "FuelCell",
        "Gas",
        "GasCap",
        "Grinding",
        "Hybrid",
        "Hybrid No-Plug_HDV",
        "HydroCap",
        "ICE",
        "ICE CNG_HDV",
        "ICE Diesel_HDV",
        "ICE Gasoline_HDV",
        "ICE Propane_HDV",
        "Integrated facility",
        "Natural Gas Liquids",
        "NuclearCap",
        "Oil",
        "OilCap",
        "Open Hearth Furnace",
        "Passenger",
        "RenewablesCap"
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

assert_valid_technology_ai <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "Anthracite Metallurgical",
        "Anthracite Thermal",
        "Basic Oxygen Furnace",
        "Bituminous Metallurgical",
        "Bituminous Thermal",
        "CoalCap",
        "Electric",
        "Electric Arc Furnace",
        "Freight",
        "Fuel Cell",
        "Gas",
        "GasCap",
        "Hybrid No-Plug",
        "Hybrid Plug-In",
        "HydroCap",
        "ICE CNG",
        "ICE Diesel",
        "ICE E85+",
        "ICE Gasoline",
        "ICE Hydrogen",
        "ICE Propane",
        "Integrated facility",
        "Lignite Thermal",
        "Natural Gas Liquids",
        "NuclearCap",
        "Oil and Condensate",
        "OilCap",
        "Open Hearth Furnace",
        "Passenger",
        "RenewablesCap",
        "Sub-Bituminous Thermal"
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
