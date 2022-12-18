assert_valid_technology <-
  function(x, allow.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
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

    msg <- "must contain only valid technology names, but has additional elements %s"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      allow.missing = allow.missing,
      .var.name = .var.name,
      add = add
    )
  }
