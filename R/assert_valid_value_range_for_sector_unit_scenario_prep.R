assert_valid_value_range_for_unit_scenario_prep <-
  function(values, units, sectors, any.missing = FALSE, .var.name = checkmate::vname(values), add = NULL) {
    # styler: off
    # values set by using a somewhat higher value than the global maximum of
    # scenarios we use
    value_ranges <- dplyr::tribble(
      ~sector,                 ~unit, ~max_value,
      "Automotive", "# (in million)",        150,
      "Automotive",          "k*veh",     200000,
      "Aviation",         "gCO2/pkm",     0.0002,
      "Cement",      "tCO2/t Cement",          1,
      "Coal",                 "Mtce",      10000,
      "Coal",                 "mtoe",       6000,
      "HDV",                 "k*veh",     100000,
      "Oil&Gas",               "bcm",       6000,
      "Oil&Gas",              "mb/d",        100,
      "Oil&Gas",              "mtoe",       6800,
      "Power",                  "GW",     100000,
      "Steel",        "tCO2/t Steel",        2.5
    )
    # styler: on

    for (i in 1:nrow(value_ranges)) {
      sector_idx <- sectors == value_ranges$sector[[i]]
      units_idx <- units == value_ranges$unit[[i]]
      sectors_units_idx <- sector_idx & units_idx
      max_value_i <- value_ranges$max_value[[i]]

      checkmate::assert_numeric(values[sectors_units_idx], lower = 0, upper = max_value_i, any.missing = any.missing, add = add, .var.name = .var.name)
    }

    invisible()
  }

