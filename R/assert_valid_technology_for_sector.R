assert_valid_technology_for_sector <-
  function(technologies, sectors, any.missing = FALSE, .var.name = checkmate::vname(technologies), add = NULL) {
    allowed_strings <-
      list(
        "Automotive" =
          c(
            "Electric",
            "FuelCell",
            "Hybrid",
            "ICE"
          ),
        "Aviation" =
          c(
            "Freight",
            "Passenger"
          ),
        "Cement" =
          c(
            "Grinding",
            "Integrated facility"
          ),
        "Coal" =
          c(
            "Coal"
          ),
        "HDV" =
          c(
            "Electric_HDV",
            "Fuel Cell_HDV",
            "Hybrid No-Plug_HDV",
            "ICE CNG_HDV",
            "ICE Diesel_HDV",
            "ICE Gasoline_HDV",
            "ICE Propane_HDV"
          ),
        "Oil&Gas" =
          c(
            "Gas",
            "Natural Gas Liquids",
            "Oil"
          ),
        "Power" =
          c(
            "CoalCap",
            "GasCap",
            "HydroCap",
            "NuclearCap",
            "OilCap",
            "RenewablesCap"
          ),
        "Shipping" =
          c(
            "Freight",
            "Passenger"
          ),
        "Steel" =
          c(
            "Basic Oxygen Furnace",
            "Electric Arc Furnace",
            "Open Hearth Furnace"
          )
      )

    vapply(
      X = seq_along(allowed_strings),
      FUN = function(i) {
        idxs <- sectors == names(allowed_strings)[[i]]
        msg <- paste0("must contain only valid technology names for ", names(allowed_strings)[[i]], ", but has additional elements %s")

        assert_subset(
          x = technologies[idxs],
          choices = allowed_strings[[i]],
          msg = msg,
          any.missing = any.missing,
          .var.name = .var.name,
          add = add
        )
        names(allowed_strings)[[i]]
      },
      FUN.VALUE = character(1)
    )

    invisible()
  }

assert_valid_technology_for_sector_scenario_prep <-
  function(technologies, sectors, any.missing = FALSE, .var.name = checkmate::vname(technologies), add = NULL) {
    allowed_strings <-
      list(
        "Automotive" =
          c(
            "Electric",
            "FuelCell",
            "Hybrid",
            "ICE"
          ),
        "Aviation" =
          c(
            "Passenger"
          ),
        "Cement" =
          c(
            NA_character_
          ),
        "Coal" =
          c(
            "Coal"
          ),
        "HDV" =
          c(
            "Electric",
            "Fuel Cell",
            "Hybrid",
            "ICE"
          ),
        "Oil&Gas" =
          c(
            "Gas",
            "Oil"
          ),
        "Power" =
          c(
            "CoalCap",
            "GasCap",
            "HydroCap",
            "NuclearCap",
            "OilCap",
            "RenewablesCap"
          ),
        # TODO: remove SHIPPING for good?
        # "Shipping" =
        #   c(
        #     "Freight",
        #     "Passenger"
        #   ),
        "Steel" =
          c(
            NA_character_
          )
      )

    vapply(
      X = seq_along(allowed_strings),
      FUN = function(i) {
        idxs <- sectors == names(allowed_strings)[[i]]
        msg <- paste0("must contain only valid technology names for ", names(allowed_strings)[[i]], ", but has additional elements %s")

        assert_subset(
          x = technologies[idxs],
          choices = allowed_strings[[i]],
          msg = msg,
          any.missing = any.missing,
          .var.name = .var.name,
          add = add
        )
        names(allowed_strings)[[i]]
      },
      FUN.VALUE = character(1)
    )

    invisible()
  }

