assert_valid_technology_type_for_technology_ai <-
  function(technologies, sectors, any.missing = FALSE, .var.name = checkmate::vname(technologies), add = NULL) {
    allowed_strings <-
      list(
        "Anthracite Metallurgical" =
          c(
            "Surface",
            "Underground"
          ),
        "Anthracite Thermal" =
          c(
            "Surface",
            "Surface and Underground",
            "Underground"
          ),
        "Basic Oxygen Furnace" =
          c(
            "Integrated Blast Furnace",
            "Integrated DRI Furnace"
          ),
        "Bituminous Metallurgical" =
          c(
            "Surface",
            "Surface and Underground",
            "Underground"
          ),
        "Bituminous Thermal" =
          c(
            "Surface",
            "Surface and Underground",
            "Underground"
          ),
        "CoalCap" =
          c(
            NA_character_
          ),
        "Electric" =
          c(
            "Bus Chassis",
            "Full-sized Light Truck",
            "Heavy Freight Transport",
            "Luxury/Sports Car",
            "Medium Car",
            "Medium Freight Transport",
            "Mid-sized Light Truck",
            "Motor Home Chassis",
            "Small Car",
            "Small Light Truck"
          ),
        "Electric Arc Furnace" =
          c(
            "Integrated Blast Furnace",
            "Integrated DRI Furnace",
            "Integrated Open Hearth Furnace",
            "Mini-Mill"
          ),
        "Freight" =
          c(
            "Bulk Carrier",
            "Chemical Tanker",
            "Container",
            "General Cargo",
            "Liquefied Gas Tanker",
            "Long-Haul",
            "Medium-Haul",
            "Oil Tanker",
            "Other Liquids Tanker",
            "Refrigerated Bulk",
            "Ro-Ro",
            "Short-Haul"
          ),
        "Fuel Cell" =
          c(
            "Bus Chassis",
            "Full-sized Light Truck",
            "Heavy Freight Transport",
            "Luxury/Sports Car",
            "Medium Car",
            "Medium Freight Transport",
            "Mid-sized Light Truck",
            "Small Car",
            "Small Light Truck"
          ),
        "Gas" =
          c(
            "CBM",
            "Conventional Gas",
            "Deepwater Gas",
            "Unconventional Gas"
          ),
        "GasCap" =
          c(
            NA_character_
          ),
        "Hybrid No-Plug" =
          c(
            "Bus Chassis",
            "Full-sized Light Truck",
            "Heavy Freight Transport",
            "Luxury/Sports Car",
            "Medium Car",
            "Medium Freight Transport",
            "Mid-sized Light Truck",
            "Small Car",
            "Small Light Truck"
          ),
        "Hybrid Plug-In" =
          c(
            "Full-sized Light Truck",
            "Luxury/Sports Car",
            "Medium Car",
            "Mid-sized Light Truck",
            "Small Car",
            "Small Light Truck"
          ),
        "HydroCap" =
          c(
            NA_character_
          ),
        "ICE CNG" =
          c(
            "Bus Chassis",
            "Full-sized Light Truck",
            "Heavy Freight Transport",
            "Luxury/Sports Car",
            "Medium Car",
            "Medium Freight Transport",
            "Mid-sized Light Truck",
            "Small Car",
            "Small Light Truck"
          ),
        "ICE Diesel" =
          c(
            "Bus Chassis",
            "Full-sized Light Truck",
            "Heavy Freight Transport",
            "Luxury/Sports Car",
            "Medium Car",
            "Medium Freight Transport",
            "Mid-sized Light Truck",
            "Motor Home Chassis",
            "Small Car",
            "Small Light Truck"
          ),
        "ICE E85+" =
          c(
            "Medium Car",
            "Mid-sized Light Truck",
            "Small Car",
            "Small Light Truck"
          ),
        "ICE Gasoline" =
          c(
            "Bus Chassis",
            "Full-sized Light Truck",
            "Heavy Freight Transport",
            "Luxury/Sports Car",
            "Medium Car",
            "Medium Freight Transport",
            "Mid-sized Light Truck",
            "Motor Home Chassis",
            "Small Car",
            "Small Light Truck"
          ),
        "ICE Hydrogen" =
          c(
            "Heavy Freight Transport",
            "Medium Freight Transport"
          ),
        "ICE Propane" =
          c(
            "Bus Chassis",
            "Full-sized Light Truck",
            "Heavy Freight Transport",
            "Luxury/Sports Car",
            "Medium Car",
            "Medium Freight Transport",
            "Mid-sized Light Truck",
            "Small Car",
            "Small Light Truck"
          ),
        "Integrated Facility" =
          c(
            "Dry With Preheater And Precalciner",
            "Dry Without Preheater (Long Dry Kiln)",
            "Mixed Kiln Type",
            "Semi-Wet/Semi Dry",
            "Wet / Shaft Kiln"
          ),
        "Lignite Thermal" =
          c(
            "Surface",
            "Surface and Underground",
            "Underground"
          ),
        "Natural Gas Liquids" =
          c(
            "Conventional NGL",
            "Deepwater NGL",
            "Unconventional NGL"
          ),
        "NuclearCap" =
          c(
            NA_character_
          ),
        "Oil and Condensate" =
          c(
            "Conventional Oil",
            "Deepwater Oil",
            "Heavy Oil",
            "Oil Sands",
            "Unconventional Oil"
          ),
        "OilCap" =
          c(
            NA_character_
          ),
        "Open Hearth Furnace" =
          c(
            "Integrated Blast Furnace"
          ),
        "Passenger" =
          c(
            "Cruise",
            "Ferry-Pax Only",
            "Ferry-RoPax",
            "Long-Haul",
            "Medium-Haul",
            "Short-Haul"
          ),
        "RenewablesCap" =
          c(
            "Biogas",
            "Biomass",
            "Geothermal",
            "Ocean",
            "Solar CPV",
            "Solar CSP",
            "Solar PV",
            "Wind Offshore",
            "Wind Onshore"
          ),
        "Sub-Bituminous Thermal" =
          c(
            "Surface",
            "Surface and Underground",
            "Underground"
          )
      )

    vapply(
      X = seq_along(allowed_strings),
      FUN = function(i) {
        idxs <- sectors == names(allowed_strings)[[i]]
        msg <- paste0("must contain only valid technology_type names for ", names(allowed_strings)[[i]], ", but has additional elements %s")

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
