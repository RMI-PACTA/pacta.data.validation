assert_valid_indicator_for_sector_scenario_prep <-
  function(indicators, sectors, any.missing = FALSE, .var.name = checkmate::vname(indicators), add = NULL) {
    allowed_strings <-
      list(
        "Automotive" =
          c(
            "Sales"
          ),
        "Aviation" =
          c(
            "Emission Intensity"
          ),
        "Cement" =
          c(
            "Emission Intensity"
          ),
        "Coal" =
          c(
            "Production",
            "Supply"
          ),
        "HDV" =
          c(
            "Sales"
          ),
        "Oil&Gas" =
          c(
            "Production",
            "Supply"
          ),
        "Power" =
          c(
            "Capacity",
            "Capacity: installed"
          ),
        "Steel" =
          c(
            "Emission Intensity"
          )
      )

    vapply(
      X = seq_along(allowed_strings),
      FUN = function(i) {
        idxs <- sectors == names(allowed_strings)[[i]]
        msg <- paste0("must contain only valid indicator names for ", names(allowed_strings)[[i]], ", but has additional elements %s")

        assert_subset(
          x = indicators[idxs],
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

