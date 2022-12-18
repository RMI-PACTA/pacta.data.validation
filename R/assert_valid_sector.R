assert_valid_sector <-
  function(x, allow.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "Automotive",
        "Aviation",
        "Cement",
        "Coal",
        "HDV",
        "Oil&Gas",
        "Power",
        "Shipping",
        "Steel"
      )

    msg <- "must contain only valid sector names, but has additional elements %s"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      allow.missing = allow.missing,
      .var.name = .var.name,
      add = add
    )
  }
