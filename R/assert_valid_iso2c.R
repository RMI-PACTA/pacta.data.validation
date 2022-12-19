assert_valid_iso2c <-
  function(x, allow.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        sort(unique(countrycode::codelist$iso2c)),
        "XK"
      )

    msg <- 'must contain only valid iso2c country codes or "XK" (for Kosovo), but has additional elements %s'

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      allow.missing = allow.missing,
      .var.name = .var.name,
      add = add
    )
  }