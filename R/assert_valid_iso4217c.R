assert_valid_iso4217c <-
  function(x, allow.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <- sort(unique(countrycode::codelist$iso4217c))

    msg <- 'must contain only valid iso4217c currency codes, but has additional elements %s'

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      allow.missing = allow.missing,
      .var.name = .var.name,
      add = add
    )
  }