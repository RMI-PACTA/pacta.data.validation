assert_valid_sector <-
  function(x, allow.other = FALSE, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
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

    msg <- "must contain only valid sector names, but has additional element{?s} {.val {misses}}"

    if (allow.other) {
      allowed_strings <- c(allowed_strings, "Other")
    }

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }

assert_valid_sector_ai <-
  function(x, allow.other = FALSE, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "Aviation",
        "Cement",
        "Coal",
        "HDV",
        "LDV",
        "Oil&Gas",
        "Power",
        "Shipping",
        "Steel"
      )

    msg <- "must contain only valid sector names, but has additional element{?s} {.val {misses}}"

    if (allow.other) {
      allowed_strings <- c(allowed_strings, "Other")
    }

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
