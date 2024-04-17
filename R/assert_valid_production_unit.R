assert_valid_production_unit <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "DWT km",
        "dwt km",
        "GJ",
        "MW",
        "pkm",
        "tkm",
        "tonnes of cement",
        "t cement",
        "tonnes of coal",
        "t coal",
        "tonnes of steel",
        "t steel",
        "vehicles produced",
        "# vehicles"
      )

    msg <- "must contain only valid production units, but has additional element{?s} {.val {misses}}"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
