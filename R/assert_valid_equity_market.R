assert_valid_equity_market <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    allowed_strings <-
      c(
        "DevelopedMarket",
        "EmergingMarket",
        "GlobalMarket",
        "USMarket"
      )

    msg <- "must contain only valid equity market names, but has additional element{?s} {.val {misses}}"

    assert_subset(
      x = x,
      choices = allowed_strings,
      msg = msg,
      any.missing = any.missing,
      .var.name = .var.name,
      add = add
    )
  }
