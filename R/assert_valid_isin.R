assert_valid_isin <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    msg <- "must contain only valid ISINs, but has additional elements %s"

    if (any.missing) { x <- stats::na.omit(x) }

    results <- !is_valid_isin(x)

    res <-
      if (any(results)) {
        sprintf(
          msg,
          set_collapse(x[results])
        )
      } else {
        TRUE
      }
    checkmate::makeAssertion(x, res, .var.name, add)
  }
