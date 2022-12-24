assert_subset <-
  function(x, choices, msg, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    if (any.missing) choices <- c(choices, NA)
    x_levels <- levels(factor(x))
    matches <- match(x_levels, choices)
    res <-
      if (checkmate::anyMissing(matches)) {
        sprintf(
          msg,
          set_collapse(x_levels[is.na(matches)])
        )
      } else {
        TRUE
      }
    checkmate::makeAssertion(x, res, .var.name, add)
  }
