assert_valid_isin <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    .var.name <- .var.name # force evaluation before x is changed
    x <- simplify_if_one_col_df(x)

    msg <- "must contain only valid ISINs, but has additional elements %s"

    results <- is_valid_isin(x)

    if (any.missing) { results[is.na(x)] <- TRUE }

    res <-
      if (any(!results)) {
        sprintf(
          msg,
          set_collapse(x[!results])
        )
      } else {
        TRUE
      }

    checkmate::makeAssertion(x, res, .var.name, add)
  }
