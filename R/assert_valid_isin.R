assert_valid_isin <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    .var.name <- .var.name # force evaluation before x is changed
    x <- simplify_if_one_col_df(x)

    msg <- "must contain only valid ISINs, but has additional element{?s} {.val {misses}}"

    results <- is_valid_isin(x)

    if (any.missing) { results[is.na(x)] <- TRUE }

    res <-
      if (any(!results)) {
        misses <- as.character(x[!results])
        cli::format_inline(msg)
      } else {
        TRUE
      }

    checkmate::makeAssertion(x, res, .var.name, add)
  }
