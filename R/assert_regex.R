assert_regex <-
  function(x, regex, msg, any.missing = FALSE, .var.name = checkmate::vname(x),
           add = NULL) {
    .var.name <- .var.name # force evaluation before x is changed
    x <- simplify_if_one_col_df(x)

    if (missing(msg)) {
      msg <- paste0("all values must match regex `", regex, "`, but has additional values %s")
    }

    matches <- match_regex(x = x, regex = regex)

    if (any.missing) { matches[is.na(x)] <- TRUE }

    res <-
      if (any(!matches)) {
        sprintf(
          msg,
          set_collapse(x[!matches])
        )
      } else {
        TRUE
      }

    checkmate::makeAssertion(x, res, .var.name, add)
  }
