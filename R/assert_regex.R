assert_regex <-
  function(x, regex, msg, any.missing = FALSE, .var.name = checkmate::vname(x),
           add = NULL) {
    .var.name <- .var.name # force evaluation before x is changed
    x <- simplify_if_one_col_df(x)

    if (missing(msg)) {
      msg <- "all values must match regex {.code {regex}}, but has additional value{?s} {.val {misses}}"
    }

    matches <- matches_regex(x = x, regex = regex)
    if (any.missing) { matches[is.na(x)] <- TRUE }

    res <-
      if (any(!matches)) {
        misses <- x[!matches]
        cli::format_inline(msg)
      } else {
        TRUE
      }

    checkmate::makeAssertion(x, res, .var.name, add)
  }
