assert_columns_exists <-
  function(x, col_names, .var.name = checkmate::vname(x), add = NULL) {
    missing_cols <- col_names[!col_names %in% names(x)]
    res <-
      if (length(missing_cols) > 0) {
        cli::format_inline("column{?s} {.val {missing_cols}} must exist")
      } else {
        TRUE
      }

    checkmate::makeAssertion(x, res, .var.name, add)
  }
