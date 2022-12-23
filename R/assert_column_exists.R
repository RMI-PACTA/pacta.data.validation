assert_column_exists <-
  function(x, col_name, .var.name = checkmate::vname(x), add = NULL) {
    msg <- paste0('column "', col_name, '" must exist')

    res <-
      if (all(col_name %in% names(x))) {
        msg
      } else {
        TRUE
      }

    checkmate::makeAssertion(x, res, .var.name, add)
  }
