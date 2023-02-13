assert_valid_sectors_with_assets <-
  function(x, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    if (!is.character(x)) {
      return(checkmate::makeAssertion(x, "must be a character vector", .var.name, add))
    }
    x <- sort(unique(unlist(strsplit(unique(x), " + ", fixed = TRUE))))
    assert_valid_sector(x, any.missing = any.missing, .var.name = .var.name, add = add)
  }
