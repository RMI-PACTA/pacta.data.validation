set_collapse <-
  function(x) {
    nas <- is.na(x)
    has_na <- any(nas)
    x <- sort(unique(x))
    paste0(ifelse(has_na, "`<NA>`, ", ""), '"', paste0(x, collapse = '", "'), '"')
  }
