set_collapse <-
  function(x) {
    na_msg <- NULL
    if (any(is.na(x))) { na_msg <- "`<NA>`" }

    str_msg <- NULL
    x <- sort(unique(x), na.last = NA)
    if (length(x) > 0) { str_msg <- paste0('"', x, '"') }

    paste0(c(na_msg, str_msg), collapse = ", ")
  }
