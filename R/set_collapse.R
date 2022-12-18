set_collapse <-
  function(x) {
    paste0('"', paste0(unique(x), collapse = '","'), '"')
  }
