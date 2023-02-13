matches_regex <-
  function(x, regex) {
    x <- simplify_if_one_col_df(x)

    x_factored <- factor(x)
    matched <- grepl(regex, levels(x_factored))
    output <- matched[as.numeric(x_factored)]
    output[is.na(output)] <- FALSE
    output
  }
