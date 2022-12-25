assert_subset <-
  function(x, choices, msg, any.missing = FALSE, .var.name = checkmate::vname(x), add = NULL) {
    if (any.missing) choices <- c(choices, NA)

    if (missing(msg)) {
      msg <- "all values must be one of {.val {choices}}, but has additional value{?s} {.val {misses}}"
    }

    x_levels <- levels(factor(x))
    matches <- match(x_levels, choices)
    misses <- x_levels[is.na(matches)]

    res <-
      if (length(misses) > 0) {
        cli::format_inline(msg)
      } else {
        TRUE
      }

    checkmate::makeAssertion(x, res, .var.name, add)
  }
