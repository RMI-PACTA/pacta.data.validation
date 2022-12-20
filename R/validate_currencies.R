#' Validate a `currencies` object
#'
#' This function validates that an object is a valid `currencies` dataset.
#'
#' @param data An object (typically a data frame)
#'
#' @return `TRUE` if the object is valid, otherwise an error with a message
#'   explaining the failed assertions
#'
#' @export

validate_currencies <-
  function(data) {
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_data_frame(data, add = coll)

    if (checkmate::test_data_frame(data)) {
      checkmate::assert_false(dplyr::is_grouped_df(data), add = coll)

      # `currency` column
      col_name <- "currency"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_iso4217c(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `exchange_rate` column
      col_name <- "exchange_rate"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_numeric(data[[col_name]], lower = 0, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }
    }

    checkmate::reportAssertions(coll)
  }
