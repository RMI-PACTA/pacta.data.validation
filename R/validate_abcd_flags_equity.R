#' Validate a `abcd_flags_equity` object
#'
#' This function validates that an object is a valid `abcd_flags_equity` dataset.
#'
#' @param data An object (typically a data frame)
#'
#' @return `TRUE` if the object is valid, otherwise an error with a message
#'   explaining the failed assertions
#'
#' @export

validate_abcd_flags_equity <-
  function(data) {
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_data_frame(data, add = coll)

    if (checkmate::test_data_frame(data)) {
      checkmate::assert_false(dplyr::is_grouped_df(data), add = coll)

      assert_columns_exists(
        data,
        col_names = c(
          "isin",
          "has_asset_level_data",
          "has_ald_in_fin_sector",
          "sectors_with_assets"
        ),
        add = coll
      )

      # `isin` column
      col_name <- "isin"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_isin(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `has_asset_level_data` column
      col_name <- "has_asset_level_data"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_logical(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `has_ald_in_fin_sector` column
      col_name <- "has_ald_in_fin_sector"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_logical(data[[col_name]], any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `sectors_with_assets` column
      col_name <- "sectors_with_assets"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        assert_valid_sectors_with_assets(data[[col_name]], any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }
    }

    checkmate::reportAssertions(coll)
  }
