#' Validate a `financial_data` object
#'
#' This function validates that an object is a valid `financial_data` dataset.
#'
#' @param data An object (typically a data frame)
#'
#' @return `TRUE` if the object is valid, otherwise an error with a message
#'   explaining the failed assertions
#'
#' @export

validate_financial_data <-
  function(data) {
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_data_frame(data, add = coll)

    if (checkmate::test_data_frame(data)) {
      checkmate::assert_false(dplyr::is_grouped_df(data), add = coll)

      assert_columns_exists(
        data,
        col_names = c(
          "isin",
          "unit_share_price",
          "current_shares_outstanding_all_classes",
          "asset_type",
          "factset_entity_id"
        ),
        add = coll
      )

      # `isin` column
      col_name <- "isin"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_isin(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `unit_share_price` column
      col_name <- "unit_share_price"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_numeric(data[[col_name]], any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `current_shares_outstanding_all_classes` column
      col_name <- "current_shares_outstanding_all_classes"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_numeric(data[[col_name]], any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `asset_type` column
      col_name <- "asset_type"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        assert_valid_asset_type(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `factset_entity_id` column
      col_name <- "factset_entity_id"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        assert_valid_factset_entity_id(data[[col_name]], any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }
    }

    checkmate::reportAssertions(coll)
  }
