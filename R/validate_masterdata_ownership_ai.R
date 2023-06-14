#' Validate a `masterdata_ownership_ai` object
#'
#' This function validates that an object is a valid `masterdata_ownership_ai`
#' dataset.
#'
#' @param data An object (typically a data frame)
#'
#' @return `TRUE` if the object is valid, otherwise an error with a message
#'   explaining the failed assertions
#'
#' @export

validate_masterdata_ownership_datastore_ai <-
  function(data) {
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_data_frame(data, add = coll)

    if (checkmate::test_data_frame(data)) {
      checkmate::assert_false(dplyr::is_grouped_df(data), add = coll)

      assert_columns_exists(
        data,
        col_names = c(
          "company_id",
          "company_name",
          "is_ultimate_parent",
          "is_ultimate_listed_parent",
          "has_financial_data",
          "sector",
          "technology",
          "technology_type",
          "asset_country",
          "emissions_factor",
          "emissions_factor_unit",
          "metric",
          "unit",
          c(paste0("_", c(2020:2030))),
          "asset_level_timestamp"
        ),
        add = coll
      )

      # `company_id` column
      col_name <- "company_id"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_integerish(data[[col_name]], lower = 1, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `company_name` column
      col_name <- "company_name"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
      }

      # `is_ultimate_parent` column
      col_name <- "is_ultimate_parent"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_logical(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `is_ultimate_listed_parent` column
      col_name <- "is_ultimate_listed_parent"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_logical(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `has_financial_data` column
      col_name <- "has_financial_data"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_logical(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `sector` column
      col_name <- "sector"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_sector_ai(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `technology` column
      col_name <- "technology"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_technology_ai(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `technology_type` column
      col_name <- "technology_type"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = TRUE, all.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_technology_type_ai(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `asset_country` column
      col_name <- "asset_country"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
        assert_valid_iso2c(data[[col_name]], any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `emissions_factor` column
      col_name <- "emissions_factor"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_numeric(data[[col_name]], lower = 0, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `emissions_factor_unit` column
      col_name <- "emissions_factor_unit"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_emissions_factor_unit_ai(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `metric` column
      col_name <- "metric"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], fixed = "company level production", any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `_20XX` columns
      col_name <- c(paste0("_", c(2020:2030)))
      for (i in col_name) {
        if (checkmate::test_names(names(data), must.include = i)) {
          checkmate::assert_numeric(data[[i]], lower = 0, any.missing = TRUE, all.missing = FALSE, add = coll, .var.name = paste0("data$", i))
        }
      }

      # `unit` column
      col_name <- "unit"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_production_unit_ai(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `asset_level_timestamp` column
      col_name <- "asset_level_timestamp"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], pattern = "^20[1-3][0-9]Q[1-4]$", any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # valid `technology` for `sector`
      if (all(c("technology", "sector") %in% names(data))) {
        assert_valid_technology_for_sector_ai(data$technology, data$sector, add = coll)
      }

      # valid `technology_type` for `technology`
      if (all(c("technology_type", "technology") %in% names(data))) {
        assert_valid_technology_type_for_technology_ai(data$technology_type, data$technology, add = coll)
      }
    }

    checkmate::reportAssertions(coll)
  }
