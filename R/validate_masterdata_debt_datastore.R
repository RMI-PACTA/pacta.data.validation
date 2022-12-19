#' Validate a `masterdata_debt_datastore` object
#'
#' This function validates that an object is a valid `masterdata_debt_datastore`
#' dataset.
#'
#' @param data An object (typically a data frame)
#'
#' @return `TRUE` if the object is valid, otherwise an error with a message
#'   explaining the failed assertions
#'
#' @export

validate_masterdata_debt_datastore <-
  function(data) {
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_data_frame(data, add = coll)

    if (checkmate::test_data_frame(data)) {
      checkmate::assert_false(dplyr::is_grouped_df(data), add = coll)

      # `id` column
      col_name <- "id"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, pattern = "^[0-9]+$", add = coll, .var.name = paste0("data$", col_name))
      }

      # `id_name` column
      col_name <- "id_name"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
      }

      # `ald_sector` column
      col_name <- "ald_sector"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_sector(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `ald_location` column
      col_name <- "ald_location"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
        assert_valid_iso2c(data[[col_name]], allow.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `technology` column
      col_name <- "technology"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_technology(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `year` column
      col_name <- "year"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_integerish(data[[col_name]], lower = 2000, upper = 2100, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `ald_production` column
      col_name <- "ald_production"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_numeric(data[[col_name]], lower = 0, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `ald_production_unit` column
      col_name <- "ald_production_unit"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_production_unit(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `ald_emissions_factor` column
      col_name <- "ald_emissions_factor"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_numeric(data[[col_name]], lower = 0, add = coll, .var.name = paste0("data$", col_name))
      }

      # `ald_emissions_factor_unit` column
      col_name <- "ald_emissions_factor_unit"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_emissions_factor_unit(data[[col_name]], allow.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `country_of_domicile` column
      col_name <- "country_of_domicile"
      checkmate::assert_names(names(data), must.include = col_name, add = coll)
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
        assert_valid_iso2c(data[[col_name]], allow.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # valid `technology` for `ald_sector`
      if (all(c("technology", "ald_sector") %in% names(data))) {
        assert_valid_technology_for_sector(data$technology, data$ald_sector, add = coll)
      }
    }

    checkmate::reportAssertions(coll)
  }
