#' Validate an intermediate scenario object
#'
#' This function validates that an object is a valid intermediate scenario
#' dataset (e.g. `weo_2022`, `geco_2022`, or more generally, `publication_YYYY`).
#'
#' @param data An object (typically a data frame)
#'
#' @return `TRUE` if the object is valid, otherwise an error with a message
#'   explaining the failed assertions
#'
#' @export

validate_intermediate_scenario_output <-
  function(data) {
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_data_frame(data, add = coll)

    if (checkmate::test_data_frame(data)) {
      checkmate::assert_false(dplyr::is_grouped_df(data), add = coll)

      assert_columns_exists(
        data,
        col_names = c(
          "source",
          "scenario",
          "scenario_geography",
          "sector",
          "technology",
          "indicator",
          "units",
          "year",
          "value"
        ),
        add = coll
      )

      # `source` column
      col_name <- "source"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scenario` column
      col_name <- "scenario"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scenario_geography` column
      col_name <- "scenario_geography"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, min.chars = 1L, add = coll, .var.name = paste0("data$", col_name))
      }

      # `sector` column
      col_name <- "sector"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_sector_scenario_prep(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `technology` column
      col_name <- "technology"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_technology_scenario_prep(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `indicator` column
      col_name <- "indicator"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        # TODO: standardize indicators by sector across scenarios
        assert_valid_indicator(data[[col_name]], add = coll, .var.name = paste0("data$", col_name))
      }

      # `units` column
      col_name <- "units"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        # TODO: standardize units by sector across scenarios
        assert_valid_units(data[[col_name]], any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `year` column
      col_name <- "year"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_integerish(data[[col_name]], lower = 2000, upper = 2100, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `value` column
      col_name <- "value"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_numeric(data[[col_name]], lower = 0, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # valid `technology` for `sector`
      if (all(c("technology", "sector") %in% names(data))) {
        assert_valid_technology_for_sector_scenario_prep(data$technology, data$sector, add = coll)
      }

      # valid `indicator` for `sector`
      if (all(c("indicator", "sector") %in% names(data))) {
        assert_valid_indicator_for_sector_scenario_prep(data$indicator, data$sector, add = coll)
      }

      # TODO: standardize units to be used per sector
      # # valid `units` for `sector`
      # if (all(c("units", "sector") %in% names(data))) {
      #   assert_valid_unit_for_sector_scenario_prep(data$units, data$sector, add = coll)
      # }

      # valid `value` range for `units` in `sectors`
      if (all(c("value", "units", "sector") %in% names(data))) {
        assert_valid_value_range_for_unit_scenario_prep(data$value, data$units, data$sector, add = coll)
      }
    }

    checkmate::reportAssertions(coll)
  }
