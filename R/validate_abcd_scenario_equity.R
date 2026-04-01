#' Validate a `abcd_scenario_equity` object
#'
#' This function validates that an object is a valid `abcd_scenario_equity` dataset.
#'
#' @param data An object (typically a data frame)
#'
#' @return `TRUE` if the object is valid, otherwise an error with a message
#'   explaining the failed assertions
#'
#' @export

validate_abcd_scenario_equity <-
  function(data) {
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_data_frame(data, add = coll)

    if (checkmate::test_data_frame(data)) {
      checkmate::assert_false(dplyr::is_grouped_df(data), add = coll)

      assert_columns_exists(
        data,
        col_names = c(
          "scenario_source",
          "scenario",
          "id_name",
          "id",
          "ald_company_sector_id",
          "equity_market",
          "scenario_geography",
          "ald_sector",
          "technology",
          "year",
          "plan_tech_prod",
          "plan_br_wt_factor",
          "plan_br_wt_techshare",
          "plan_emission_factor",
          "scen_tech_prod",
          "scen_br_wt_factor",
          "scen_br_wt_techshare",
          "scen_emission_factor",
          "current_plan_row",
          "scenario_exists"),
        add = coll
      )

      # `scenario_source` column
      col_name <- "scenario_source"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scenario` column
      col_name <- "scenario"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `id_name` column
      col_name <- "id_name"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `id` column
      col_name <- "id"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        assert_valid_ai_company_id(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `ald_company_sector_id` column
      col_name <- "ald_company_sector_id"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_character(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `equity_market` column
      col_name <- "equity_market"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        assert_valid_equity_market(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scenario_geography` column
      col_name <- "scenario_geography"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        assert_valid_scenario_geography(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `ald_sector` column
      col_name <- "ald_sector"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        assert_valid_sector(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `technology` column
      col_name <- "technology"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        assert_valid_technology(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
        assert_valid_technology_for_sector(data[[col_name]], data[["ald_sector"]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `year` column
      col_name <- "year"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_integerish(data[[col_name]], any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `plan_tech_prod` column
      col_name <- "plan_tech_prod"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_double(data[[col_name]], lower = 0, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `plan_br_wt_factor` column
      col_name <- "plan_br_wt_factor"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_double(data[[col_name]], lower = 0, any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `plan_br_wt_techshare` column
      col_name <- "plan_br_wt_techshare"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_double(data[[col_name]], lower = 0, any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `plan_emission_factor` column
      col_name <- "plan_emission_factor"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_double(data[[col_name]], lower = 0, any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scen_tech_prod` column
      col_name <- "scen_tech_prod"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_double(data[[col_name]], lower = 0, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scen_br_wt_factor` column
      col_name <- "scen_br_wt_factor"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_double(data[[col_name]], lower = 0, any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scen_br_wt_techshare` column
      col_name <- "scen_br_wt_techshare"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_double(data[[col_name]], lower = 0, any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scen_emission_factor` column
      col_name <- "scen_emission_factor"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_double(data[[col_name]], lower = 0, any.missing = TRUE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `current_plan_row` column
      col_name <- "current_plan_row"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_integerish(data[[col_name]], lower = 0, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }

      # `scenario_exists` column
      col_name <- "scenario_exists"
      if (checkmate::test_names(names(data), must.include = col_name)) {
        checkmate::assert_integerish(data[[col_name]], lower = 0, upper = 1, any.missing = FALSE, add = coll, .var.name = paste0("data$", col_name))
      }
    }

    checkmate::reportAssertions(coll)
  }
