#' Create an example `masterdata_ownership_datastore` object
#'
#' This function creates an example `masterdata_ownership_datastore` object.
#'
#' @param id value/s to be used for the `id` column
#' @param id_name value/s to be used for the `id_name` column
#' @param ald_sector value/s to be used for the `ald_sector` column
#' @param ald_location value/s to be used for the `ald_location` column
#' @param technology value/s to be used for the `technology` column
#' @param year value/s to be used for the `year` column
#' @param ald_production value/s to be used for the `ald_production` column
#' @param ald_production_unit value/s to be used for the `ald_production_unit`
#'   column
#' @param ald_emissions_factor value/s to be used for the `ald_emissions_factor`
#'   column
#' @param ald_emissions_factor_unit value/s to be used for the
#'   `ald_emissions_factor_unit` column
#' @param country_of_domicile value/s to be used for the `country_of_domicile`
#'   column
#'
#' @return A data frame with the specified columns and/or their default values
#'
#' @export

fake_masterdata_ownership_datastore <-
  function(id = "8",
           id_name = "ar_company_id",
           ald_sector = "Oil&Gas",
           ald_location = "DE",
           technology = "Gas",
           year = 2022,
           ald_production = 5.228123e+07,
           ald_production_unit = "GJ",
           ald_emissions_factor = 0.06202439,
           ald_emissions_factor_unit = "tonnes of CO2 per GJ",
           country_of_domicile = "DE") {
    data.frame(
      id = id,
      id_name = id_name,
      ald_sector = ald_sector,
      ald_location = ald_location,
      technology = technology,
      year = year,
      ald_production = ald_production,
      ald_production_unit = ald_production_unit,
      ald_emissions_factor = ald_emissions_factor,
      ald_emissions_factor_unit = ald_emissions_factor_unit,
      country_of_domicile = country_of_domicile
    )
  }
