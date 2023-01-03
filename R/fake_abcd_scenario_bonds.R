#' Create an example `abcd_scenario_bonds` object
#'
#' This function creates an example `abcd_scenario_bonds` object.
#'
#' @param scenario_source value/s to be used for the `scenario_source` column
#' @param scenario value/s to be used for the `scenario` column
#' @param id_name value/s to be used for the `id_name` column
#' @param id value/s to be used for the `id` column
#' @param ald_company_sector_id value/s to be used for the
#'   `ald_company_sector_id` column
#' @param equity_market value/s to be used for the `equity_market` column
#' @param scenario_geography value/s to be used for the `scenario_geography`
#'   column
#' @param ald_sector value/s to be used for the `ald_sector` column
#' @param technology value/s to be used for the `technology` column
#' @param year value/s to be used for the `year` column
#' @param plan_tech_prod value/s to be used for the `plan_tech_prod` column
#' @param plan_br_wt_factor value/s to be used for the `plan_br_wt_factor`
#'   column
#' @param plan_br_wt_techshare value/s to be used for the `plan_br_wt_techshare`
#'   column
#' @param plan_emission_factor value/s to be used for the `plan_emission_factor`
#'   column
#' @param scen_tech_prod value/s to be used for the `scen_tech_prod` column
#' @param scen_br_wt_factor value/s to be used for the `scen_br_wt_factor`
#'   column
#' @param scen_br_wt_techshare value/s to be used for the `scen_br_wt_techshare`
#'   column
#' @param scen_emission_factor value/s to be used for the `scen_emission_factor`
#'   column
#' @param current_plan_row value/s to be used for the `current_plan_row` column
#' @param scenario_exists value/s to be used for the `scenario_exists` column
#'
#' @return A data frame with the specified columns and/or their default values
#'
#' @export

fake_abcd_scenario_bonds <-
  function(scenario_source = "ISF2021",
           scenario = "NZE",
           id_name = "ar_company_id",
           id = "10015",
           ald_company_sector_id = "4",
           equity_market = "DevelopedMarket",
           scenario_geography = "Global",
           ald_sector = "Coal",
           technology = "Coal",
           year = 2021,
           plan_tech_prod = 0,
           plan_br_wt_factor = NA_real_,
           plan_br_wt_techshare = NA_real_,
           plan_emission_factor = NA_real_,
           scen_tech_prod = 0,
           scen_br_wt_factor = NA_real_,
           scen_br_wt_techshare = NA_real_,
           scen_emission_factor = 0,
           current_plan_row = 1,
           scenario_exists = 1) {
    `class<-`(
      data.frame(
        scenario_source = scenario_source,
        scenario = scenario,
        id_name = id_name,
        id = id,
        ald_company_sector_id = ald_company_sector_id,
        equity_market = equity_market,
        scenario_geography = scenario_geography,
        ald_sector = ald_sector,
        technology = technology,
        year = year,
        plan_tech_prod = plan_tech_prod,
        plan_br_wt_factor = plan_br_wt_factor,
        plan_br_wt_techshare = plan_br_wt_techshare,
        plan_emission_factor = plan_emission_factor,
        scen_tech_prod = scen_tech_prod,
        scen_br_wt_factor = scen_br_wt_factor,
        scen_br_wt_techshare = scen_br_wt_techshare,
        scen_emission_factor = scen_emission_factor,
        current_plan_row = current_plan_row,
        scenario_exists = scenario_exists
      ),
      c("tbl", "data.frame")
    )
  }
