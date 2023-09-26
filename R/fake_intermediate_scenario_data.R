#' Create an example `intermediate_scenario_data` object
#'
#' This function creates an example `intermediate_scenario_data` object.
#'
#' @param source value/s to be used for the `source` column
#' @param scenario value/s to be used for the `scenario` column
#' @param scenario_geography value/s to be used for the `scenario_geography` column
#' @param sector value/s to be used for the `sector` column
#' @param technology value/s to be used for the `technology` column
#' @param indicator value/s to be used for the `indicator` column
#' @param units value/s to be used for the `units` column
#' @param year value/s to be used for the `year` column
#' @param value value/s to be used for the `value` column
#'
#' @return A data frame with the specified columns and/or their default values
#'
#' @export

fake_intermediate_scenario_data <-
  function(source = "WEO2022",
           scenario = "NZE_2050",
           scenario_geography = "Global",
           sector = "Power",
           technology = "RenewablesCap",
           indicator = "Capacity: installed",
           units = "GW",
           year = 2025,
           value = 500) {
    data.frame(
      source = source,
      scenario = scenario,
      scenario_geography = scenario_geography,
      sector = sector,
      technology = technology,
      indicator = indicator,
      units = units,
      year = year,
      value = value
    )
  }
