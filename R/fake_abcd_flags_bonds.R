#' Create an example `abcd_flags_bonds` object
#'
#' This function creates an example `abcd_flags_bonds` object.
#'
#' @param credit_parent_id value/s to be used for the `credit_parent_id` column
#' @param has_asset_level_data value/s to be used for the `has_asset_level_data`
#'   column
#' @param has_ald_in_fin_sector value/s to be used for the
#'   `has_ald_in_fin_sector` column
#' @param sectors_with_assets value/s to be used for the `sectors_with_assets`
#'   column
#'
#' @return A data frame with the specified columns and/or their default values
#'
#' @export

fake_abcd_flags_bonds <-
  function(credit_parent_id = "000BGD-E",
           has_asset_level_data = TRUE,
           has_ald_in_fin_sector = TRUE,
           sectors_with_assets = "Power + Oil&Gas") {
    `class<-`(
      data.frame(
        credit_parent_id = credit_parent_id,
        has_asset_level_data = has_asset_level_data,
        has_ald_in_fin_sector = has_ald_in_fin_sector,
        sectors_with_assets = sectors_with_assets
      ),
      c("tbl", "data.frame")
    )
  }
