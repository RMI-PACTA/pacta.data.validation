#' Create an example `financial_data` object
#'
#' This function creates an example `financial_data` object.
#'
#' @param isin value/s to be used for the `isin` column
#' @param unit_share_price value/s to be used for the `unit_share_price` column
#' @param current_shares_outstanding_all_classes value/s to be used for the
#'   `current_shares_outstanding_all_classes` column
#' @param asset_type value/s to be used for the `asset_type` column
#' @param factset_entity_id value/s to be used for the `factset_entity_id`
#'   column
#'
#' @return A data frame with the specified columns and/or their default values
#'
#' @export

fake_financial_data <-
  function(
    isin = "US3140KKGV04",
    unit_share_price = 12.3,
    current_shares_outstanding_all_classes = 333000,
    asset_type = "Equity",
    factset_entity_id = "000Y86-E"
  ) {
    data.frame(
      isin = isin,
      unit_share_price = unit_share_price,
      current_shares_outstanding_all_classes = current_shares_outstanding_all_classes,
      asset_type = asset_type,
      factset_entity_id = factset_entity_id
    )
  }
