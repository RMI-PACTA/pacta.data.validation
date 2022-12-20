#' Create an example `currencies` object
#'
#' This function creates an example `currencies` object.
#'
#' @param currency value/s to be used for the `currency` column
#' @param exchange_rate value/s to be used for the `exchange_rate` column
#'
#' @return A data frame with the specified columns and/or their default values
#'
#' @export

fake_currencies <-
  function(currency = "USD", exchange_rate = 1) {
    data.frame(
      currency = currency,
      exchange_rate = exchange_rate
    )
  }
