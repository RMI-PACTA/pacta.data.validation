% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fake_financial_data.R
\name{fake_financial_data}
\alias{fake_financial_data}
\title{Create an example \code{financial_data} object}
\usage{
fake_financial_data(
  isin = "US3140KKGV04",
  unit_share_price = 12.3,
  current_shares_outstanding_all_classes = 333000,
  asset_type = "Equity",
  factset_entity_id = "000Y86-E"
)
}
\arguments{
\item{isin}{value/s to be used for the \code{isin} column}

\item{unit_share_price}{value/s to be used for the \code{unit_share_price} column}

\item{current_shares_outstanding_all_classes}{value/s to be used for the
\code{current_shares_outstanding_all_classes} column}

\item{asset_type}{value/s to be used for the \code{asset_type} column}

\item{factset_entity_id}{value/s to be used for the \code{factset_entity_id}
column}
}
\value{
A data frame with the specified columns and/or their default values
}
\description{
This function creates an example \code{financial_data} object.
}
