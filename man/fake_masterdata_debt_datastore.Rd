% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fake_masterdata_debt_datastore.R
\name{fake_masterdata_debt_datastore}
\alias{fake_masterdata_debt_datastore}
\title{Create an example \code{masterdata_debt_datastore} object}
\usage{
fake_masterdata_debt_datastore(
  id = "8",
  id_name = "credit_parent_ar_company_id",
  ald_sector = "Oil&Gas",
  ald_location = "DE",
  technology = "Gas",
  year = 2022,
  country_of_domicile = "DE",
  ald_production_unit = "GJ",
  ald_production = 52281230,
  ald_emissions_factor_unit = "tonnes of CO2 per GJ",
  ald_emissions_factor = 0.06202439
)
}
\arguments{
\item{id}{value/s to be used for the \code{id} column}

\item{id_name}{value/s to be used for the \code{id_name} column}

\item{ald_sector}{value/s to be used for the \code{ald_sector} column}

\item{ald_location}{value/s to be used for the \code{ald_location} column}

\item{technology}{value/s to be used for the \code{technology} column}

\item{year}{value/s to be used for the \code{year} column}

\item{country_of_domicile}{value/s to be used for the \code{country_of_domicile}
column}

\item{ald_production_unit}{value/s to be used for the \code{ald_production_unit}
column}

\item{ald_production}{value/s to be used for the \code{ald_production} column}

\item{ald_emissions_factor_unit}{value/s to be used for the
\code{ald_emissions_factor_unit} column}

\item{ald_emissions_factor}{value/s to be used for the \code{ald_emissions_factor}
column}
}
\value{
A data frame with the specified columns and/or their default values
}
\description{
This function creates an example \code{masterdata_debt_datastore} object.
}
