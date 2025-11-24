# Create an example `masterdata_debt_datastore` object

This function creates an example `masterdata_debt_datastore` object.

## Usage

``` r
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
```

## Arguments

- id:

  value/s to be used for the `id` column

- id_name:

  value/s to be used for the `id_name` column

- ald_sector:

  value/s to be used for the `ald_sector` column

- ald_location:

  value/s to be used for the `ald_location` column

- technology:

  value/s to be used for the `technology` column

- year:

  value/s to be used for the `year` column

- country_of_domicile:

  value/s to be used for the `country_of_domicile` column

- ald_production_unit:

  value/s to be used for the `ald_production_unit` column

- ald_production:

  value/s to be used for the `ald_production` column

- ald_emissions_factor_unit:

  value/s to be used for the `ald_emissions_factor_unit` column

- ald_emissions_factor:

  value/s to be used for the `ald_emissions_factor` column

## Value

A data frame with the specified columns and/or their default values
