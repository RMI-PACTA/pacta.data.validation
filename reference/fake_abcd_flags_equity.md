# Create an example `abcd_flags_equity` object

This function creates an example `abcd_flags_equity` object.

## Usage

``` r
fake_abcd_flags_equity(
  isin = "US3140KKGV04",
  has_asset_level_data = TRUE,
  has_ald_in_fin_sector = TRUE,
  sectors_with_assets = "Power + Oil&Gas"
)
```

## Arguments

- isin:

  value/s to be used for the `isin` column

- has_asset_level_data:

  value/s to be used for the `has_asset_level_data` column

- has_ald_in_fin_sector:

  value/s to be used for the `has_ald_in_fin_sector` column

- sectors_with_assets:

  value/s to be used for the `sectors_with_assets` column

## Value

A data frame with the specified columns and/or their default values
