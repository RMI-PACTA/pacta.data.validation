# Create an example `financial_data` object

This function creates an example `financial_data` object.

## Usage

``` r
fake_financial_data(
  isin = "US3140KKGV04",
  unit_share_price = 12.3,
  current_shares_outstanding_all_classes = 333000,
  asset_type = "Equity",
  factset_entity_id = "000Y86-E"
)
```

## Arguments

- isin:

  value/s to be used for the `isin` column

- unit_share_price:

  value/s to be used for the `unit_share_price` column

- current_shares_outstanding_all_classes:

  value/s to be used for the `current_shares_outstanding_all_classes`
  column

- asset_type:

  value/s to be used for the `asset_type` column

- factset_entity_id:

  value/s to be used for the `factset_entity_id` column

## Value

A data frame with the specified columns and/or their default values
