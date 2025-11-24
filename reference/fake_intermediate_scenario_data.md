# Create an example `intermediate_scenario_data` object

This function creates an example `intermediate_scenario_data` object.

## Usage

``` r
fake_intermediate_scenario_data(
  source = "WEO2022",
  scenario = "NZE_2050",
  scenario_geography = "Global",
  sector = "Power",
  technology = "RenewablesCap",
  indicator = "Capacity: installed",
  units = "GW",
  year = 2025,
  value = 500
)
```

## Arguments

- source:

  value/s to be used for the `source` column

- scenario:

  value/s to be used for the `scenario` column

- scenario_geography:

  value/s to be used for the `scenario_geography` column

- sector:

  value/s to be used for the `sector` column

- technology:

  value/s to be used for the `technology` column

- indicator:

  value/s to be used for the `indicator` column

- units:

  value/s to be used for the `units` column

- year:

  value/s to be used for the `year` column

- value:

  value/s to be used for the `value` column

## Value

A data frame with the specified columns and/or their default values
