# Validate an intermediate scenario object

This function validates that an object is a valid intermediate scenario
dataset (e.g. `weo_2022`, `geco_2022`, or more generally,
`publication_YYYY`).

## Usage

``` r
validate_intermediate_scenario_output(data)
```

## Arguments

- data:

  An object (typically a data frame)

## Value

`TRUE` if the object is valid, otherwise an error with a message
explaining the failed assertions
