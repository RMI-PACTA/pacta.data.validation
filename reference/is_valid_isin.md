# Validate a vector of ISINs

This function validates that a vector of ISINs are valid codes that
conform to the ISO 6166 specification with `TRUE` or `FALSE`. It checks
the basic structure (2 alpha characters, 9 alpha-numeric characters, 1
check digit) and also validates the check digit using the Luhn
algorithm.

## Usage

``` r
is_valid_isin(isins)
```

## Arguments

- isins:

  A character vector

## Value

A logical vector the same length as `isins`.
