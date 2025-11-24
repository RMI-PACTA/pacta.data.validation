# pacta.data.validation

The `pacta.data.validation` R package provides a number of utility
functions to facilitate validation of data objects used in the PACTA
ecosystem of R packages.

## Installation

You can install the most recent version of pacta.data.validation from
[R-universe](https://r-universe.dev/) with:

``` r
install.packages("pacta.data.validation", repos = "https://rmi-pacta.r-universe.dev")
```

You can install the development version of pacta.data.validation from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RMI-PACTA/pacta.data.validation")
```

## Usage

To validate a data object, pass it to the appropriate validation
function. For example…

``` r
library("pacta.data.validation")

masterdata_debt_datastore <- fake_masterdata_debt_datastore(id = "x")
validate_masterdata_debt_datastore(masterdata_debt_datastore)
```
