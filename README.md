# pacta.data.validation <a href="https://rmi-pacta.github.io/pacta.data.validation"><img src="man/figures/logo.png" align="right" height="31" /></a>

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/RMI-PACTA/pacta.data.validation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.data.validation/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/RMI-PACTA/pacta.data.validation/graph/badge.svg)](https://codecov.io/gh/RMI-PACTA/pacta.data.validation)
[![CRAN status](https://www.r-pkg.org/badges/version/pacta.data.validation)](https://CRAN.R-project.org/package=pacta.data.validation)
[![pacta.data.validation status badge](https://rmi-pacta.r-universe.dev/badges/pacta.data.validation)](https://rmi-pacta.r-universe.dev/ui#package:pacta.data.validation)
<!-- badges: end -->

The `pacta.data.validation` R package provides a number of utility functions to facilitate validation of data objects used in the PACTA ecosystem of R packages.

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

To validate a data object, pass it to the appropriate validation function. For example...

``` r
library("pacta.data.validation")

masterdata_debt_datastore <- fake_masterdata_debt_datastore(id = "x")
validate_masterdata_debt_datastore(masterdata_debt_datastore)
```

