
## *vaccineff*: An R package with tools for estimating vaccine effectiveness and vaccine related metrics <img src="man/figures/vaccineff.png" align="right" width="130"/>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![R-CMD-check](https://github.com/epiverse-trace/vaccineff/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/vaccineff/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/vaccineff/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/vaccineff?branch=main)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version/vaccineff)](https://CRAN.R-project.org/package=vaccineff)
[![lifecycle-experimental](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-experimental.svg)](https://www.reconverse.org/lifecycle.html#concept)

<!-- badges: end -->

*vaccineff* is an R package that offers tools for estimating the vaccine
effectiveness, using a series of epidemiological designs (including
cohort studies, test-negative case-control, and screening methods). The
package provides a set of features for preparing the data, to analyze
different study designs and for assessing the performance of the models.

*vaccineff* is developed at [Pontificia Universidad
Javeriana](https://www.javeriana.edu.co/inicio) as part of the
[Epiverse-TRACE initiative](https://data.org/initiatives/epiverse/).

## Installation

The current development version of *vaccineff* can be installed from
[GitHub](https://github.com/) using the `pak` package.

``` r
if(!require("pak")) install.packages("pak")
pak::pak("epiverse-trace/vaccineff")
```

## Quick start

*vaccineff* provides minimal datasets that can be used to test out each
design `(cohortdata, testnegdata, screeningdata)`

``` r
# Load example `cohortdata` included in the package
data("cohortdata")
head(cohortdata, 5)
#>   sex age death_date vaccine_date_1 vaccine_date_2 vaccine_1 vaccine_2
#> 1   M  68       <NA>     2021-07-27     2021-12-01    BRAND2    BRAND2
#> 2   F  26       <NA>           <NA>           <NA>      <NA>      <NA>
#> 3   M  42       <NA>           <NA>           <NA>      <NA>      <NA>
#> 4   M  23       <NA>           <NA>           <NA>      <NA>      <NA>
#> 5   M  51       <NA>     2021-07-09     2021-08-10    BRAND2    BRAND2
```

## Package vignettes

More details on how to use *vaccineff* can be found in the [online
documentation as package
vignettes](https://epiverse-trace.github.io/vaccineff/), in the article
“Get Started with vaccineff”.

## Help

To report a bug or to request a new feature please open an
[issue](https://github.com/epiverse-trace/vaccineff/issues/new/choose).

## Contribute

Contributions to *vaccineff* are welcomed. Please follow the [package
contributing
guide](https://github.com/epiverse-trace/vaccineff/blob/main/.github/CONTRIBUTING.md).

## Code of conduct

Please note that the *vaccineff* project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Contributions

Contributors to the project include:

- [David Santiago Quevedo](https://github.com/davidsantiagoquevedo)
  (author)

- [Zulma M. Cucunubá](https://github.com/zmcucunuba) (author)

- [Santiago Loaiza](https://github.com/santilo9513) (author)

- [Geraldine Gómez](https://github.com/GeraldineGomez) (contributor)

- [Jaime A. Pavlich-Mariscal](https://github.com/jpavlich) (contributor)
