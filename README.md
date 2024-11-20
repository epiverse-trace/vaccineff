
## *vaccineff*: An R package with tools for estimating vaccine effectiveness and vaccine related metrics <img src="man/figures/logo.png" align="right" width="130"/>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![R-CMD-check](https://github.com/ErikaCantor/vaccineff/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ErikaCantor/vaccineff/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ErikaCantor/vaccineff/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ErikaCantor/vaccineff?branch=main)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version/vaccineff)](https://CRAN.R-project.org/package=vaccineff)
[![lifecycle-experimental](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-experimental.svg)](https://www.reconverse.org/lifecycle.html#concept)

<!-- badges: end -->

*vaccineff* is developed at [Pontificia Universidad
Javeriana](https://www.javeriana.edu.co/inicio) as part of the
[Epiverse-TRACE initiative](https://data.org/initiatives/epiverse/).

*vaccineff* is an R package that offers tools for estimating vaccine
effectiveness, using a series of epidemiological designs including
cohort studies, test-negative case-control, and screening methods
([Halloran, Longini, and Struchiner 2010](#ref-bookvaccine)). The
current version of the package provides a set of features for preparing,
visualizing, and managing cohort data, estimating vaccine effectiveness,
and assessing the performance of the models. Test-negative design and
screening method will be included in future versions.

## Installation

The current development version of *vaccineff* can be installed from
[GitHub](https://github.com/) using the `pak` package.

``` r
if(!require("pak")) install.packages("pak")
pak::pak("ErikaCantor/vaccineff")
```

## Quick start

*vaccineff* provides a minimal cohort dataset that can be used to test
out the models.

``` r
# Load example `cohortdata` included in the package
data("cohortdata")
head(cohortdata, 5)
#>         id sex age death_date death_other_causes vaccine_date_1 vaccine_date_2
#> 1 afade1b2   F  37       <NA>               <NA>           <NA>           <NA>
#> 2 556c8c76   M  19       <NA>               <NA>           <NA>           <NA>
#> 3 04edf85a   M  50       <NA>               <NA>           <NA>           <NA>
#> 4 7e51a18e   F   8       <NA>               <NA>           <NA>           <NA>
#> 5 c5a83f56   M  66       <NA>               <NA>           <NA>           <NA>
#>   vaccine_1 vaccine_2
#> 1      <NA>      <NA>
#> 2      <NA>      <NA>
#> 3      <NA>      <NA>
#> 4      <NA>      <NA>
#> 5      <NA>      <NA>
```

## Package vignettes

More details on how to use *vaccineff* can be found in the [online
documentation as package
vignettes](https://epiverse-trace.github.io/vaccineff/), in the article
“Get Started with vaccineff”.

## Help

To report a bug or to request a new feature please open an
[issue](https://github.com/ErikaCantor/vaccineff/issues/new/choose).

## Contribute

Contributions to *vaccineff* are welcomed. Contributions are welcome via
[pull requests](https://github.com/%7B%7B%20gh_repo%20%7D%7D/pulls).

Contributors to the project include:

**Authors**: [David Santiago
Quevedo](https://github.com/davidsantiagoquevedo) and [Zulma M.
Cucunubá](https://github.com/zmcucunuba) (maintainer)

**Contributors**: [Geraldine Gómez](https://github.com/GeraldineGomez),
[Pratik Gupte](https://github.com/pratikunterwegs), [Érika J.
Cantor](https://github.com/ErikaCantor), [Santiago
Loaiza](https://github.com/santilo9513), [Jaime A.
Pavlich-Mariscal](https://github.com/jpavlich), [Hugo
Gruson](https://github.com/Bisaloo), [Chris
Hartgerink](https://github.com/chartgerink), [Felipe Segundo
Abril-Bermúdez](https://github.com/fsabrilb)

## Code of conduct

Please note that the *vaccineff* project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-bookvaccine" class="csl-entry">

Halloran, Elizabeth, Ira Longini, and Claudio Struchiner. 2010. *Design
and Analysis of Vaccine Studies*. Springer.

</div>

</div>
