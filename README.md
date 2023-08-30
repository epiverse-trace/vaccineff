
## *{{ packagename }}*: An R package with tools for estimating vaccine effectiveness and vaccine related metrics <img src="man/figures/vaccineff.png" align="right" width="130"/>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![R-CMD-check](https://github.com/%7B%7B%20gh_repo%20%7D%7D/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/%7B%7B%20gh_repo%20%7D%7D/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/%7B%7B%20gh_repo%20%7D%7D/branch/main/graph/badge.svg)](https://app.codecov.io/gh/%7B%7B%20gh_repo%20%7D%7D?branch=main)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version/%7B%7B%20packagename%20%7D%7D)](https://CRAN.R-project.org/package=%7B%7B%20packagename%20%7D%7D)
[![lifecycle-experimental](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-experimental.svg)](https://www.reconverse.org/lifecycle.html#concept)

<!-- badges: end -->

*{{ packagename }}* is an R package that offers tools for estimating the
vaccine effectiveness, using a series of epidemiological designs
including cohort studies, test-negative case-control, and screening
methods ([Torvaldsen and McIntyre 2020](#ref-torvaldsen2020)). The
package provides a set of features for preparing the data, to analyze
different study designs and for assessing the performance of the models.

*{{ packagename }}* is developed at [Pontificia Universidad
Javeriana](https://www.javeriana.edu.co/inicio) as part of the
[Epiverse-TRACE initiative](https://data.org/initiatives/epiverse/).

## Installation

The current development version of *{{ packagename }}* can be installed
from [GitHub](https://github.com/) using the `pak` package.

``` r
if(!require("pak")) install.packages("pak")
pak::pak("{{ gh_repo }}")
```

## Quick start

*{{ packagename }}* provides minimal datasets that can be used to test
out each design `(cohortdata, testnegdata, screeningdata)`

``` r
# Load example `cohortdata` included in the package
data("cohortdata")
head(cohortdata, 5)
#>   sex age subsidy death_date vaccine_date_1 vaccine_date_2 vaccine_1 vaccine_2
#> 1   F   6       0       <NA>           <NA>           <NA>      <NA>      <NA>
#> 2   M  79       0       <NA>     2044-03-31     2044-05-07    BRAND2    BRAND2
#> 3   F  34       0       <NA>     2044-07-26     2044-09-03    BRAND2    BRAND2
#> 4   M  26       0       <NA>           <NA>           <NA>      <NA>      <NA>
#> 5   F  66       0       <NA>     2044-05-20     2044-06-17    BRAND2    BRAND2
```

## Package vignettes

More details on how to use *{{ packagename }}* can be found in the
[online documentation as package
vignettes](https://epiverse-trace.github.io/%7B%7B%20packagename%20%7D%7D/),
in the article “Get Started with vaccineff”.

## Help

To report a bug or to request a new feature please open an
[issue](https://github.com/%7B%7B%20gh_repo%20%7D%7D/issues/new/choose).

## Contribute

Contributions to *{{ packagename }}* are welcomed. Please follow the
[package contributing
guide](https://github.com/%7B%7B%20gh_repo%20%7D%7D/blob/main/.github/CONTRIBUTING.md).

## Code of conduct

Please note that the *{{ packagename }}* project is released with a
[Contributor Code of
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

- [Pratik Gupte](https://github.com/pratikunterwegs) (contributor)

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-torvaldsen2020" class="csl-entry">

Torvaldsen, S., and P. B. McIntyre. 2020. “Observational Methods in
Epidemiologic Assessment of Vaccine Effectiveness.” *Communicable
Diseases Intelligence Quarterly Report* 26 (3).
<https://doi.org/10.3316/informit.511798489353134>.

</div>

</div>
