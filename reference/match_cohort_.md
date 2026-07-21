# Match Cohort Using Mahalanobis Distance

This function constructs pairs of vaccinated and unvaccinated
individuals with similar characteristics. It relies on the matching
algorithm implemented in the package `{MatchIt}`. By default, the
function uses `method = "nearest"`, `ratio = 1`, and
`distance = "mahalanobis"` to perform the matching.

## Usage

``` r
match_cohort_(data_set, vacc_status_col, exact = NULL, nearest = NULL)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- vacc_status_col:

  Name of the column containing the vaccination.

- exact:

  Name(s) of column(s) for `exact` matching

- nearest:

  Named vector with name(s) of column(s) for `nearest` matching and
  caliper(s) for each variable (e.g.,
  `nearest = c("characteristic1" = n1, "characteristic2" = n2)`, where
  `n1` and `n2` are the calipers).

## Value

`data.frame` with the matched population.
