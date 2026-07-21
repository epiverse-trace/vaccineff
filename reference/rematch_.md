# Rematch Step for Iterative Match

This auxiliary function performs the re-matching strategy for
[`iterate_match()`](https://epiverse-trace.github.io/vaccineff/reference/iterate_match.md).
It rematches the population with status given by `rematch_status` and
excludes the removed population with status `control_status`. A
`tryCatch` validation handles errors when no matches can be generated
due to the lack of treated or untreated individuals.

## Usage

``` r
rematch_(
  all,
  adjusted,
  outcome_date_col,
  censoring_date_col,
  immunization_date_col,
  removed_i,
  vacc_status_col,
  rematch_status,
  control_status,
  nearest,
  exact,
  start_cohort,
  end_cohort,
  im
)
```

## Arguments

- all:

  `data.frame` with the entire cohort.

- adjusted:

  `data.frame` with the adjusted cohort to calculate removed cases.
  Default is NULL, which returns 0.

- outcome_date_col:

  Name of the column that contains the outcome dates.

- censoring_date_col:

  Name of the column that contains the censoring date.

- immunization_date_col:

  Name of the column that contains the immunization date to set the
  beginning of the follow-up period (`t0_follow_up`).

- vacc_status_col:

  Name of the column containing the vaccination.

- nearest:

  Named vector with name(s) of column(s) for `nearest` matching and
  caliper(s) for each variable (e.g.,
  `nearest = c("characteristic1" = n1, "characteristic2" = n2)`, where
  `n1` and `n2` are the calipers).

- exact:

  Name(s) of column(s) for `exact` matching

- start_cohort:

  Start date of the study.

- end_cohort:

  End date of the study.

- im:

  Iteration number for error message

## Value

List that contains three `data.frame`: `adjusted`: updated adjusted
matches `adjusted_i_s`: new adjusted matches for `rematch_status`
`matched_i_s`: all the new matches for `rematch_status`
