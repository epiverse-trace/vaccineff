# Iterate Match

This function iterates to re-match registers that were removed after
adjusting exposure times. To avoid generating the same pairs already
removed, each iteration is split in two steps, one for the removed
vaccinated population and the other for the unvaccinated. A `tryCatch`
validation handles errors when no matches can be generated due to the
lack of treated or untreated individuals. The threshold for the maximum
number of iterations is the total removed population for the first
iteration. The algorithm iterates until no new adjusted pairs are
generated or the maximum number of iterations is reached.

## Usage

``` r
iterate_match(
  all,
  matched,
  adjusted,
  outcome_date_col,
  censoring_date_col,
  immunization_date_col,
  vacc_status_col,
  vaccinated_status,
  unvaccinated_status,
  exact,
  nearest,
  start_cohort,
  end_cohort
)
```

## Arguments

- all:

  `data.frame` with the entire cohort.

- matched:

  `data.frame` with the matched cohort.

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

- vaccinated_status:

  Status assigned to the vaccinated population.

- unvaccinated_status:

  Status assigned to the unvaccinated population.

- exact:

  Name(s) of column(s) for `exact` matching

- nearest:

  Named vector with name(s) of column(s) for `nearest` matching and
  caliper(s) for each variable (e.g.,
  `nearest = c("characteristic1" = n1, "characteristic2" = n2)`, where
  `n1` and `n2` are the calipers).

- start_cohort:

  Start date of the study.

- end_cohort:

  End date of the study.

## Value

`data.frame` with adjusted pairs after iterating.
