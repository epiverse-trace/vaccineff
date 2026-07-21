# Static Matching

This function calls `match_cohort_` once and then removes the pairs
whose exposure times do not coincide. It returns the adjusted cohort, a
summary of the matching result, and the balance of the cohort before and
after matching.

## Usage

``` r
static_match(
  data_set,
  outcome_date_col,
  censoring_date_col,
  immunization_date_col,
  vacc_status_col,
  vaccinated_status,
  unvaccinated_status,
  start_cohort,
  end_cohort,
  nearest,
  exact
)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

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

- start_cohort:

  Start date of the study.

- end_cohort:

  End date of the study.

- nearest:

  Named vector with name(s) of column(s) for `nearest` matching and
  caliper(s) for each variable (e.g.,
  `nearest = c("characteristic1" = n1, "characteristic2" = n2)`, where
  `n1` and `n2` are the calipers).

- exact:

  Name(s) of column(s) for `exact` matching

## Value

List with results from static match: `match`: adjusted cohort,
`summary`: matching summary, `balance_all`: balance of the cohort before
matching, `balance_matched`: balance of the cohort after matching.
