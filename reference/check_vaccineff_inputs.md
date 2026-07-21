# Check Correctness of Inputs in `make_vaccineff_data`

This auxiliary function checks the correctness of the inputs provided to
`make_vaccineff_data`.

## Usage

``` r
check_vaccineff_inputs(
  data_set,
  outcome_date_col,
  censoring_date_col,
  vacc_date_col,
  vacc_name_col,
  vaccinated_status,
  unvaccinated_status,
  immunization_delay,
  end_cohort,
  match,
  exact,
  nearest,
  take_first
)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- outcome_date_col:

  Name of the column that contains the outcome dates.

- censoring_date_col:

  Name of the column that contains the censoring date. NULL by default.

- vacc_date_col:

  Name of the column(s) that contain the vaccine dates.

- vacc_name_col:

  Name of the column(s) that contain custom vaccine names for the
  vaccines (e.g. brand name, type of vaccine). If provided, must be of
  the same length as `vacc_date_col`.

- vaccinated_status:

  Status assigned to the vaccinated population. Default is `v`.

- unvaccinated_status:

  Status assigned to the unvaccinated population. Default is `u`.

- immunization_delay:

  Characteristic time in days before the patient is considered immune.
  Default is 0.

- end_cohort:

  End date of the study.

- match:

  `TRUE`: cohort matching is performed. Default is `FALSE`

- exact:

  Name(s) of column(s) for `exact` matching. Default is `NULL`.

- nearest:

  Named vector with name(s) of column(s) for `nearest` matching and
  caliper(s) for each variable (e.g.,
  `nearest = c("characteristic1" = n1, "characteristic2" = n2)`, where
  `n1` and `n2` are the calipers). Default is `NULL`.

- take_first:

  `FALSE`: takes the latest vaccine date. `TRUE`: takes the earliest
  vaccine date.

## Value

This function does not return a value.
