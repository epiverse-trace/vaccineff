# Construct Immunization Date

This function constructs the immunization date per individual by
searching for the vaccine dates that satisfy the condition:
`vacc_date_col + immunization_delay <= limit_date`, where `limit_date`
is defined following the hierarchy: `censoring_date_col`,
`outcome_date_col`, `end_cohort`. If a date is not provided/found the
function takes the next one in the hierarchy. If several columns with
vaccine dates are provided, the function selects by default the closest
vaccine date to `limit_date`. However, it can also select the first
vaccine date by setting `take_first = TRUE`.

## Usage

``` r
get_immunization_date(
  data_set,
  outcome_date_col,
  censoring_date_col,
  immunization_delay,
  vacc_date_col,
  end_cohort,
  take_first = FALSE
)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- outcome_date_col:

  Name of the column that contains the outcome dates.

- censoring_date_col:

  Name of the column that contains the censoring date.

- immunization_delay:

  Characteristic time in days before the patient is considered immune.

- vacc_date_col:

  Name of the column(s) that contain the vaccine dates.

- end_cohort:

  End date of the study.

- take_first:

  `FALSE`: takes the latest vaccine date. `TRUE`: takes the earliest
  vaccine date.

## Value

Immunization date
