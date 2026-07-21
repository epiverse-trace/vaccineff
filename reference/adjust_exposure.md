# Adjust Exposure for Static Matching

This function removes the pairs whose exposure times do not match. This
happens when the outcome of the unvaccinated individual occurs before
the vaccination date of their partner.

## Usage

``` r
adjust_exposure(
  matched_cohort,
  outcome_date_col,
  censoring_date_col,
  immunization_date,
  start_cohort,
  end_cohort
)
```

## Arguments

- matched_cohort:

  `data.frame` with matched cohort from `match_cohort_`.

- outcome_date_col:

  Name of the column that contains the outcome dates.

- censoring_date_col:

  Name of the column that contains the censoring date.

- start_cohort:

  Start date of the study.

- end_cohort:

  End date of the study.

## Value

`data.frame` with matched population and corrected exposure times.
