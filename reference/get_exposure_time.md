# Calculate Exposure Time

This auxiliary function calculates the exposure time of individuals
starting from the t0_follow_up date defined based on whether a matching
strategy is invoked or not. The end of the exposure time is assigned
based on the follow hierarchy outcome_status -\> censoring_date -\>
end_cohort

## Usage

``` r
get_exposure_time(data_set, outcome_date_col, censoring_date_col, end_cohort)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- outcome_date_col:

  Name of the column that contains the outcome dates.

- censoring_date_col:

  Name of the column that contains the censoring date.

- end_cohort:

  End date of the study.

## Value

`column` with exposure time per individual.
