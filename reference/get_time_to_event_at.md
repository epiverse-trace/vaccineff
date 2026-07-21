# Construct Time-to-Event at

This function returns both the time-to-event until a reference number of
days, as provided in `at`, and the outcome status at the same point. It
uses the exposure time as an auxiliary variable to calculate the
time-to-event. The starting point for counting the time-to-event is
`t0_follow_up`, which is determined based on whether a matching strategy
is used or not. If the event occurs before the reference date, the end
date of the exposure period is used to calculate the time-to-event. This
accounts for whether censoring or an event occurred. The outcome status
is determined based on whether the outcome date coincides with the end
of the follow-up period.

## Usage

``` r
get_time_to_event_at(
  data_set,
  outcome_date_col,
  censoring_date_col,
  end_cohort,
  at
)
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

- at:

  Time to truncate the follow-up period

## Value

`data.frame` containing time_to_event and outcome_status
