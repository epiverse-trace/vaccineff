# Internal function to calculate Kaplan-Meier model and related metrics.

Internal function to calculate Kaplan-Meier model and related metrics.

## Usage

``` r
km_model(
  data_set,
  outcome_status_col,
  time_to_event_col,
  vacc_status_col,
  vaccinated_status,
  unvaccinated_status,
  start_cohort,
  end_cohort
)
```

## Value

`data.frame` with data from KM model: "time", "date", "strata",
"n.risk", "n.event", "n.censor", "surv", "lower", "upper",
"cumincidence", "cumincidence_lower", "cumincidence_upper"
