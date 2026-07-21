# Internal function to calculate Cox-PH model and related metrics.

Internal function to calculate Cox-PH model and related metrics.

## Usage

``` r
cox_model(
  data_set,
  outcome_status_col,
  time_to_event_col,
  vacc_status_col,
  vaccinated_status,
  unvaccinated_status
)
```

## Value

List with data from Cox model: hr - hazard ratio (CI95%) p_value
`{survival}` object with model `{survival}` object with Schoenfeld test
