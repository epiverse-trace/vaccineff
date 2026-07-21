# Function to Estimate the Vaccine Effectiveness Using Hazard Ratio

The function relies on the implementation of the Cox model for
proportional hazards from the package `{survival}` The proportional
hazards hypothesis is tested using the Schoenfeld test, and the
resultant p-value is provided in the results. Log-log plots are also
calculated using the Kaplan-Meier survival estimator to provide a visual
test for the proportional hazards hypothesis. The function returns a
list with the method called, the estimation of VE (CI95%), the p-value
of the Schoenfeld test, and the log-log plot.

## Usage

``` r
coh_eff_hr(
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

VE (CI95%), output from `cox_model`, prediction from `cox_model`, and
output from `km_model`
