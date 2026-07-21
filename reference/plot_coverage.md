# Plot Vaccine Coverage

This function returns a plot of the vaccine coverage or the cumulative
coverage (if cumulative = TRUE). The return is a 2-axis `ggplot2`
element with the number of vaccines per date on the left axis and the
coverage per date on the right axis. When a matching routine is
performed, the left axis also accounts for the doses of the matched
cohort.

## Usage

``` r
plot_coverage(vaccineff_data, date_interval = NULL, cumulative = FALSE)
```

## Arguments

- vaccineff_data:

  Object of the class `vaccineff_data` with vaccineff data.

- date_interval:

  If NULL, the function calculates the coverage interval based on the
  min() and max() of the `vacc_date_col`. It is also possible to pass a
  custom date interval to truncate or expand the date interval (see
  example).

- cumulative:

  If `TRUE`, returns the cumulative number of doses over the time
  window.

## Value

2-axis ggplot2 plot of vaccine coverage and daily doses.
