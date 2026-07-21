# Calculate Vaccine Coverage

This function returns the vaccination coverage of a dose along the
cohort study. The coverage can be calculated grouped by year, day, and
month. This must be specified in the parameter `unit`. If there are no
records for some dates, the function assigns 0 instead of NA to
calculate the cumulative coverage.

## Usage

``` r
coh_coverage(
  data_set,
  vacc_date_col,
  unit = c("day", "month", "year"),
  date_interval = NULL
)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- vacc_date_col:

  Name of the column(s) that contain the vaccine date to calculate the
  coverage.

- unit:

  Aggregation unit, must be either "year", "month", or "day".

- date_interval:

  If NULL, the function calculates the coverage interval based on the
  min() and max() of the `vacc_date_col`. It is also possible to pass a
  custom date interval to truncate or expand the date interval (see
  example).

## Value

`data.frame` with the number of vaccine doses per date, cumulative count
of doses, and vaccine coverage.
