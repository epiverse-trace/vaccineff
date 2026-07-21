# Function for Extracting Vaccineff Data Plot

This function returns a plot of the vaccine coverage or the cumulative
coverage (if cumulative = TRUE). The return is a 2-axis `ggplot2`
element with the number of vaccines per date on the left axis and the
coverage per date on the right axis. When a matching routine is
performed, the left axis also accounts for the doses of the matched
cohort.

## Usage

``` r
# S3 method for class 'vaccineff_data'
plot(x, date_interval = NULL, cumulative = FALSE, ...)
```

## Arguments

- x:

  Object of class `vaccineff_data`.

- date_interval:

  If NULL, the function calculates the coverage interval

- cumulative:

  If `TRUE`, returns the cumulative number of doses over the time
  window.

- ...:

  Additional arguments passed to other functions.

## Value

Plot extracted from `vaccineff`.
