# Function for Extracting Vaccine Effectiveness Plot

This function creates plots from an object of class `vaccineff`. It
returns a Log-Log plot when `type = "loglog"`, or a Survival curve when
`type = "surv"`. Survival plots can be shown as cumulative incidence
(`cumulative = TRUE`), and using percentages (`percentage = TRUE`).

## Usage

``` r
# S3 method for class 'vaccineff'
plot(
  x,
  type = c("loglog", "surv"),
  cumulative = FALSE,
  percentage = FALSE,
  ...
)
```

## Arguments

- x:

  Object of class `vaccineff`.

- type:

  Type of plot. Options are `loglog` and `surv`.

- cumulative:

  If `TRUE`, the survival curve is shown as cumulative incidence.

- percentage:

  If `TRUE`, results are shown on a percentage scale.

- ...:

  Additional arguments passed to other functions.

## Value

Plot extracted from `vaccineff`.
