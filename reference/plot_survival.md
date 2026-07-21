# Plot the Survival Probability Based on the Kaplan-Meier Model

This function relies on the implementation of the Kaplan-Meier model
from the package `{survival}`. It returns a plot of the Survival
Probability or the Cumulative Hazard (if cumulative = TRUE). The return
is a ggplot2 element of the curves with 95% C.I. It is possible to
manipulate the colors, labels, legend, and most of the graphic elements.

## Usage

``` r
plot_survival(km, percentage = TRUE, cumulative = FALSE)
```

## Arguments

- km:

  Kaplan-Meier estimation created with `km_model`.

- percentage:

  If `TRUE`, returns probability in percentage.

- cumulative:

  If `TRUE`, returns cumulative incidence

## Value

`{ggplot2}` object with plot of survival or cumulative incidence.
