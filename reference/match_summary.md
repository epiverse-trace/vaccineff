# Constructs Summary with Results from Matching

This function creates a summary for the results from cohort matching. It
provides counts grouped by vaccine status for the entire population, the
matched and unmatched populations.

## Usage

``` r
match_summary(all, matched, vacc_status_col)
```

## Arguments

- all:

  `data.frame` with the entire cohort.

- matched:

  `data.frame` with the matched cohort. calculate removed cases. Default
  is NULL, which returns 0.

- vacc_status_col:

  Name of the column containing the vaccination.

## Value

Summary `data.frame` with counts by vaccine status for: all, matched,
unmatched, and removed.
