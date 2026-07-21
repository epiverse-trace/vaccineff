# Create Censoring date by Pairs

This function creates the censoring date of the pairs by inheriting the
minimum date in which any of the partners has a censoring event. Two
conditions are checked to inherit a censoring date in a pair.

1.  Individual censoring occurs before individual event;

2.  If an outcome happens before the censoring of the partner no
    censoring is inherited by the other.

## Usage

``` r
get_censoring_after_match(data_set, outcome_date_col, censoring_date_col)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- outcome_date_col:

  Name of the column that contains the outcome dates.

- censoring_date_col:

  Name of the column that contains the censoring date.
