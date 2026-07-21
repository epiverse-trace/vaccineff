# Check Correctness of `data_set`

This auxiliary function checks that the type of the object provided in
`data_set` is a `data.frame` and that the specified columns are included
in it.

## Usage

``` r
check_dataset(data_set, columns)
```

## Arguments

- data_set:

  `data.frame`.

- columns:

  A vector of column names that should be present in `data_set`.

## Value

This function does not return a value.
