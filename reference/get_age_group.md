# Construct age-group variable from age column

This method splits an age interval from `min_val` to `max_val` into
intervals of size `step`. If the method finds ages greater or equal than
`max_val` it assigns the string `">max_val"`. By default `min_val` is
set to 0, however it can be assigned by convenience. If the method finds
ages lower or equal than `min_val` it assigns the string `"<min_val-1"`.
The function warns when (max_val - min_val) is not an integer multiple
of step. In that case the last interval is truncated to the upper value
closest to max_val for which (closest_upper - min_val) is multiple of
step.

## Usage

``` r
get_age_group(data_set, col_age, max_val, min_val = 0, step)
```

## Arguments

- data_set:

  `data.frame` with at least a column containing the age information

- col_age:

  Name of the column containing the age information

- max_val:

  Maximum value of age interval to split

- min_val:

  Minimum value of age interval to split

- step:

  Step used to split the age interval

## Value

Column of type `factor` with the same length as the number of rows in
`data_set`, with levels corresponding to age bins between `min_val` and
`max_val`. Ages above `max_val` are represented as `>max_val`.

## Examples

``` r
# load data provided with the package
data(cohortdata)

# assign age groups as a column of the `data.frame`
cohortdata$age_group <- get_age_group(
  data_set = cohortdata,
  col_age = "age",
  max_val = 80,
  step = 10
)

# view the `data.frame` with new column
head(cohortdata)
#>         id sex age death_date death_other_causes vaccine_date_1 vaccine_date_2
#> 1 04edf85a   M  50       <NA>               <NA>           <NA>           <NA>
#> 2 c5a83f56   M  66       <NA>               <NA>           <NA>           <NA>
#> 3 82991731   M  81       <NA>               <NA>           <NA>           <NA>
#> 4 afbab268   M  74       <NA>               <NA>     2021-03-30     2021-05-16
#> 5 3faf2474   M  54       <NA>               <NA>     2021-06-01     2021-06-22
#> 6 97df7bdc   M  79       <NA>               <NA>     2021-03-21     2021-05-02
#>   vaccine_1 vaccine_2 age_group
#> 1      <NA>      <NA>     50-59
#> 2      <NA>      <NA>     60-69
#> 3      <NA>      <NA>       >80
#> 4    BRAND2    BRAND2     70-79
#> 5    BRAND1    BRAND1     50-59
#> 6    BRAND2    BRAND2     70-79
```
