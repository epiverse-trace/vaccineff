# Balance of Vaccinated/Unvaccinated Groups

This function creates a summary after matching.

## Usage

``` r
balance_summary(
  data_set,
  nearest,
  exact,
  vacc_status_col,
  vaccinated_status,
  unvaccinated_status
)
```

## Arguments

- data_set:

  `data.frame` to assess matching balance.

- nearest:

  Named vector with name(s) of column(s) for `nearest` matching and
  caliper(s) for each variable (e.g.,
  `nearest = c("characteristic1" = n1, "characteristic2" = n2)`, where
  `n1` and `n2` are the calipers).

- exact:

  Name(s) of column(s) for `exact` matching

- vacc_status_col:

  Name of the column containing the vaccination.

- vaccinated_status:

  Status assigned to the vaccinated population.

- unvaccinated_status:

  Status assigned to the unvaccinated population.

## Value

Summary `data.frame` with the balance of each variable by vaccine
status. Numeric variables are reported with means, and
categorical/factor variables are reported with proportions. In both
cases, the Standardized Mean Difference (SMD) is calculated.
