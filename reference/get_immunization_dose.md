# Return Column Name Associated to Immunizing Vaccine

This function is used in cases where several columns with vaccine dates
are provided by the user. It returns the name of the column of the
vaccine used as immunizing. The parameters are set in agreement with
`get_immunization_date`.

## Usage

``` r
get_immunization_dose(
  data_set,
  immunization_date_col,
  vacc_date_col,
  immunization_delay
)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- vacc_date_col:

  Name of the column(s) that contain the vaccine dates.

- immunization_delay:

  Characteristic time in days before the patient is considered immune.

## Value

Name of the column taken as immunizing vaccine for each register.
