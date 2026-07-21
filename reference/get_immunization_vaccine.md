# Construct Vaccine Name Associated to Immunizing Vaccine

This function is used in cases where several columns with vaccine dates
and different names are provided by the user. It returns a column with
the name of the vaccine used as immunizing. The parameters are set in
agreement with `get_immunization_date`.

## Usage

``` r
get_immunization_vaccine(
  data_set,
  immunization_date_col,
  vacc_date_col,
  vacc_name_col,
  immunization_delay
)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- vacc_date_col:

  Name of the column(s) that contain the vaccine dates.

- vacc_name_col:

  Name of the column(s) that contain custom vaccine names for the
  vaccines (e.g. brand name, type of vaccine)

- immunization_delay:

  Characteristic time in days before the patient is considered immune.

## Value

Custom vaccine names of the immunizing vaccine.
