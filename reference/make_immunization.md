# Construct Information of Immunization

This function returns a `data.frame` with the relevant information on
immunization for the study. An individual is considered immunized after
the characteristic time for the immune response of the vaccine passes.
This time is provided to the function by the parameter
`immunization_delay`. By default, it is set to 0. The function searches
for vaccine dates based on the end of the follow-up period of each
individual defined by the censoring date (if provided), outcome delay
(if present), and end of the study.

The function also works with vaccination information spread across
several columns. If this is the case, the parameter `vacc_date_col` must
be passed as a vector with the names of all the columns to use. The
function uses by default the latest date found. However, it can also
select the first date by setting `take_first = TRUE`.

The function returns a column with the immunization date
(`immunization`) and a vaccine status column (`vaccine_status`) that is
constructed based on `immunization`. For several vaccines, the function
also returns the name of the column of the vaccine that was selected as
immunizing (`immunizing_dose`). If different custom names (e.g., brands)
are associated with each vaccine date, the function can return the
custom name of the vaccine selected as immunizing. This information must
be passed in the parameter `vacc_name_col`, as a vector in the same
order as `vacc_date_col`.

## Usage

``` r
make_immunization(
  data_set,
  outcome_date_col,
  censoring_date_col,
  vacc_date_col,
  vacc_name_col,
  vaccinated_status,
  unvaccinated_status,
  immunization_delay,
  end_cohort,
  take_first = FALSE
)
```

## Arguments

- data_set:

  `data.frame` with cohort information.

- outcome_date_col:

  Name of the column that contains the outcome dates.

- censoring_date_col:

  Name of the column that contains the censoring date.

- vacc_date_col:

  Name of the column(s) that contain the vaccine dates.

- vacc_name_col:

  Name of the column(s) that contain custom vaccine names for the
  vaccines (e.g. brand name, type of vaccine)

- vaccinated_status:

  Status assigned to the vaccinated population.

- unvaccinated_status:

  Status assigned to the unvaccinated population.

- immunization_delay:

  Characteristic time in days before the patient is considered immune.

- end_cohort:

  End date of the study.

- take_first:

  `FALSE`: takes the latest vaccine date. `TRUE`: takes the earliest
  vaccine date.

## Value

Original `data.frame` passed in `data_set` and additional columns
containing information on the immunization.
