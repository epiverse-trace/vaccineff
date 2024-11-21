#### Tests for handler_validations.R module ####
#### Prepare data for all the tests ####
data("cohortdata")

data_set <- cohortdata
outcome_date_col <- "death_date"
censoring_date_col <- "death_other_causes"
vacc_date_col <- "vaccine_date_2"
vacc_name_col <- "vaccine_2"
vaccinated_status <- "v"
unvaccinated_status <- "u"
immunization_delay <- 0
start_cohort <- as.Date("2044-01-31")
end_cohort <- as.Date("2044-12-31")
match <- NULL
nearest <- NULL
exact <- NULL
take_first <- FALSE

#### Tests for check_dataset() ####
# Test for basic expectations
test_that("`check_dataset`: basic expectations", {

  # passing other type of element to data_set
  vector <- c("v1", "v2", "v3")
  expect_error(
    check_dataset(data_set = vector, columns = vector),
    regexp =
      paste0("Assertion on 'data_set' failed: Must be of type 'data.frame',",
             " not 'character'.")
  )
  # No error expected when passing NULL argument in name of cols
  expect_silent(
    check_dataset(data_set = cohortdata, columns = c("death_date", NULL, "age"))
  )
  # Expect error if columns name not included in data_set
  expect_error(
    check_dataset(data_set = cohortdata, columns = "wrong_name")
  )
})

#### Tests for check_vaccineff_inputs() ####
# Test for basic expectations
test_that("`check_vaccineff_inputs`: basic expectations", {
  # No error expected in allowed null cols
  expect_silent(
    check_vaccineff_inputs(
      data_set = data_set,
      outcome_date_col = outcome_date_col,
      censoring_date_col = NULL,
      vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
      vacc_name_col = NULL,
      vaccinated_status = vaccinated_status,
      unvaccinated_status = unvaccinated_status,
      immunization_delay = immunization_delay,
      end_cohort = end_cohort,
      match = FALSE,
      exact = NULL,
      nearest = NULL,
      take_first = take_first
    )
  )
  # vacc_name_col and vacc_date_col not matching
  expect_error(
    check_vaccineff_inputs(
      data_set = data_set,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
      vacc_name_col = "vaccine_1",
      vaccinated_status = vaccinated_status,
      unvaccinated_status = unvaccinated_status,
      immunization_delay = immunization_delay,
      end_cohort = end_cohort,
      match = match,
      exact = exact,
      nearest = nearest,
      take_first = take_first
    ),
    regexp = paste("Assertion on 'vacc_name_col' failed: Must have length",
                   ">= 2, but has length 1.")
  )

  # worng column name
  expect_error(
    check_vaccineff_inputs(
      data_set = data_set,
      outcome_date_col = "wrongcol",
      censoring_date_col = censoring_date_col,
      vacc_date_col = vacc_date_col,
      vacc_name_col = vacc_name_col,
      vaccinated_status = vaccinated_status,
      unvaccinated_status = unvaccinated_status,
      immunization_delay = immunization_delay,
      end_cohort = end_cohort,
      match = match,
      exact = exact,
      nearest = nearest,
      take_first = take_first
    ),
    regexp = "(Names must include the elements)*('wrongcol')"
  )

  # non-integer immunization delay
  expect_error(
    check_vaccineff_inputs(
      data_set = data_set,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      vacc_date_col = vacc_date_col,
      vacc_name_col = vacc_name_col,
      vaccinated_status = vaccinated_status,
      unvaccinated_status = unvaccinated_status,
      immunization_delay = 1.5,
      end_cohort = end_cohort,
      match = match,
      exact = exact,
      nearest = nearest,
      take_first = take_first
    )
  )

  # same vaccinated/unvaccinated status
  expect_error(
    check_vaccineff_inputs(
      data_set = data_set,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      vacc_date_col = vacc_date_col,
      vacc_name_col = vacc_name_col,
      vaccinated_status = "u",
      unvaccinated_status = "u",
      immunization_delay = immunization_delay,
      end_cohort = end_cohort,
      match = match,
      exact = exact,
      nearest = nearest,
      take_first = take_first
    ),
    regexp = "`vaccinated_status` and `unvaccinated_status` cannot be equal"
  )

  # error arguments not provided in match
  expect_error(
    check_vaccineff_inputs(
      data_set = data_set,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      vacc_date_col = vacc_date_col,
      vacc_name_col = vacc_name_col,
      vaccinated_status = vaccinated_status,
      unvaccinated_status = unvaccinated_status,
      immunization_delay = immunization_delay,
      end_cohort = end_cohort,
      match = TRUE,
      exact = NULL,
      nearest = NULL,
      take_first = take_first
    ),
    regexp =
      "`exact` and `nearest` cannot be NULL. At least one must be provided"
  )
})
