#### Tests for make_immunization()
## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# test for basic expectations
test_that("`make_immunization`: basic expectations", {
  # Create rows with information on immunization
  cohortdata <- make_immunization(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = "vaccine_date_2",
    end_cohort = end_cohort
  )
  expect_true(
    all(c("immunization_date", "vaccine_status") %in% names(cohortdata))
  )
  # custom names for vaccine status
  cohortdata <- make_immunization(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = "vaccine_date_2",
    end_cohort = end_cohort,
    vaccinated_status = "vaccinated",
    unvaccinated_status = "unvaccinated"
  )

  expect_true(
    all(c("vaccinated", "unvaccinated") %in% cohortdata$vaccine_status)
  )
})

#### test for input validation
test_that("`make_immunization`: input validation", {
  df <- data.frame()

  # Function does not find columns in df
  expect_error(
    make_immunization(
      data_set = df,
      outcome_date_col = "death_date",
      censoring_date_col = "death_other_causes",
      immunization_delay = 14,
      vacc_date_col = "vaccine_date_2",
      end_cohort = end_cohort
    )
  )

  # non-integer delay
  expect_error(
    make_immunization(
      data_set = df,
      outcome_date_col = "death_date",
      censoring_date_col = "death_other_causes",
      immunization_delay = 14.7,
      vacc_date_col = "vaccine_date_2",
      end_cohort = end_cohort
    )
  )

  # vacc_name col must be same length as vacc_date_col
  expect_error(
    make_immunization(
      data_set = cohortdata,
      outcome_date_col = "death_date",
      censoring_date_col = "death_other_causes",
      immunization_delay = 14,
      vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
      vacc_name_col = "vaccine_1",
      end_cohort = end_cohort
    )
  )

  # test for end_cohort date no longer than 2100
  expect_warning(
    make_immunization(
      data_set = cohortdata,
      outcome_date_col = "death_date",
      immunization_delay = 14,
      vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
      end_cohort = as.Date("2101-12-31")
    )
  )
})

# test for more than one vaccination column
test_that("`make_immunization`: two vaccination columns", {
  # Create rows with information on immunizing vaccine
  cohortdata <- make_immunization(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = end_cohort
  )
  expect_true(
    all(c("immunization_date", "vaccine_status", "immunizing_vaccine") %in%
          names(cohortdata))
  )
  expect_true(
    all(c("vaccine_date_1", "vaccine_date_2") %in%
          cohortdata$immunizing_vaccine)
  )
})

test_that("`make_immunization`: vaccine names provided", {
  # vaccine name provided
  cohortdata <- make_immunization(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = "vaccine_date_1",
    vacc_name_col = "vaccine_1",
    end_cohort = end_cohort
  )
  expect_true(
    "immunizing_vaccine_name" %in% names(cohortdata)
  )
  expect_true(
    all(c("BRAND1", "BRAND2") %in%
          cohortdata$immunizing_vaccine_name)
  )
})
