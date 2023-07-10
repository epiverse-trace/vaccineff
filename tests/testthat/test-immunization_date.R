#### Tests for get_immnization_date()

# snapshot test to test edited implementations
test_that("`get_immunization_date`: Snapshot test", {
  data("cohortdata")
  cohortdata = as.data.frame(cohortdata)

  # get immunization dates
  immunization_death <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = 0,
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = "2021-12-31",
    take_first = FALSE
  )

  expect_snapshot(
    head(immunization_death, 15)
  )
})


