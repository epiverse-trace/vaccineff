#### Tests for get_time_to_event() ####

# Prepare some data
data("cohortdata")
data <- as.data.frame(cohortdata)

# assign immunization date
data$immunization_death <- get_immunization_date(
  data = cohortdata,
  outcome_date_col = "death_date",
  outcome_delay = 0,
  immunization_delay = 14,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = as.Date("2044-12-31"),
  take_first = FALSE
)

# Snapshot tests to check cleaned up implementation
test_that("Snapshot test for get_time_to_event", {

  # calculate time to death
  time_to_death <- get_time_to_event(
    data = data,
    outcome_date_col = "death_date",
    start_cohort = as.Date("2044-01-01"),
    end_cohort = as.Date("2044-12-31"),
    start_from_immunization = TRUE,
    immunization_date_col = "immunization_death"
  )

  expect_snapshot(
    head(time_to_death, 20)
  )
})

# Tests for basic expectations
test_that("`get_time_to_event`: Basic expectations", {
  # cohort start and end time
  start_cohort <- as.Date("2044-01-01")
  end_cohort <- as.Date("2044-12-31")

  # calculate time to death
  time_to_death <- get_time_to_event(
    data = data,
    outcome_date_col = "death_date",
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    start_from_immunization = TRUE,
    immunization_date_col = "immunization_death"
  )

  # expect numeric vector
  expect_vector(
    time_to_death,
    ptype = numeric()
  )
  expect_true(
    all(time_to_death >= 0 | is.na(time_to_death))
  )
})
