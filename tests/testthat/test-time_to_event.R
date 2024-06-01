#### Tests for get_time_to_event() ####

# Prepare some data
data("cohortdata")

# assign immunization date
cohortdata$immunization <- get_immunization_date(
  data = cohortdata,
  outcome_date_col = "death_date",
  immunization_delay = 14,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = as.Date("2044-12-31"),
  take_first = FALSE
)

# Snapshot tests to check cleaned up implementation
test_that("Snapshot test for get_time_to_event", {

  # calculate time to death
  time_to_death <- get_time_to_event(
    data = cohortdata,
    outcome_date_col = "death_date",
    start_cohort = as.Date("2044-01-01"),
    end_cohort = as.Date("2044-12-31"),
    start_from_immunization = TRUE,
    immunization_date_col = "immunization"
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
    data = cohortdata,
    outcome_date_col = "death_date",
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    start_from_immunization = TRUE,
    immunization_date_col = "immunization"
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

# Tests for censoring
test_that("`get_time_to_event`: Censoring provided", {
  # cohort start and end time
  start_cohort <- as.Date("2044-01-01")
  end_cohort <- as.Date("2044-12-31")

  # assign immunization date
  cohortdata$immunization_c <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = as.Date("2044-12-31"),
    take_first = FALSE
  )

  # calculate time to death using censoring data
  time_to_death_c <- get_time_to_event(
    data = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    start_from_immunization = TRUE,
    immunization_date_col = "immunization_c"
  )

  # expect numeric vector
  expect_vector(
    time_to_death_c,
    ptype = numeric()
  )
  expect_true(
    all(time_to_death_c >= 0 | is.na(time_to_death_c))
  )

  # expected outcome for registers with immunization and censoring informed
  cohortdata$time_to_death_c <- time_to_death_c
  informed <- cohortdata[!is.na(cohortdata$death_other_causes) &
                           !is.na(cohortdata$immunization_c), ]
  expect_true(
    all(informed$time_to_death_c ==
        informed$death_other_causes - informed$immunization_c
    )
  )

})
