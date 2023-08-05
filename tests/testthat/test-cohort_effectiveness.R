#### Tests for the cohort method of vaccine effectiveness calculation ####

# prepare data
# load example data from package
data("cohortdata")

# add immunization dates
cohortdata$immunization_death <- get_immunization_date(
  data = cohortdata,
  outcome_date_col = "death_date",
  outcome_delay = 0,
  immunization_delay = 14,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = "2021-12-31",
  take_first = FALSE
)

# add vaccine status
cohortdata$vaccine_status <- set_status(
  data = cohortdata,
  col_names = "immunization_death",
  status = c("v", "u")
)

# add death status
cohortdata$death_status <- set_status(
  data = cohortdata,
  col_names = "death_date"
)

# add time to death
cohortdata$time_to_death <- get_time_to_event(
  data = cohortdata,
  outcome_date_col = "death_date",
  start_cohort = "2021-01-01",
  end_cohort = "2021-12-31",
  FALSE
)

#### Basic expectations of `coh_eff_noconf()`
test_that("`coh_eff_noconf`: basic expectations", {

  # runs without conditions
  expect_no_condition(
    coh_eff_noconf(
      cohortdata,
      "death_status",
      "time_to_death",
      "vaccine_status"
    )
  )

  # returns a data.frame
  data <- coh_eff_noconf(
    cohortdata,
    "death_status",
    "time_to_death",
    "vaccine_status"
  )
  expect_s3_class(
    data, "data.frame"
  )
  # check all cols are numerics except PH
  expect_true(
    all(
      apply(data[, setdiff(names(data), "PH")], 2, is.numeric)
    )
  )
})

#### Basic expectations of `coh_test_noconf()`
test_that("`coh_test_noconf`: basic expectations", {

  # runs without conditions
  expect_no_condition(
    coh_test_noconf(
      cohortdata,
      "death_status",
      "time_to_death",
      "vaccine_status"
    )
  )

  # returns a survival object
  data <- coh_test_noconf(
    cohortdata,
    "death_status",
    "time_to_death",
    "vaccine_status"
  )
  expect_s3_class(
    data, "cox.zph"
  )
})