#### Tests for the cohort method of vaccine effectiveness calculation ####

# prepare data
# load example data from package
data("cohortdata")

# add immunization dates
cohortdata$immunization <- get_immunization_date(
  data = cohortdata,
  outcome_date_col = "death_date",
  outcome_delay = 0,
  immunization_delay = 14,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = as.Date("2044-12-31"),
  take_first = FALSE
)

# add vaccine status
cohortdata$vaccine_status <- set_status(
  data = cohortdata,
  col_names = "immunization",
  status = c("v", "u")
)

# add death status
cohortdata$death_status <- set_status(
  data = cohortdata,
  col_names = "death_date"
)

# add time to death
cohortdata$time_to_event <- get_time_to_event(
  data = cohortdata,
  outcome_date_col = "death_date",
  start_cohort = as.Date("2044-01-01"),
  end_cohort = as.Date("2044-12-31"),
  start_from_immunization = FALSE
)

#### Basic expectations of `coh_eff_noconf()`
test_that("`coh_eff_noconf`: basic expectations", {

  # runs without conditions
  expect_no_condition(
    coh_eff_noconf(
      cohortdata,
      "death_status",
      "time_to_event",
      "vaccine_status"
    )
  )

  # returns a data.frame
  data <- coh_eff_noconf(
    cohortdata,
    "death_status",
    "time_to_event",
    "vaccine_status"
  )
  expect_s3_class(
    data, "data.frame"
  )
})
