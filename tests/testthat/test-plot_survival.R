#### Tests for plot_survival()
## This test uses directly the internal functions of the package
## to avoid running the matching functions
## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

## get immunization date to death
cohortdata$immunization <-
  get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = end_cohort,
    take_first = FALSE
  )

## set vaccine status based on immunization date
cohortdata$vaccine_status <- set_status(
  data = cohortdata,
  col_names = "immunization",
  status = c("v", "u")
)

## set outcome status
cohortdata$death_status <- set_status(
  data = cohortdata,
  col_names = "death_date"
)

## get time to event
cohortdata$time_to_death <- get_time_to_event(
  data = cohortdata,
  outcome_date_col = "death_date",
  start_cohort = start_cohort,
  end_cohort = end_cohort,
  start_from_immunization = FALSE
)

# test to test default options
test_that("`plot_survival`: default params", {
  plt <- plot_survival(
    data = cohortdata,
    outcome_status_col = "death_status",
    time_to_event_col = "time_to_death",
    vacc_status_col = "vaccine_status",
    vaccinated_status = "v",
    unvaccinated_status = "u",
    vaccinated_color = "steelblue",
    unvaccinated_color = "darkred",
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    percentage = TRUE,
    cumulative = FALSE
  )

  expect_identical(plt$labels$y, "Survival probability")
  expect_setequal(
    unique(plt$data$strata),
    c("v", "u")
  )
})

# test to test integer scale y axis
test_that("`plot_survival`: integer scale", {
  plt <- plot_survival(
    data = cohortdata,
    outcome_status_col = "death_status",
    time_to_event_col = "time_to_death",
    vacc_status_col = "vaccine_status",
    vaccinated_status = "v",
    unvaccinated_status = "u",
    vaccinated_color = "steelblue",
    unvaccinated_color = "darkred",
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    percentage = FALSE,
    cumulative = TRUE
  )

  expect_s3_class(ggplot2::layer_scales(plt)$y, "ScaleContinuous")
})

# test to test non-percentage
test_that("`plot_survival`: Snapshot test", {
  plt <- plot_survival(
    data = cohortdata,
    outcome_status_col = "death_status",
    time_to_event_col = "time_to_death",
    vacc_status_col = "vaccine_status",
    vaccinated_status = "v",
    unvaccinated_status = "u",
    vaccinated_color = "steelblue",
    unvaccinated_color = "darkred",
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    percentage = TRUE,
    cumulative = TRUE
  )

  expect_identical(plt$labels$y, "Cumulative incidence")
})
