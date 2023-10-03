#### Tests for plot_survival()

## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

## get immunization date to death
cohortdata$immunization_death <-
  get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = 0,
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = end_cohort,
    take_first = FALSE
  )

## set vaccine status based on immunization date
cohortdata$vaccine_status <- set_status(
  data = cohortdata,
  col_names = "immunization_death",
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

# snapshot test to test default options
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
    cumulative = FALSE
  )

  vdiffr::expect_doppelganger("survival_default", plt)
})

# snapshot test to test cumulative
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

  vdiffr::expect_doppelganger("survival_cumulative", plt)
})

# snapshot test to test non-percentage
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
    percentage = FALSE,
    cumulative = FALSE
  )

  vdiffr::expect_doppelganger("survival_non_perc", plt)
})

# snapshot test to test non-percentage cumulative
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
    percentage = FALSE,
    cumulative = TRUE
  )

  vdiffr::expect_doppelganger("survival_non_perc_cum", plt)
})
