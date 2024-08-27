#### Tests for plot_coverage()

## prepare data
data("cohortdata")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 15000
set.seed(123) # use fixed seed to avoid problems with snapshots
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort <- cohortdata[sample_indices, ]
rownames(sample_cohort) <- NULL

vaccineff_data <- make_vaccineff_data(
  data_set = sample_cohort,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  vacc_date_col = "vaccine_date_2",
  vaccinated_status = "1 vacc",
  unvaccinated_status = "0 unvacc",
  immunization_delay = 15,
  start_cohort = as.Date("2044-01-01"),
  end_cohort = as.Date("2044-12-31"),
  match = TRUE,
  exact = c("age", "sex"),
  nearest = NULL
)

# snapshot test to test default options
test_that("`plot_coverage`: default plot", {
  plt <- plot_coverage(
    vaccineff_data = vaccineff_data,
    date_interval = NULL,
    cumulative = FALSE
  )

  expect_identical(plt$labels$y, "coverage * max(dose_plot)")
  expect_identical(plt$data$doses, plt$data$dose_plot)
})

# snapshot test to test cumulative
test_that("`plot_coverage`: cumulative plot", {
  plt <- plot_coverage(
    vaccineff_data = vaccineff_data,
    date_interval = NULL,
    cumulative = TRUE
  )

  expect_identical(plt$labels$y, "coverage * max(dose_plot)")
  expect_identical(plt$data$cum_doses, plt$data$dose_plot)
})
