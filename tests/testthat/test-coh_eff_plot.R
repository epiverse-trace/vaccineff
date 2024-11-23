#### Tests for coh_immunization.R module ####
#### Prepare data for all the tests ####
data("cohortdata")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 15000 # Minimum sample size that contains outcomes
set.seed(123) # use fixed seed
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort <- cohortdata[sample_indices, ]
rownames(sample_cohort) <- NULL

vaccineff_data <- make_vaccineff_data(
  data_set = sample_cohort,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  vacc_date_col = "vaccine_date_2",
  vaccinated_status = "v",
  unvaccinated_status = "u",
  immunization_delay = 15,
  end_cohort = as.Date("2044-12-31"),
  match = FALSE
)

# avoid warnings from p_value in this test
output <- capture_warnings(estimate_vaccineff(vaccineff_data, at = 180))
ve <- output$result

#### Tests for plot_loglog() ####
# This test wil be added after refactoring plot_loglog
# using Cox model prediction

#### Tests for plot_survival() ####
# test to test default options
test_that("`plot_survival`: default params", {
  plt <- plot_survival(
    km = ve$kaplan_meier,
    percentage = TRUE,
    cumulative = FALSE
  )

  expect_identical(plt$labels$y, "Survival probability")
  expect_true(
    all(levels(plt$data$strata) %in% c("v", "u"))
  )
})

# test to test integer scale y axis
test_that("`plot_survival`: integer scale", {
  plt <- plot_survival(
    km = ve$kaplan_meier,
    percentage = FALSE,
    cumulative = TRUE
  )

  expect_s3_class(ggplot2::layer_scales(plt)$y, "ScaleContinuous")
})

# test to test non-percentage
test_that("`plot_survival`: test for features cumulative TRUE", {
  plt <- plot_survival(
    km = ve$kaplan_meier,
    percentage = TRUE,
    cumulative = TRUE
  )

  expect_identical(plt$labels$y, "Cumulative incidence")
})
