#### Tests for estimate_vaccineff.R module ####
#### Prepare data for all the tests ####
data("cohortdata")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 15000 # Minimum sample size that contains outcomes
set.seed(123) # use fixed seed to avoid problems with snapshots
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
  match = TRUE,
  exact = c("age", "sex"),
  nearest = NULL
)

ve <- estimate_vaccineff(vaccineff_data, at = 60)

####Tests for estimate_vaccineff() ####
# Basic expectations for `estimate_vaccineff()`
test_that("`estimate_vaccineff`: basic expectations", {
  # returns `vaccineff` s3class object
  expect_s3_class(
    ve, "vaccineff"
  )

  # runs without conditions
  expect_no_condition(
    estimate_vaccineff(vaccineff_data, at = 60)
  )

  # expect error when at not provided
  expect_error(
    estimate_vaccineff(vaccineff_data, at = NULL)
  )
})

#### Tests for generic methods plot and summary ####
# test for input validation of plot() and summary() methods
test_that("`estimate_vaccineff`: test for input validation", {
  df <- data.frame()

  expect_error(
    summary.vaccineff(df),
    regexp = "Input must be an object of class 'vaccineff'"
  )

  expect_error(
    plot.vaccineff(df),
    regexp = "Input must be an object of class 'vaccineff'"
  )
})

# test for plot method. Basic expectations
test_that("`plot.vaccineff`: basic expectations", {
  # test for loglog plot
  plt <- plot.vaccineff(ve, type = "loglog")
  expect_identical(plt$labels$y, "-Log[-Log[Surv.]]")
  plt <- plot.vaccineff(ve, type = "surv")
  expect_identical(plt$labels$y, "Survival probability")
  expect_s3_class(plt, "ggplot")
})
