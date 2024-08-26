#### Tests for plot_survival()
## This test uses directly the internal functions of the package
## to avoid running the matching functions
## prepare data
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
  start_cohort = as.Date("2044-01-01"),
  end_cohort = as.Date("2044-12-31"),
  match = TRUE,
  exact = c("age", "sex"),
  nearest = NULL
)

#### Basic expectations of `effectiveness()`
test_that("`effectiveness`: basic expectations", {

  # runs without conditions
  expect_no_condition(
    effectiveness(vaccineff_data)
  )

  # returns `effectiveness` s3class object
  eff <- effectiveness(vaccineff_data)
  expect_s3_class(
    eff, "effectiveness"
  )
})

#### test for input validation of plot() and summary() methods
test_that("`effectiveness`: test for input validation", {
  df <- data.frame()

  expect_error(
    summary.effectiveness(df),
    regexp = "Input must be an object of class 'effectiveness'"
  )

  expect_error(
    plot.effectiveness(df),
    regexp = "Input must be an object of class 'effectiveness'"
  )
})

#### Truncate at `effectiveness()`
test_that("`effectiveness`: at not null", {

  # runs without conditions
  eff <- effectiveness(vaccineff_data, at = 90)
  summ <- capture.output(summary.effectiveness(eff))
  expect_snapshot(summ)
})

## Tests for generic methods
eff <- effectiveness(vaccineff_data)

#### Summary
test_that("`summary.effectiveness`: basic expectations", {
  # snapshot for summary
  summ <- capture.output(summary.effectiveness(eff))
  expect_snapshot(summ)
})

#### Plot
test_that("`plot.effectiveness`: basic expectations", {
  # test for loglog plot
  plt <- plot.effectiveness(eff, type = "loglog")
  expect_identical(plt$labels$y, "Log[-Log[Surv.]]")
  plt <- plot.effectiveness(eff, type = "surv")
  expect_identical(plt$labels$y, "Survival probability")
  expect_s3_class(plt, "ggplot")
})
