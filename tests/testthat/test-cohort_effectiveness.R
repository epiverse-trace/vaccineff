#### Tests for the effectiveness calculation ####
# Internal functions of the package are used to speed-up the test
# This test is not checking for PH hypothesis. To do so, it is necessary
# to match the population

# prepare data
# load example data from package
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# add immunization dates
cohortdata$immunization <- get_immunization_date(
  data = cohortdata,
  outcome_date_col = "death_date",
  immunization_delay = 14,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = end_cohort,
  take_first = FALSE
)

# add vaccine status
cohortdata$vaccine_status <- set_status(
  data = cohortdata,
  col_names = "immunization",
  status = c("v", "u")
)

# add death status
cohortdata$outcome_status <- set_status(
  data = cohortdata,
  col_names = "death_date"
)

# add time to death
cohortdata$time_to_event <- get_time_to_event(
  data = cohortdata,
  outcome_date_col = "death_date",
  start_cohort = start_cohort,
  end_cohort = end_cohort,
  start_from_immunization = FALSE
)

#### Basic expectations of `effectiveness()`
test_that("`effectiveness`: basic expectations", {

  # runs without conditions
  expect_no_condition(
    effectiveness(
      data = cohortdata,
      start_cohort = start_cohort,
      end_cohort = end_cohort,
      method = "HR"
    )
  )

  # returns `effectiveness` s3class object
  eff <- effectiveness(
    data = cohortdata,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    method = "HR"
  )
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

#### Basic expectations: summary and plot methods
test_that("`effectiveness`: basic expectations", {

  eff <- effectiveness(
    data = cohortdata,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    method = "HR"
  )

  # snapshot for summary
  invisible(capture.output(summ <- summary.effectiveness(eff))) #nolint
  expect_snapshot(summ)

  # tes for loglog plot
  plt <- plot.effectiveness(eff)
  expect_identical(plt$labels$y, "Log[-Log[Surv.]]")
  expect_s3_class(plt, "ggplot")
})
