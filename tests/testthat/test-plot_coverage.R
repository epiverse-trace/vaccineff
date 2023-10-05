#### Tests for plot_coverage()

## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# snapshot test to test default options
test_that("`plot_coverage`: Snapshot test", {
  plt <- plot_coverage(
    data = cohortdata,
    vacc_date_col = "vaccine_date_1",
    unit = "month",
    doses_count_color = "steelblue",
    coverage_color = "mediumpurple",
    date_interval = NULL,
    cumulative = FALSE
  )

  vdiffr::expect_doppelganger("coverage_default", plt)
})

# snapshot test to test cumulative
test_that("`plot_coverage`: Snapshot test", {
  plt <- plot_coverage(
    data = cohortdata,
    vacc_date_col = "vaccine_date_1",
    unit = "month",
    doses_count_color = "steelblue",
    coverage_color = "mediumpurple",
    date_interval = NULL,
    cumulative = TRUE
  )

  vdiffr::expect_doppelganger("coverage_cumulative", plt)
})

# snapshot test to test fixed interval
test_that("`plot_coverage`: Snapshot test", {
  start <- as.Date("2044-04-01")
  end <- as.Date("2044-12-31")
  date_interval <- c(start, end)
  plt <- plot_coverage(
    data = cohortdata,
    vacc_date_col = "vaccine_date_1",
    unit = "month",
    doses_count_color = "steelblue",
    coverage_color = "mediumpurple",
    date_interval = date_interval,
    cumulative = FALSE
  )

  vdiffr::expect_doppelganger("coverage_interval", plt)
})
