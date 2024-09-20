#### Tests for match_cohort()
## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 1000
set.seed(123) # use fixed seed to avoid problems with snapshots
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort <- cohortdata[sample_indices, ]
rownames(sample_cohort) <- NULL

# Create `data.frame` with information on immunization
sample_cohort <- make_immunization(
  data_set = sample_cohort,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  immunization_delay = 14,
  vacc_date_col = "vaccine_date_2",
  end_cohort = end_cohort
)

# Match the data
output <- capture_warnings(match_cohort(
  data_set = sample_cohort,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  start_cohort = start_cohort,
  end_cohort = end_cohort,
  method = "static",
  exact = "sex",
  nearest = c(age = 1)
))

matching <- output$result

# test for basic expectations - all provided
test_that("`match_cohort`: basic expectations", {
  expect_s3_class(
    matching, "match"
  )
  # error arguments not provided
  expect_error(
    match_cohort(
      data_set = sample_cohort,
      outcome_date_col = "death_date",
      censoring_date_col = "death_other_causes",
      start_cohort = start_cohort,
      end_cohort = end_cohort
    ),
    regexp =
      "`exact` and `nearest` cannot be NULL. At least one must be provided"
  )
})

#### test for input validation of get_dataset() and summary() methods
test_that("`match_cohort`: test for input validation", {
  df <- data.frame()

  expect_error(
    summary.match(df),
    regexp = "Input must be an object of class 'match'"
  )

  expect_error(
    get_dataset.match(df),
    regexp = "Input must be an object of class 'match'"
  )
})

#### Snapshot for summary
test_that("`match_cohort`: summary snapshot", { # nolint
  for (column in c("balance_all", "balance_match", "summary")) {
    expect_snapshot_value(matching[column], style = "json2", tolerance = 1e-2)
  }
})
