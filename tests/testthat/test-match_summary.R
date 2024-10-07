#### Tests for match_cohort()
## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 1000
set.seed(123) #use fixed seed to avoid problems with snapshots
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

#### Snapshot for summary comparing only two dataframes
test_that("`match_cohort`: summary snapshot", {
  # snapshot for summary
  matched <- get_dataset.match(matching)
  summary <- match_summary(all = sample_cohort,
    matched = matched,
    vacc_status_col = "vaccine_status"
  )
  expect_snapshot(summary)
})
