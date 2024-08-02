#### Tests for iterate_match()
## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 10000
set.seed(123) # use fixed seed to avoid problems with snapshots
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort <- cohortdata[sample_indices, ]
rownames(sample_cohort) <- NULL

nearest <- c(age = 1)
exact <- "sex"
outcome_date_col <- "death_date"
censoring_date_col <- "death_other_causes"
vacc_date_col <- "vaccine_date_2"
immunization_delay <- 15


# Create `data.frame` with information on immunization
sample_cohort <- make_immunization(
  data_set = sample_cohort,
  outcome_date_col = outcome_date_col,
  censoring_date_col = censoring_date_col,
  immunization_delay = immunization_delay,
  vacc_date_col = vacc_date_col,
  end_cohort = end_cohort
)

# Generate id to control matches
sample_cohort$match_id <- seq_len(nrow(sample_cohort))

vacc_status_col <- "vaccine_status"
immunization_date_col <- "immunization_date"

# Match sample cohort
matched <- match_cohort_(
  data_set = sample_cohort,
  vacc_status_col = vacc_status_col,
  nearest = nearest,
  exact = exact
)

# Adjust exposition times of matched cohort
adjusted_0 <- adjust_exposition(matched_cohort = matched,
  outcome_date_col = outcome_date_col,
  censoring_date_col = censoring_date_col,
  immunization_date = immunization_date_col,
  start_cohort = start_cohort,
  end_cohort = end_cohort
)

# Test for basic expectations and correctness of algorithm
test_that("`iterate_match`: Correctness", {
  # handler function is used to avoid undesired warnings on test
  # This warnings are already tested in the test script for rematch_()
  output <- capture_warnings(iterate_match(
    all = sample_cohort,
    matched = matched,
    adjusted = adjusted_0,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    immunization_date_col = immunization_date_col,
    vacc_status_col = vacc_status_col,
    vaccinated_status = "v",
    unvaccinated_status = "u",
    nearest = nearest,
    exact = exact,
    start_cohort = start_cohort,
    end_cohort = end_cohort
  ))

  expect_gt(
    # new cohort must contain new pairs
    nrow(output$result), nrow(adjusted_0)
  )

  expect_true(
    # new cohort must contain previous cohort
    all(adjusted_0$match_id %in% output$result$match_id)
  )

  # Warnings are also tested for completeness using a snapshot
  expect_snapshot_value(output$warnings, style = "json2", tolerance = 1e-2)
})
