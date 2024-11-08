#### Tests for coh_match.R module ####
#### Prepare data for all the tests ####
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# Sample cohort to make tests faster - take a bigger sample
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
  vacc_date_col = "vaccine_date_2",
  vacc_name_col = NULL,
  vaccinated_status = "v",
  unvaccinated_status = "u",
  immunization_delay = 14,
  end_cohort = end_cohort
)

# Match the data
output <- capture_warnings(match_cohort(
  data_set = sample_cohort,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  start_cohort = start_cohort,
  end_cohort = end_cohort,
  exact = "sex",
  nearest = c(age = 1),
  immunization_date_col = "immunization_date",
  vacc_status_col = "vaccine_status",
  vaccinated_status = "v",
  unvaccinated_status = "u"
))

matching <- output$result

# Test for basic expectations - all provided
test_that("`match_cohort`: basic expectations", {
  expect_s3_class(
    matching, "match"
  )
})

# Snapshot for summary
test_that("`match_cohort`: summary snapshot", { # nolint
  for (column in c("balance_all", "balance_match", "summary")) {
    expect_snapshot_value(matching[column], style = "json2", tolerance = 1e-2)
  }
})
