#### Tests for match_info()
## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# sample cohort to make tests faster
sample_size <- 5000
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort <- cohortdata[sample_indices, ]
rownames(sample_cohort) <- NULL

# assign vaccination status
sample_cohort$vaccine_status <- set_status(
  data = sample_cohort,
  col_names = "vaccine_date_2",
  status = c("v", "u")
)

# match cohort
matched_cohort <- match_cohort_(data = sample_cohort,
  vacc_status_col = "vaccine_status",
  exact = "sex"
)

# add column with minimum censoring date for pair
matched_cohort$censoring_pair <-  as.Date(match_pair_info(
  data = matched_cohort,
  column_to_match = "death_other_causes",
  criteria = "min"
))

# Basic expectations
test_that("`get_censoring_date_match`: basic expectations", {
  # expect date
  expect_vector(
    matched_cohort$censoring_pair,
    ptype = as.Date("2045-01-01")
  )
  # filter registers with censoring date informed
  censored_original <-
    matched_cohort[!is.na(matched_cohort$death_other_causes), ]
  # filter registers with censoring date created after matching
  censored_match <-
    matched_cohort[!is.na(matched_cohort$censoring_pair), ]

  # All the subclass IDs in censored_match must be contained in
  # censored_original
  expect_true(
    all(censored_match$subclass %in% censored_original$subclass)
  )
})

# Check for minimum censoring date per pair
test_that("`get_censoring_date_match`: take minimum censoring date", {
  censored_twodates <-
    matched_cohort[!is.na(matched_cohort$censoring_pair)  &
      !is.na(matched_cohort$death_other_causes),
    ]

  expect_true(
    all(
      censored_twodates$censoring_pair <= censored_twodates$death_other_causes
    )
  )
})

# Check for maximum censoring date per pair
# add column with censoring date for pair
matched_cohort$censoring_pair_max <-  as.Date(match_pair_info(
  data = matched_cohort,
  column_to_match = "death_other_causes",
  criteria = "max"
))

test_that("`get_censoring_date_match`: take maximum censoring date", {
  censored_twodates <-
    matched_cohort[!is.na(matched_cohort$censoring_pair_max)  &
      !is.na(matched_cohort$death_other_causes),
    ]

  expect_true(
    all(
      censored_twodates$censoring_pair_max <=
        censored_twodates$death_other_causes
    )
  )
})
