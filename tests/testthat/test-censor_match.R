#### Tests for censor_match()

## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# sample cohort to make tests faster - take a bigger sample
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
matched_cohort <- match_cohort(data = sample_cohort,
  status_vacc_col = "vaccine_status",
  exact = "sex"
)
# add column with censoring date for match
matched_cohort$censoring_date <- censor_match(
  data = matched_cohort,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes"
)

# snapshot test basic expectations
test_that("`censor_match`: basic expectations", {
  # expect date
  expect_vector(
    matched_cohort$censoring_date,
    ptype = as.Date("2045-01-01")
  )
  # filter registers with censoring date informed
  censored_original <-
    matched_cohort[!is.na(matched_cohort$death_other_causes), ]
  # filter registers with censoring date created after matching
  censored_match <-
    matched_cohort[!is.na(matched_cohort$censoring_date), ]

  # All the subclass IDs in censored_match must be contained in
  # censored_original
  expect_true(
    all(censored_match$subclass %in% censored_original$subclass)
  )
})

# Always take the minimum censoring date per couple
test_that("`censor_match`: take minimum censoring date", {
  censored_twodates <-
    matched_cohort[!is.na(matched_cohort$censoring_date)  &
      !is.na(matched_cohort$death_other_causes),
    ]

  expect_true(
    all(
      censored_twodates$censoring_date <= censored_twodates$death_other_causes
    )
  )
})

# If one of the members of the couple has an outcome date before
# the censoring date of the other. Do not censor, set censoring date
# for the member with outcome as NA
test_that("`censor_match`: not censoring cases", {
  censored_and_outcome <-
    matched_cohort[!is.na(matched_cohort$censoring_date)  &
                     !is.na(matched_cohort$death_date),
    ]

  expect_true(
    all(
      censored_and_outcome$censoring_date > censored_and_outcome$death_date
    )
  )
})
