#### Tests for coh_match_utils.R module ####
#### Prepare data for all tests ####
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# tests for match_cohort_() and match_summary can use the small data
sample_size <- 1000
set.seed(123) #use fixed seed to avoid problems with snapshots
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort_s <- cohortdata[sample_indices, ]
rownames(sample_cohort_s) <- NULL

# assign vaccination status
sample_cohort_s$vaccine_status <- set_status(
  data_set = sample_cohort_s,
  col_names = "vaccine_date_2",
  status = c("v", "u")
)

# match_pair_info requires a slightly bigger cohort
# additional data preparation
sample_size <- 5000
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort_l <- cohortdata[sample_indices, ]
rownames(sample_cohort_l) <- NULL

# Assign vaccination status
sample_cohort_l$vaccine_status <- set_status(
  data_set = sample_cohort_l,
  col_names = "vaccine_date_2",
  status = c("v", "u")
)

# Match cohort
matched_cohort <- match_cohort_(data_set = sample_cohort_l,
  vacc_status_col = "vaccine_status",
  exact = "sex"
)

# Add column with minimum censoring date for pair
matched_cohort$censoring_pair <-  as.Date(match_pair_info(
  data_set = matched_cohort,
  column_to_match = "death_other_causes",
  criteria = "min"
))


#### Tests for match_cohort_() ####
# Test for basic expectations - all provided
test_that("`match_cohort`: basic expectations - all provided", {
  matched_cohort <- match_cohort_(
    data_set = sample_cohort_s,
    vacc_status_col = "vaccine_status",
    exact = "sex",
    nearest = c(age = 1)
  )
  # expect dataframe
  expect_s3_class(matched_cohort, "data.frame")

  # keep all the columns and add only "subclass"
  expect_true(
    all(
      names(matched_cohort) %in%
        c(names(sample_cohort_s), "subclass")
    )
  )

  # even number of pairs
  expect_setequal(
    nrow(matched_cohort),
    length(unique(matched_cohort$subclass)) * 2
  )

  # same number of vaccinated and unvaccinated
  expect_identical(
    nrow(matched_cohort[matched_cohort$vaccine_status == "u", ]),
    nrow(matched_cohort[matched_cohort$vaccine_status == "v", ])
  )

  # same number of sex in "v" and "u"
  expect_identical(
    table(matched_cohort[matched_cohort$vaccine_status == "u", ]$sex),
    table(matched_cohort[matched_cohort$vaccine_status == "v", ]$sex)
  )

  # same number of sex in "v" and "u"
  expect_identical(
    table(matched_cohort[matched_cohort$vaccine_status == "u", ]$sex),
    table(matched_cohort[matched_cohort$vaccine_status == "v", ]$sex)
  )

  # ages are not equal
  expect_false(
    isTRUE(
      all.equal(
        table(matched_cohort[matched_cohort$vaccine_status == "u", ]$age),
        table(matched_cohort[matched_cohort$vaccine_status == "v", ]$age)
      )
    )
  )
})

# Test for exact match
test_that("`match_cohort`: exact match", {
  matched_cohort <- match_cohort_(
    data_set = sample_cohort_s,
    vacc_status_col = "vaccine_status",
    exact = "sex"
  )
  # same number of categories in "v" and "u"
  expect_identical(
    table(matched_cohort[matched_cohort$vaccine_status == "u", ]$sex),
    table(matched_cohort[matched_cohort$vaccine_status == "v", ]$sex)
  )
})

# Test for nearest match
test_that("`match_cohort`: nearest match", {
  matched_cohort <- match_cohort_(
    data_set = sample_cohort_s,
    vacc_status_col = "vaccine_status",
    nearest = c(age = 3)
  )
  # even number of pairs
  expect_setequal(
    nrow(matched_cohort),
    length(unique(matched_cohort$subclass)) * 2
  )

  # same number of vaccinated and unvaccinated
  expect_identical(
    nrow(matched_cohort[matched_cohort$vaccine_status == "u", ]),
    nrow(matched_cohort[matched_cohort$vaccine_status == "v", ])
  )

  # ages are not equal due to caliper
  expect_false(
    isTRUE(
      all.equal(
        table(matched_cohort[matched_cohort$vaccine_status == "u", ]$age),
        table(matched_cohort[matched_cohort$vaccine_status == "v", ]$age)
      )
    )
  )
})

#### Tests for match_pair_info() ####
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
test_that("`get_censoring_date_match`: take maximum censoring date", {
  # Add column with censoring date for pair
  matched_cohort$censoring_pair_max <-  as.Date(match_pair_info(
    data_set = matched_cohort,
    column_to_match = "death_other_causes",
    criteria = "max"
  ))

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

#### Tests for match_summary() ####
# Snapshot for summary comparing only two dataframes
test_that("`match_cohort`: summary snapshot", {
  # Create `data.frame` with information on immunization
  sample_cohort_imm <- make_immunization(
    data_set = sample_cohort_s,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    vacc_date_col = "vaccine_date_2",
    vacc_name_col = NULL,
    vaccinated_status = "v",
    unvaccinated_status = "u",
    immunization_delay = 14,
    end_cohort = end_cohort
  )
  # Match the data directly using match_cohort
  output <- capture_warnings(match_cohort(
    data_set = sample_cohort_imm,
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

  # Extract results from matching after capturing warning
  matching <- output$result

  # snapshot for summary
  matched <- matching$match
  summary <- match_summary(all = sample_cohort_imm,
    matched = matched,
    vacc_status_col = "vaccine_status"
  )
  expect_snapshot(summary)
})
