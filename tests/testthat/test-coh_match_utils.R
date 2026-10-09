#### Tests for coh_match_utils.R module ####
#### Prepare data for all tests ####
data("cohortdata")
start_cohort <- as.Date("2021-01-01")
end_cohort <- as.Date("2021-12-31")

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
matched_cohort <- capture_warnings(match_cohort_(data_set = sample_cohort_l,
  vacc_status_col = "vaccine_status",
  exact = "sex"
))$result

# Add column with minimum censoring date for pair
matched_cohort$censoring_pair <-  as.Date(match_pair_info(
  data_set = matched_cohort,
  column_to_match = "death_other_causes",
  criteria = "min"
))


#### Tests for match_cohort_() ####
# Test for basic expectations - all provided
test_that("`match_cohort`: basic expectations - all provided", {
  matched_cohort <- capture_warnings(match_cohort_(
    # match_cohort_ can produce warnings for few units. This are
    # ignored in this test
    data_set = sample_cohort_s,
    vacc_status_col = "vaccine_status",
    exact = "sex",
    nearest = c(age = 1)
  ))$result
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
test_that("`match_cohort_`: exact match", {
  matched_cohort <- capture_warnings(match_cohort_(
    # match_cohort_ can produce warnings for few units. This are
    # ignored in this test
    data_set = sample_cohort_s,
    vacc_status_col = "vaccine_status",
    exact = "sex"
  ))$result
  # same number of categories in "v" and "u"
  expect_identical(
    table(matched_cohort[matched_cohort$vaccine_status == "u", ]$sex),
    table(matched_cohort[matched_cohort$vaccine_status == "v", ]$sex)
  )
})

# Test for nearest match
test_that("`match_cohort`: nearest match", {
  matched_cohort <- capture_warnings(match_cohort_(
    # match_cohort_ can produce warnings for few units. This are
    # ignored in this test
    data_set = sample_cohort_s,
    vacc_status_col = "vaccine_status",
    nearest = c(age = 3)
  ))$result
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
# Unit test for `match_summary()` on deterministic inputs. The exact counts
# produced by `match_cohort()` depend on the `{MatchIt}` version, so the
# summary contract is checked here without relying on `{MatchIt}`.
test_that("`match_summary`: counts by vaccine status", {
  all <- data.frame(
    vaccine_status = c("u", "u", "u", "u", "v", "v", "v")
  )
  matched <- data.frame(
    vaccine_status = c("u", "u", "v")
  )
  summary <- match_summary(
    all = all,
    matched = matched,
    vacc_status_col = "vaccine_status"
  )
  # Rows describe the full, matched and unmatched populations
  expect_identical(rownames(summary), c("All", "Matched", "Unmatched"))
  # Counts by vaccine status
  expect_equal(summary[["u"]], c(4L, 2L, 2L))
  expect_equal(summary[["v"]], c(3L, 1L, 2L))
})

# Summary for the matched cohort has to be consistent with the matching inputs
test_that("`match_cohort`: summary is consistent", {
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

  # Summary of the matching result
  matched <- matching$match
  summary <- match_summary(all = sample_cohort_imm,
    matched = matched,
    vacc_status_col = "vaccine_status"
  )

  # Rows describe the full, matched and unmatched populations
  expect_identical(rownames(summary), c("All", "Matched", "Unmatched"))
  # The "All" population is the composition of the entire cohort
  expect_equal(
    summary["All", "u"],
    sum(sample_cohort_imm$vaccine_status == "u")
  )
  expect_equal(
    summary["All", "v"],
    sum(sample_cohort_imm$vaccine_status == "v")
  )
  # Matched and unmatched populations partition the full cohort
  expect_equal(
    summary["Matched", "u"] + summary["Unmatched", "u"],
    summary["All", "u"]
  )
  expect_equal(
    summary["Matched", "v"] + summary["Unmatched", "v"],
    summary["All", "v"]
  )
  # Matched counts reflect the matched cohort
  expect_equal(
    summary["Matched", "u"],
    sum(matched$vaccine_status == "u")
  )
  expect_equal(
    summary["Matched", "v"],
    sum(matched$vaccine_status == "v")
  )
  # A non-empty matched population is expected for this cohort
  expect_gt(summary["Matched", "u"], 0)
})
