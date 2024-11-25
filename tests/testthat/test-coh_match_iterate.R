#### Tests for coh_match_iterate.R module ####
#### Prepare data for all the tests ####
data("cohortdata")
start_cohort <- as.Date("2021-01-01")
end_cohort <- as.Date("2021-12-31")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 20000
set.seed(123) # use fixed seed
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort <- cohortdata[sample_indices, ]
rownames(sample_cohort) <- NULL

nearest <- c(age = 1)
exact <- "sex"
outcome_date_col <- "death_date"
censoring_date_col <- "death_other_causes"
vacc_date_col <- "vaccine_date_2"
immunization_delay <- 15
vacc_status_col <- "vaccine_status"
immunization_date_col <- "immunization_date"

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

# Generate id to control matches
sample_cohort$match_id <- seq_len(nrow(sample_cohort))

# Match sample cohort
# Warnings can be generated but are not
# taken into account at this point
matched <- capture_warnings(match_cohort_(
  data_set = sample_cohort,
  vacc_status_col = vacc_status_col,
  nearest = nearest,
  exact = exact
))$result

# Adjust exposition times of matched cohort
adjusted_0 <- adjust_exposition(matched_cohort = matched,
                                outcome_date_col = outcome_date_col,
                                censoring_date_col = censoring_date_col,
                                immunization_date = immunization_date_col,
                                start_cohort = start_cohort,
                                end_cohort = end_cohort)
#### Tests for the rematch() ####
# Test for basic expectations and correctness of algorithm
test_that("`rematch`: Correctness", {
  removed_i <- matched[!(matched$match_id %in% adjusted_0$match_id), ]
  # iteration on removed vaccinated

  # There are not enough unvaccinated units to find a pair for the
  # removed vaccinated, new ones have to be manually generated for
  # this test
  # Create 5 unvaccinated from removed vaccinated
  # (these should be re-matched since all the features are the same)
  virtual_u <- head(removed_i[removed_i$vaccine_status == "v", ], 5)
  virtual_u$vaccine_status <- "u"
  virtual_u$vaccine_date1 <- as.Date(NA)
  virtual_u$vaccine_date2 <- as.Date(NA)
  virtual_u$immunization_date <- as.Date(NA)
  virtual_u$vaccine_1 <- "NULL"
  virtual_u$vaccine_2 <- "NULL"
  virtual_u$match_id <- virtual_u$match_id + nrow(sample_cohort)
  virtual_u <- virtual_u[, names(sample_cohort)]
  sample_cohort <- rbind(sample_cohort, virtual_u)

  output <- capture_warnings(rematch_(
    all = sample_cohort,
    adjusted = adjusted_0,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    immunization_date_col = immunization_date_col,
    removed_i = removed_i,
    vacc_status_col = vacc_status_col,
    rematch_status = "v",
    control_status = "u",
    nearest = nearest,
    exact = exact,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    im = 0
  ))
  rm_v <- output$result

  adjusted_fv <- rm_v$adjusted
  adjusted_v_it <- rm_v$adjusted_i_s

  # test for iteration on vaccinated
  expect_gt(
    # non-empty dataframes expected for this test (first iteration)
    nrow(adjusted_v_it), 0
  )

  expect_identical(
    # adjusted cohort after iteration must be bigger
    nrow(adjusted_fv), nrow(adjusted_0) + nrow(adjusted_v_it)
  )

  expect_true(
    # new cohort must contain previous cohort
    all(adjusted_0$match_id %in% adjusted_fv$match_id)
  )

  expect_false(
    # duplicated registers are not allowed
    any(adjusted_v_it$match_id %in% adjusted_0$match_id)
  )

  # iteration on removed unvaccinated
  output <- capture_warnings(rematch_(
    all = sample_cohort,
    adjusted = adjusted_fv,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    immunization_date_col = immunization_date_col,
    removed_i = removed_i,
    vacc_status_col = vacc_status_col,
    rematch_status = "u",
    control_status = "v",
    nearest = nearest,
    exact = exact,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    im = 0
  ))

  rm_u <- output$result

  adjusted_fu <- rm_u$adjusted
  adjusted_u_it <- rm_u$adjusted_i_s

  # test for iteration on vaccinated
  expect_gt(
    # non-empty dataframes expected for this test (first iteration)
    nrow(adjusted_u_it), 0
  )

  expect_identical(
    # adjusted cohort after iteration must be bigger
    nrow(adjusted_fu), nrow(adjusted_fv) + nrow(adjusted_u_it)
  )

  expect_true(
    # new cohort must contain previous cohort
    all(adjusted_fv$match_id %in% adjusted_fu$match_id)
  )

  expect_false(
    # duplicated registers are not allowed
    any(adjusted_u_it$match_id %in% adjusted_fv$match_id)
  )
})

# Test of conditions to avoid rematch
test_that("`rematch_`: return empty when no unmatched registers", {
  removed_i <- matched[!(matched$match_id %in% adjusted_0$match_id), ]

  # all = adjusted mimics no unmatched registers
  output <- capture_warnings(rematch_(
    all = adjusted_0,
    adjusted = adjusted_0,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    immunization_date_col = immunization_date_col,
    removed_i = removed_i,
    vacc_status_col = vacc_status_col,
    rematch_status = "v",
    control_status = "u",
    nearest = nearest,
    exact = exact,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    im = 0
  ))

  rm_v <- output$result

  expect_identical(
    nrow(rm_v$adjusted_i_s), 0L
  )
})

# Test for warning message when no matches found
test_that("`rematch`: tryCatch error handle", {
  removed_i <- matched[!(matched$match_id %in% adjusted_0$match_id), ]
  unmatched <- sample_cohort[
    !(sample_cohort$match_id %in% adjusted_0$match_id),
  ]
  # Suposse there is only one last unit to match
  virtual_last <- head(removed_i[removed_i$vaccine_status == "v", ], 1)
  virtual_last$vaccine_status <- "u"
  virtual_last$vaccine_date1 <- as.Date(NA)
  virtual_last$vaccine_date2 <- as.Date(NA)
  virtual_last$immunization_date <- as.Date(NA)
  virtual_last$vaccine_1 <- "NULL"
  virtual_last$vaccine_2 <- "NULL"
  virtual_last$match_id <- virtual_last$match_id + nrow(sample_cohort)
  virtual_last <- virtual_last[, names(sample_cohort)]
  # Change sex to be sure that it won't be matched
  virtual_last$sex <- "R"
  adjusted_temp <- subset(adjusted_0, select = names(sample_cohort))
  # The new cohort is the adjusted one + the additional unvaccinated case
  all <- rbind(adjusted_temp, virtual_last)

  expect_warning(
    rematch_(
      all = all,
      adjusted = adjusted_0,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      immunization_date_col = immunization_date_col,
      removed_i = removed_i,
      vacc_status_col = vacc_status_col,
      rematch_status = "v",
      control_status = "u",
      nearest = NULL,
      exact = exact,
      start_cohort = start_cohort,
      end_cohort = end_cohort,
      im = 0
    )
  )
})

#### Tests for the iterate_match() ####
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
    nrow(output$result$adjusted), nrow(adjusted_0)
  )

  expect_true(
    # new cohort must contain previous cohort
    all(adjusted_0$match_id %in% output$result$adjusted$match_id)
  )

  # Warnings are also tested for completeness
  expect_gt(
    length(output$warnings), 0
  )
})
