#### Tests for get_censoring_date_match()

## prepare data
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 1000
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort <- cohortdata[sample_indices, ]
rownames(sample_cohort) <- NULL

# assign vaccination status
sample_cohort$vaccine_status <- set_status(
  data = sample_cohort,
  col_names = "vaccine_date_2",
  status = c("v", "u")
)

# test for basic expectations - all provided
test_that("`match_cohort`: basic expectations - all provided", {
  matched_cohort <- match_cohort(
    data = sample_cohort,
    status_vacc_col = "vaccine_status",
    exact = "sex",
    nearest = "age",
    caliper = c(age = 1)
  )
  # expect dataframe
  expect_s3_class(matched_cohort, "data.frame")

  # error argumentes not provided
  expect_error(
    match_cohort(
      data = sample_cohort,
      status_vacc_col = "vaccine_status"
    ),
    regexp =
      "`exact` and `nearest` cannot be NULL. At least one must be provided"
  )

  # keep all the columns and add only "subclass" and "prop_score"
  expect_true(
    all(
      names(matched_cohort) %in%
        c(names(sample_cohort), "subclass", "prop_score")
    )
  )

  # even number of couples
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

# test for exact match
test_that("`match_cohort`: exact match", {
  matched_cohort <- match_cohort(
    data = sample_cohort,
    status_vacc_col = "vaccine_status",
    exact = "sex"
  )
  # same number of categories in "v" and "u"
  expect_identical(
    table(matched_cohort[matched_cohort$vaccine_status == "u", ]$sex),
    table(matched_cohort[matched_cohort$vaccine_status == "v", ]$sex)
  )

  expect_warning(
    match_cohort(
      data = sample_cohort,
      status_vacc_col = "vaccine_status",
      exact = "sex",
      caliper = c(age = 1)
    ),
    regexp = "`caliper` ignored caused by `nearest` not provided"
  )
})

# test for exact match
test_that("`match_cohort`: nearest match", {
  matched_cohort <- match_cohort(
    data = sample_cohort,
    status_vacc_col = "vaccine_status",
    nearest = "age",
    caliper = c(age = 3)
  )
  # even number of couples
  expect_setequal(
    nrow(matched_cohort),
    length(unique(matched_cohort$subclass)) * 2
  )

  # same number of vaccinated and unvaccinated
  expect_identical(
    nrow(matched_cohort[matched_cohort$vaccine_status == "u", ]),
    nrow(matched_cohort[matched_cohort$vaccine_status == "v", ])
  )

  # expect error caliper not passed
  expect_error(
    match_cohort(
      data = sample_cohort,
      status_vacc_col = "vaccine_status",
      nearest = "age"
    ),
    regexp =
      "`caliper` must be provided together with `nearest`"
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
