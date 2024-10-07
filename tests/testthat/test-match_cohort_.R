#### Tests for match_cohort_()
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
  data_set = sample_cohort,
  col_names = "vaccine_date_2",
  status = c("v", "u")
)

# test for basic expectations - all provided
test_that("`match_cohort`: basic expectations - all provided", {
  matched_cohort <- match_cohort_(
    data_set = sample_cohort,
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
        c(names(sample_cohort), "subclass")
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

# test for exact match
test_that("`match_cohort`: exact match", {
  matched_cohort <- match_cohort_(
    data_set = sample_cohort,
    vacc_status_col = "vaccine_status",
    exact = "sex"
  )
  # same number of categories in "v" and "u"
  expect_identical(
    table(matched_cohort[matched_cohort$vaccine_status == "u", ]$sex),
    table(matched_cohort[matched_cohort$vaccine_status == "v", ]$sex)
  )
})

# test for nearest match
test_that("`match_cohort`: nearest match", {
  matched_cohort <- match_cohort_(
    data_set = sample_cohort,
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
