#### Tests for get_immunization_date()

# snapshot test to test edited implementations
test_that("`get_immunization_date`: Snapshot test", {
  data("cohortdata")
  cohortdata <- as.data.frame(cohortdata)

  # get immunization dates
  immunization_death <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = 0,
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = as.Date("2044-12-31"),
    take_first = FALSE
  )

  expect_snapshot(
    head(immunization_death, 15)
  )
})

# snapshot test to test edited implementations
test_that("`get_immunization_date`: Basic expectations", {
  data("cohortdata")
  cohortdata <- as.data.frame(cohortdata)
  # filter out all real deaths
  cohortdata <- cohortdata[is.na(cohortdata$death_date), ]

  # get immunization dates
  vax_date_col <- c("vaccine_date_1", "vaccine_date_2")
  immunization_delay <- 14
  immunization_death <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = 0,
    immunization_delay = immunization_delay,
    vacc_date_col = vax_date_col,
    end_cohort = as.Date("2044-12-31"),
    take_first = FALSE
  )

  expect_vector(
    immunization_death,
    ptype = as.Date("2045-01-01")
  )

  # expect NAs for those not vaccinated at least once
  imm_date_manual <- apply(
    cohortdata[, vax_date_col], 1,
    FUN = function(x) all(is.na(x))
  )
  # running on first 15 expectations
  expect_equal(
    is.na(head(immunization_death, 15)),
    head(imm_date_manual, 15),
    ignore_attr = TRUE
  )

  # expect that the immunization date is after the last vaccination date
  # given a delay of 14 days
  vax_difference <- immunization_death - as.Date(cohortdata$vaccine_date_2)
  vax_difference <- as.numeric(vax_difference[!is.na(vax_difference)])
  expect_true(
    all(vax_difference == immunization_delay)
  )
})

# snapshot test to test edited implementations
test_that("`get_immunization_date`: Take first vaccination", {
  data("cohortdata")
  cohortdata <- as.data.frame(cohortdata)
  # filter out all real deaths
  cohortdata <- cohortdata[is.na(cohortdata$death_date), ]

  # get immunization dates
  vax_date_col <- c("vaccine_date_1", "vaccine_date_2")
  immunization_delay <- 14
  immunization_death <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = 0,
    immunization_delay = immunization_delay,
    vacc_date_col = vax_date_col,
    end_cohort = as.Date("2044-12-31"),
    take_first = TRUE
  )

  expect_vector(
    immunization_death,
    ptype = as.Date("2023-01-01")
  )

  # expect NAs for those not vaccinated at least once
  imm_date_manual <- apply(
    cohortdata[, vax_date_col], 1,
    FUN = function(x) all(is.na(x))
  )
  # running on first 15 expectations
  expect_equal(
    is.na(head(immunization_death, 15)),
    head(imm_date_manual, 15),
    ignore_attr = TRUE
  )

  # expect that the immunization date is after the first vaccination date
  # given a delay of 14 days
  min_vax_date <- apply(cohortdata[, vax_date_col], 1, min)
  vax_difference <- immunization_death - as.Date(min_vax_date)
  vax_difference <- as.numeric(vax_difference[!is.na(vax_difference)])
  expect_true(
    all(vax_difference == immunization_delay)
  )
})
