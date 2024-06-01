#### Tests for get_immunization_date()
data("cohortdata")

# basic expectations
test_that("`get_immunization_date`: Basic expectations", {
  # get immunization dates
  vax_date_col <- c("vaccine_date_1", "vaccine_date_2")
  tf <- as.Date("2044-12-31")
  immunization_delay <- 45 # use extreme outcome delay to test expectations
  limit_delta <- immunization_delay
  cohortdata$immunization <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    immunization_delay = immunization_delay,
    vacc_date_col = vax_date_col,
    end_cohort = tf,
    take_first = FALSE
  )

  expect_vector(
    cohortdata$immunization,
    ptype = as.Date("2045-01-01")
  )

  # mark registers without any vaccine date
  cohortdata$non_vacc <- apply(
    cohortdata[, vax_date_col], 1,
    FUN = function(x) all(is.na(x))
  )

  # check registers with at least one vac but without immunization date
  # must be the registers that didn't satisfy limit date constraint
  diff <- cohortdata[(is.na(cohortdata$immunization)) &
      !(cohortdata$non_vacc),
  ]

  #Differences with outcome
  diff_outcome <- diff[!is.na(diff$death_date), ]

  expect_true(
    all((diff_outcome$death_date - diff_outcome$vaccine_date_1) < limit_delta)
  )

  #Differences without outcome
  diff_no_outcome <- diff[is.na(diff$death_date), ]

  expect_true(
    all((tf - diff_no_outcome$vaccine_date_1) < limit_delta)
  )

  #Differences can only be related to outcome
  expect_identical(
    nrow(diff_outcome) + nrow(diff_no_outcome),
    nrow(diff)
  )

  # for population with immunization date
  # If available vaccine 2: immunization must be
  # equal to vaccine_date_2 + immunization_delay
  # If not available: immunization must be vaccine_date_1 + immunization_delay
  cohortdata$vaccine_status <-
    set_status(
      data = cohortdata,
      col_names = "immunization",
      status = c("v", "u")
    )

  # Test first population without outcome
  vaccinated_no_out <- cohortdata[(cohortdata$vaccine_status == "v") &
      is.na(cohortdata$death_date),
  ]
  vaccinated_no_out$test_date <- ifelse(
    !is.na(vaccinated_no_out$vaccine_date_2) &
      (vaccinated_no_out$vaccine_date_2 <= tf -
          immunization_delay
      ),
    vaccinated_no_out$immunization ==
      vaccinated_no_out$vaccine_date_2 + immunization_delay,
    vaccinated_no_out$immunization ==
      vaccinated_no_out$vaccine_date_1 + immunization_delay
  )

  expect_true(
    all(vaccinated_no_out$test_date)
  )

  # Now population with outcome
  vaccinated_out <- cohortdata[(cohortdata$vaccine_status == "v") &
      !is.na(cohortdata$death_date),
  ]
  vaccinated_out$test_date <- ifelse(!is.na(vaccinated_out$vaccine_date_2) &
      (vaccinated_out$vaccine_date_2 <= vaccinated_out$death_date -
          immunization_delay
      ),
    vaccinated_out$immunization ==
      vaccinated_out$vaccine_date_2 + immunization_delay,
    vaccinated_out$immunization ==
      vaccinated_out$vaccine_date_1 + immunization_delay
  )

  expect_true(
    all(vaccinated_out$test_date)
  )

})

# test for take_first = TRUE
# always must return vaccine_date_1 if no outcome
test_that("`get_immunization_date`: Take first vaccination", {
  # get immunization dates
  vax_date_col <- c("vaccine_date_1", "vaccine_date_2")
  tf <- as.Date("2044-12-31")
  immunization_delay <- 45 # use extreme outcome delay to test expectations
  limit_delta <- immunization_delay
  cohortdata$immunization <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    immunization_delay = immunization_delay,
    vacc_date_col = vax_date_col,
    end_cohort = tf,
    take_first = TRUE
  )

  expect_vector(
    cohortdata$immunization,
    ptype = as.Date("2045-01-01")
  )

  # mark registers without any vaccine date
  cohortdata$non_vacc <- apply(
    cohortdata[, vax_date_col], 1,
    FUN = function(x) all(is.na(x))
  )

  # check registers with at least one vac but without immunization date
  # must be the registers that didn't satisfy limit date constraint
  diff <- cohortdata[(is.na(cohortdata$immunization)) &
      !(cohortdata$non_vacc),
  ]

  #Differences with outcome
  diff_outcome <- diff[!is.na(diff$death_date), ]

  expect_true(
    all((diff_outcome$death_date - diff_outcome$vaccine_date_1) < limit_delta)
  )

  #Differences without outcome
  diff_no_outcome <- diff[is.na(diff$death_date), ]

  expect_true(
    all((tf - diff_no_outcome$vaccine_date_1) < limit_delta)
  )

  # for population with immunization date
  # If available vaccine 1: immunization must be
  # equal vaccine_date_1 + immunization_delay
  cohortdata$vaccine_status <-
    set_status(
      data = cohortdata,
      col_names = "immunization",
      status = c("v", "u")
    )

  vaccinated <- cohortdata[(cohortdata$vaccine_status == "v"), ]
  expect_true(
    all(vaccinated$immunization ==
        vaccinated$vaccine_date_1 + immunization_delay
    )
  )
})

# test coherence immunization date and end_cohort date
test_that("`get_immunization_date`: end_cohort > immunization", {
  date_format <- "%Y-%m-%d"
  t0 <- as.Date("2044-01-01", date_format)
  tf <- as.Date("2044-12-31", date_format)
  immunization_delay <- 14
  data(cohortdata)

  cohortdata$immunization <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    immunization_delay = immunization_delay,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = tf,
    take_first = FALSE
  )

  expect_true(
    all(cohortdata[!is.na(cohortdata$immunization), ]$immunization <= tf)
  )

})

# Tests for censoring
test_that("`get_immunization_date`: Censoring date provided", {
  # cohort start and end time
  start_cohort <- as.Date("2044-01-01")
  end_cohort <- as.Date("2044-12-31")

  # assign immunization date
  cohortdata$immunization_c <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = end_cohort,
    take_first = FALSE
  )

  # immunization must be lower or equal than censoring death for informed
  # censoring
  informed <- cohortdata[!is.na(cohortdata$death_other_causes) &
                           !is.na(cohortdata$immunization_c), ]
  expect_true(
    all(informed$immunization_c <= informed$death_other_causes)
  )
})
