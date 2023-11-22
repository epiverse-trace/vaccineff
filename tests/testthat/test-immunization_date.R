#### Tests for get_immunization_date()
# snapshot test to test edited implementations
test_that("`get_immunization_date`: Snapshot test", {
  data("cohortdata")
  cohortdata <- as.data.frame(cohortdata)

  # get immunization dates
  immunization <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = 0,
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = as.Date("2044-12-31"),
    take_first = FALSE
  )

  expect_snapshot(
    head(immunization, 15)
  )
})

# basic expectations
test_that("`get_immunization_date`: Basic expectations", {
  data("cohortdata")
  cohortdata <- as.data.frame(cohortdata)

  # get immunization dates
  vax_date_col <- c("vaccine_date_1", "vaccine_date_2")
  immunization_delay <- 14
  outcome_delay <- 30 # use extreme outcome delay to test expectations
  limit_delta <- immunization_delay + outcome_delay
  cohortdata$immunization <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = outcome_delay,
    immunization_delay = immunization_delay,
    vacc_date_col = vax_date_col,
    end_cohort = as.Date("2044-12-31"),
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
                       (cohortdata$non_vacc==FALSE),
  ]

  expect_true(
    all((diff$death_date - diff$vaccine_date_1) < limit_delta)
  )

  # for population with immunization date
  # If available vaccine 2: immunization must be
  # immunization_date = vaccine_date_2 + immunization_delay
  # If not available: immunization must be
  # immunization_date = vaccine_date_1 + immunization_delay
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
  vaccinated_no_out$test_date <- ifelse (!is.na(vaccinated_no_out$vaccine_date_2),
    vaccinated_no_out$immunization == vaccinated_no_out$vaccine_date_2 + immunization_delay,
    vaccinated_no_out$immunization == vaccinated_no_out$vaccine_date_1 + immunization_delay
  )

  expect_true(
    all(vaccinated_no_out$test_date)
  )

  # Now population with outcome
  vaccinated_out <- cohortdata[(cohortdata$vaccine_status == "v") &
                                    !is.na(cohortdata$death_date),
  ]
  vaccinated_out$test_date <- ifelse (!is.na(vaccinated_out$vaccine_date_2) &
    (vaccinated_out$vaccine_date_2 <= vaccinated_out$death_date -
      immunization_delay -
      outcome_delay
     ),
    vaccinated_out$immunization == vaccinated_out$vaccine_date_2 + immunization_delay,
    vaccinated_out$immunization == vaccinated_out$vaccine_date_1 + immunization_delay
  )

  expect_true(
    all(vaccinated_out$test_date)
  )

})

# test for take_first = TRUE
# always must return vaccine_date_1 if no outcome
test_that("`get_immunization_date`: Take first vaccination", {
  data("cohortdata")
  cohortdata <- as.data.frame(cohortdata)

  # get immunization dates
  vax_date_col <- c("vaccine_date_1", "vaccine_date_2")
  immunization_delay <- 14
  outcome_delay <- 30 # use extreme outcome delay to test expectations
  limit_delta <- immunization_delay + outcome_delay
  cohortdata$immunization <- get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = outcome_delay,
    immunization_delay = immunization_delay,
    vacc_date_col = vax_date_col,
    end_cohort = as.Date("2044-12-31"),
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
                       (cohortdata$non_vacc==FALSE),
  ]

  expect_true(
    all((diff$death_date - diff$vaccine_date_1) < limit_delta)
  )

  # for population with immunization date
  # If available vaccine 1: immunization must be
  # immunization_date = vaccine_date_1 + immunization_delay
  cohortdata$vaccine_status <-
    set_status(
      data = cohortdata,
      col_names = "immunization",
      status = c("v", "u")
    )

  vaccinated <- cohortdata[(cohortdata$vaccine_status == "v"), ]
  expect_true(
    all(vaccinated$immunization == vaccinated$vaccine_date_1 + immunization_delay)
  )
})

# test for end_cohort date no longer than 2100
test_that("`get_immunization_date`: end_cohort > max_date", {
  data("cohortdata")
  cohortdata <- as.data.frame(cohortdata)

  # get immunization dates
  expect_warning(
    get_immunization_date(
      data = cohortdata,
      outcome_date_col = "death_date",
      outcome_delay = 0,
      immunization_delay = 14,
      vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
      end_cohort = as.Date("2101-12-31"),
      take_first = FALSE
    )
  )
})
