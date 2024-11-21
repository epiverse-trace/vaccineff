#### Tests for coh_immunization.R module ####
#### Prepare data for all the tests ####
data("cohortdata")
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")
# calculate immunization date
cohortdata$immunization <- get_immunization_date(
  data_set = cohortdata,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  immunization_delay = 14,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = as.Date("2044-12-31"),
  take_first = FALSE
)

#### Tests for make_immunization() ####
# test for basic expectations
test_that("`make_immunization`: basic expectations", {
  # Create rows with information on immunization
  # make_immunization creates a new data frame, so it's safe
  # to rewrite cohortdata inside the test
  cohortdata <- make_immunization(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = "vaccine_date_2",
    vacc_name_col = NULL,
    end_cohort = end_cohort,
    vaccinated_status = "vaccinated",
    unvaccinated_status = "unvaccinated"
  )
  expect_true(
    all(c("immunization_date", "vaccine_status") %in% names(cohortdata))
  )
  # custom names for vaccine status
  cohortdata <- make_immunization(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = "vaccine_date_2",
    vacc_name_col = NULL,
    end_cohort = end_cohort,
    vaccinated_status = "vaccinated",
    unvaccinated_status = "unvaccinated"
  )

  expect_true(
    all(c("vaccinated", "unvaccinated") %in% cohortdata$vaccine_status)
  )
})

# test for more than one vaccination column
test_that("`make_immunization`: two vaccination columns", {
  # Create rows with information on immunizing vaccine
  cohortdata <- make_immunization(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    vacc_name_col = NULL,
    end_cohort = end_cohort,
    vaccinated_status = "vaccinated",
    unvaccinated_status = "unvaccinated"
  )
  expect_true(
    all(c("immunization_date", "vaccine_status", "immunizing_vaccine") %in%
          names(cohortdata))
  )
  expect_true(
    all(c("vaccine_date_1", "vaccine_date_2") %in%
          cohortdata$immunizing_vaccine)
  )
})

test_that("`make_immunization`: vaccine names provided", {
  # vaccine name provided
  cohortdata <- make_immunization(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    immunization_delay = 14,
    vacc_date_col = "vaccine_date_1",
    vacc_name_col = "vaccine_1",
    end_cohort = end_cohort,
    vaccinated_status = "v",
    unvaccinated_status = "u"
  )
  expect_true(
    "immunizing_vaccine_name" %in% names(cohortdata)
  )
  expect_true(
    all(c("BRAND1", "BRAND2") %in%
          cohortdata$immunizing_vaccine_name)
  )
})

#### Tests for get_immunization_date() ####
# Basic expectations
test_that("`get_immunization_date`: Basic expectations", {
  # get immunization dates
  vacc_date_col <- c("vaccine_date_1", "vaccine_date_2")
  tf <- as.Date("2044-12-31")
  immunization_delay <- 45 # use extreme outcome delay to test expectations
  limit_delta <- immunization_delay
  cohortdata$immunization <- get_immunization_date(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = NULL,
    immunization_delay = immunization_delay,
    vacc_date_col = vacc_date_col,
    end_cohort = tf,
    take_first = FALSE
  )

  expect_vector(
    cohortdata$immunization,
    ptype = as.Date("2045-01-01")
  )

  # mark registers without any vaccine date
  cohortdata$non_vacc <- apply(
    cohortdata[, vacc_date_col], 1,
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
      data_set = cohortdata,
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

# Test for take_first = TRUE
# always must return vaccine_date_1 if no outcome
test_that("`get_immunization_date`: Take first vaccination", {
  # get immunization dates
  vacc_date_col <- c("vaccine_date_1", "vaccine_date_2")
  tf <- as.Date("2044-12-31")
  immunization_delay <- 45 # use extreme outcome delay to test expectations
  limit_delta <- immunization_delay
  cohortdata$immunization <- get_immunization_date(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = NULL,
    immunization_delay = immunization_delay,
    vacc_date_col = vacc_date_col,
    end_cohort = tf,
    take_first = TRUE
  )

  expect_vector(
    cohortdata$immunization,
    ptype = as.Date("2045-01-01")
  )

  # mark registers without any vaccine date
  cohortdata$non_vacc <- apply(
    cohortdata[, vacc_date_col], 1,
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
      data_set = cohortdata,
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

# Test coherence immunization date and end_cohort date
test_that("`get_immunization_date`: end_cohort > immunization", {
  date_format <- "%Y-%m-%d"
  t0 <- as.Date("2044-01-01", date_format)
  tf <- as.Date("2044-12-31", date_format)
  immunization_delay <- 14
  data(cohortdata)

  cohortdata$immunization <- get_immunization_date(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = NULL,
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

  # assign immunization date
  cohortdata$immunization_c <- get_immunization_date(
    data_set = cohortdata,
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

#### Tests for get_immunization_dose() ####
# calculate immunization date
test_that("`immunization_dose`: Basic expectations", {
  # get the immunization dose
  vacc_date_col <- c("vaccine_date_1", "vaccine_date_2")
  immunization_dose <- get_immunization_dose(
    data_set = cohortdata,
    immunization_date_col = "immunization",
    vacc_date_col = vacc_date_col,
    immunization_delay = 14
  )

  expect_vector(
    immunization_dose,
    ptype = character()
  )
  expect_length(
    immunization_dose, nrow(cohortdata)
  )
  expect_setequal(
    unique(immunization_dose),
    c(vacc_date_col, NA)
  )
  expect_snapshot(
    head(immunization_dose, 20)
  )
})

#### Tests for get_immunization_vaccine() ####
test_that("`immunization_vaccine`: Basic expectations", {
  which_vaccine <- get_immunization_vaccine(
    data_set = cohortdata,
    immunization_date_col = "immunization",
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    vacc_name_col = c("vaccine_1", "vaccine_2"),
    immunization_delay = 14
  )
  expect_vector(
    which_vaccine, character()
  )
  expect_length(
    which_vaccine,
    nrow(cohortdata)
  )
  expect_setequal(
    which_vaccine,
    unique(cohortdata$vaccine_1, cohortdata$vaccine_2)
  )
  expect_snapshot(
    head(which_vaccine, 30)
  )
})
