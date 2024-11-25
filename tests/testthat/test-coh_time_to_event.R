#### Tests for coh_time_to_event.R module ####
#### Prepare data for all the tests ####
data("cohortdata")
end_cohort <- as.Date("2021-12-31")

# sample cohort to make tests faster - take a bigger sample
sample_size <- 10000
set.seed(123) # use fixed seed to avoid problems with snapshots
sample_indices <- sample(nrow(cohortdata), sample_size)
sample_cohort <- cohortdata[sample_indices, ]
rownames(sample_cohort) <- NULL

# Create vaccineff data
vaccineff_data <- make_vaccineff_data(data_set = sample_cohort,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  vacc_date_col = "vaccine_date_2",
  vaccinated_status = "v",
  unvaccinated_status = "u",
  immunization_delay = 15,
  end_cohort = end_cohort,
  match = TRUE,
  exact = c("age", "sex"),
  nearest = NULL
)

# Data for the first test
# assign immunization date
data_test1 <- sample_cohort
data_test1$immunization_date <- get_immunization_date(
  data_set = data_test1,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  immunization_delay = 15,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = end_cohort,
  take_first = FALSE
)
start_cohort <- min(data_test1$immunization_date, na.rm = TRUE)

data_test1$t0_follow_up <- as.Date(
  ifelse(is.na(data_test1$immunization_date),
    yes = as.character(start_cohort),
    no = as.character(data_test1$immunization_date)
  )
)

# Data for the second test
data_test2 <- vaccineff_data$matching$match
tags <- linelist::tags(data_test2)

#### Tests for the get_exposure_time() ####
# Snapshot tests to check cleaned up implementation
test_that("Snapshot test for `get_exposure_time`", {

  # calculate time to death
  exposure_time <- get_exposure_time(
    data_set = data_test1,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    end_cohort = end_cohort
  )

  expect_snapshot(
    head(exposure_time, 20)
  )
})

# Correctness test
test_that("`get_exposure_time`: correctness", {

  # calculate time to death
  data_test1$exposure_time <- get_exposure_time(
    data_set = data_test1,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    end_cohort = end_cohort
  )

  # Check for NAs
  expect_false(anyNA(data_test1$exposure_time))

  #Check for informed outcome or censoring dates
  informed_data <- data_test1[!is.na(data_test1$death_date) |
      !is.na(data_test1$death_other_causes),
  ]
  informed_data$tf <- pmin(informed_data$death_date,
    informed_data$death_other_causes, na.rm = TRUE
  )
  informed_data$exposure_time_test <-
    informed_data$tf - informed_data$t0_follow_up

  expect_true(
    all(informed_data$exposure_time_test == informed_data$exposure_time)
  )
  #Check for non-informed outcome or censoring dates
  non_informed_data <- data_test1[is.na(data_test1$death_date) &
      is.na(data_test1$death_other_causes),
  ]

  non_informed_data$exposure_time_test <-
    end_cohort - non_informed_data$t0_follow_up

  expect_true(
    all(non_informed_data$exposure_time_test ==
          non_informed_data$exposure_time)
  )
})

#### Tests for the get_time_to_event_at() ####
# Basic expectations of `get_time_to_event_at()`
test_that("`get_time_to_event_at`: basic expectations", {
  data_trunc <- get_time_to_event_at(
    data_set = data_test2,
    outcome_date_col = tags$outcome_date_col,
    censoring_date_col = tags$censoring_date_col,
    end_cohort = vaccineff_data$end_cohort,
    at = 90
  )
  expect_identical(max(data_trunc$time_to_event), 90)
})
