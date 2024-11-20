#### Tests for coh_time_to_event.R module ####

#### Tests for the get_exposition_time() ####
# Prepare data for first tests
data("cohortdata")

end_cohort <- as.Date("2044-12-31")
# assign immunization date
cohortdata$immunization_date <- get_immunization_date(
  data_set = cohortdata,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  immunization_delay = 15,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = end_cohort,
  take_first = FALSE
)

start_cohort <- min(cohortdata$immunization_date, na.rm = TRUE)

cohortdata$t0_follow_up <- as.Date(
  ifelse(is.na(cohortdata$immunization_date),
    yes = as.character(start_cohort),
    no = as.character(cohortdata$immunization_date)
  )
)

# Snapshot tests to check cleaned up implementation
test_that("Snapshot test for `get_exposition_time`", {

  # calculate time to death
  exposition_time <- get_exposition_time(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    end_cohort = end_cohort
  )

  expect_snapshot(
    head(exposition_time, 20)
  )
})

# Correctness test
test_that("`get_exposition_time`: correctness", {

  # calculate time to death
  cohortdata$exposition_time <- get_exposition_time(
    data_set = cohortdata,
    outcome_date_col = "death_date",
    censoring_date_col = "death_other_causes",
    end_cohort = end_cohort
  )

  # Check for NAs
  expect_false(anyNA(cohortdata$exposition_time))

  #Check for informed outcome or censoring dates
  informed_data <- cohortdata[!is.na(cohortdata$death_date) |
      !is.na(cohortdata$death_other_causes),
  ]
  informed_data$tf <- pmin(informed_data$death_date,
    informed_data$death_other_causes, na.rm = TRUE
  )
  informed_data$exposition_time_test <-
    informed_data$tf - informed_data$t0_follow_up

  expect_true(
    all(informed_data$exposition_time_test == informed_data$exposition_time)
  )
  #Check for non-informed outcome or censoring dates
  non_informed_data <- cohortdata[is.na(cohortdata$death_date) &
      is.na(cohortdata$death_other_causes),
  ]

  non_informed_data$exposition_time_test <-
    end_cohort - non_informed_data$t0_follow_up

  expect_true(
    all(non_informed_data$exposition_time_test ==
          non_informed_data$exposition_time)
  )
})

#### Tests for the get_time_to_event_at() ####
#reload data for clean test
data("cohortdata")

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
  end_cohort = as.Date("2044-12-31"),
  match = TRUE,
  exact = c("age", "sex"),
  nearest = NULL
)

data_test <- vaccineff_data$matching$match
tags <- linelist::tags(data_test)

#### Basic expectations of `get_time_to_event_at()`
test_that("`get_time_to_event_at`: basic expectations", {
  data_trunc <- get_time_to_event_at(
    data_set = data_test,
    outcome_date_col = tags$outcome_date_col,
    censoring_date_col = tags$censoring_date_col,
    end_cohort = vaccineff_data$end_cohort,
    at = 90
  )
  expect_identical(max(data_trunc$time_to_event), 90)
})
