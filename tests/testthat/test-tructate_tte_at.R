#### Tests for the truncate_tte_at ####
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
  start_cohort = as.Date("2044-01-01"),
  end_cohort = as.Date("2044-12-31"),
  match = TRUE,
  exact = c("age", "sex"),
  nearest = NULL
)

data_test <- vaccineff_data$matching$match
tags <- linelist::tags(data_test)

#### Basic expectations of `effectiveness()`
test_that("`truncate_tte_at`: basic expectations", {
  data_trunc <- truncate_tte_at(
    data_set = data_test,
    outcome_date_col = tags$outcome_date_col,
    end_cohort = vaccineff_data$end_cohort,
    at = 90
  )
  expect_identical(max(data_trunc$time_to_event), 90)
  expect_gte(
    nrow(data_test[data_test$outcome_status == 1, ]),
    nrow(data_trunc[data_trunc$outcome_status == 1, ])
  )
})
