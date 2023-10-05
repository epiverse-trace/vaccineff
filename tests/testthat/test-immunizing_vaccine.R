#### Test for immunizing vaccine ####

# prepare data
data("cohortdata")

cohortdata$immunization <- get_immunization_date(
  data = cohortdata,
  outcome_date_col = "death_date",
  outcome_delay = 0,
  immunization_delay = 14,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = as.Date("2044-12-31"),
  take_first = FALSE
)

which_vaccine <- get_immunization_vaccine(
  data = cohortdata,
  immunization_date_col = "immunization",
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  vacc_name_col = c("vaccine_1", "vaccine_2"),
  immunization_delay = 14
)

test_that("`immunization_vaccine`: Basic expectations", {
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
