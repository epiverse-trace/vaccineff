#### Tests for the immunization dose ####

# prepare data
data("cohortdata")

# calculate immunization date
cohortdata$immunization_death <- get_immunization_date(
  data = cohortdata,
  outcome_date_col = "death_date",
  outcome_delay = 0,
  immunization_delay = 14,
  vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
  end_cohort = as.Date("2044-12-31"),
  take_first = FALSE
)

# get the immunization dose
vacc_date_col <- c("vaccine_date_1", "vaccine_date_2")
immunization_dose <- get_immunization_dose(
  data = cohortdata,
  immunization_date_col = "immunization_death",
  vacc_date_col = vacc_date_col,
  immunization_delay = 14
)

test_that("`immunization_dose`: Basic expectations", {
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
