#### Test for set event status ####

# load package example data for cohort studies
data("cohortdata")

# assign vaccination status

test_that("`set_event_status`: Basic expectations", {
  # runs without errors
  expect_no_condition(
    set_event_status(
      data_set = cohortdata,
      outcome_date_col = "death_date",
      censoring_date_col = "death_other_causes"
    )
  )
})
