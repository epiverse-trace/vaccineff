#### Test for set event status ####

# load package example data for cohort studies
data("cohortdata")

# assign vaccination status

test_that("`set_event_status`: Basic expectations", {
  # runs without errors
  expect_no_condition(
    set_status(
      data = cohortdata,
      col_names = "death_date",
      status = c(1, 0)
    )
  )

  # expect character
  expect_vector(
    vaccine_status,
    ptype = character()
  )

  # expect unique values
  expect_setequal(vaccine_status, vax_status)
})