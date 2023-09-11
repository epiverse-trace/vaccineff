#### Test for set status ####

# load package example data for cohort studies
data("cohortdata")

# unique statuses
vax_status <- c("vaccinated", "unvaccinated")

# assign vaccination status
vaccine_status <- set_status(
  data = cohortdata,
  col_names = c("vaccine_date_1", "vaccine_date_2"),
  status = vax_status
)

test_that("`set_status`: Basic expectations", {
  # runs without errors
  expect_no_condition(
    set_status(
      data = cohortdata,
      col_names = c("vaccine_date_1", "vaccine_date_2"),
      status = vax_status
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

test_that("`set_status`: Correctness", {
  # get status with AND operator
  vaccine_status_all <- set_status(
    data = cohortdata,
    col_names = c("vaccine_date_1", "vaccine_date_2"),
    operator = "&",
    status = vax_status
  )

  # get status with OR operator
  vaccine_status_any <- set_status(
    data = cohortdata,
    col_names = c("vaccine_date_1", "vaccine_date_2"),
    operator = "|",
    status = vax_status
  )

  # expect not identical
  expect_false(
    all(vaccine_status_any == vaccine_status_all)
  )

  # manual checking for AND operator is correct
  vaccine_status_all_manual <- apply(
    cohortdata[, c("vaccine_date_1", "vaccine_date_2")],
    MARGIN = 1,
    FUN = function(x) {
      all(!is.na(x)) # nolint
    }
  )
  expect_identical(
    vaccine_status_all == "vaccinated",
    vaccine_status_all_manual
  )

  # manual checking for OR operator is correct
  vaccine_status_any_manual <- apply(
    cohortdata[, c("vaccine_date_1", "vaccine_date_2")],
    MARGIN = 1,
    FUN = function(x) {
      any(!is.na(x)) # nolint
    }
  )
  expect_identical(
    vaccine_status_any == "vaccinated",
    vaccine_status_any_manual
  )
})

test_that("`set_status`: Input checking", {
  # errors triggered correctly
  expect_error(
    set_status(
      data = "data",
      col_names = c("vaccine_date_1", "vaccine_date_2"),
      status = vax_status
    ),
    regexp = "Must be of type 'data.frame'"
  )

  expect_error(
    set_status(
      data = cohortdata,
      col_names = c("vaccine_date_3", "vaccine_date_4"),
      status = vax_status
    ),
    regexp = "(Names must include)*('vaccine_date_3')*('vaccine_date_4')"
  )

  expect_error(
    set_status(
      data = cohortdata,
      col_names = c("vaccine_date_1", "vaccine_date_2"),
      operator = "&&",
      status = vax_status
    ),
    regexp = "('arg' should be one of)*(\\&)*(\\|)"
  )

  # failure on excessive status length
  expect_error(
    set_status(
      data = cohortdata,
      col_names = c("vaccine_date_1", "vaccine_date_2"),
      status = c(vax_status, vax_status)
    ),
    regexp = "Must have length 2"
  )
})
