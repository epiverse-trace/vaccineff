#### Tests for coh_data_wrangling.R module ####
#### Prepare data for all the tests ####
data("cohortdata")

# load package example data for cohort studies
# unique statuses
vax_status <- c("vaccinated", "unvaccinated")
# assign vaccination status
vaccine_status <- set_status(
  data_set = cohortdata,
  col_names = c("vaccine_date_1", "vaccine_date_2"),
  status = vax_status
)

#### Tests for set_status() ####
#basic expectations
test_that("`set_status`: Basic expectations", {
  # runs without errors
  expect_no_condition(
    set_status(
      data_set = cohortdata,
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

#correctness
test_that("`set_status`: Correctness", {
  status <- c(1, 0)
  # get status with AND operator
  status_all <- set_status(
    data_set = cohortdata,
    col_names = c("death_date", "vaccine_date_1"),
    operator = "&",
    status = status
  )

  # get status with OR operator
  status_any <- set_status(
    data_set = cohortdata,
    col_names = c("death_date", "vaccine_date_1"),
    operator = "|",
    status = status
  )

  # expect not identical
  expect_false(
    all(status_any == status_all)
  )

  # death status both causes
  death_status_both <- set_status(
    data_set = cohortdata,
    col_names = c("death_date", "death_other_causes"),
    operator = "&",
    status = status
  )
  # death_date and death_other_causes are disjoint
  expect_false(
    any(death_status_both == "1")
  )

  # manual checking for AND operator is correct
  status_all_manual <- apply(
    cohortdata[, c("death_date", "vaccine_date_1")],
    MARGIN = 1,
    FUN = function(x) {
      all(!is.na(x))
    }
  )
  expect_identical(
    status_all == "1",
    status_all_manual
  )

  # manual checking for OR operator is correct
  status_any_manual <- apply(
    cohortdata[, c("death_date", "vaccine_date_1")],
    MARGIN = 1,
    FUN = function(x) {
      any(!is.na(x))
    }
  )
  expect_identical(
    status_any == "1",
    status_any_manual
  )
})

# test for inputs
test_that("`set_status`: Input checking", {
  # errors triggered correctly
  expect_error(
    set_status(
      data_set = cohortdata,
      col_names = c("vaccine_date_1", "vaccine_date_2"),
      operator = "&&",
      status = vax_status
    ),
    regexp = "('arg' should be one of)*(\\&)*(\\|)"
  )

  # failure on excessive status length
  expect_error(
    set_status(
      data_set = cohortdata,
      col_names = c("vaccine_date_1", "vaccine_date_2"),
      status = c(vax_status, vax_status)
    ),
    regexp = "Must have length 2"
  )
})
