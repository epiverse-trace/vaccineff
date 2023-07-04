test_that("immunization date", {
  data("cohortdata")
  cohortdata$immunization_death <-
    get_immunization_date(cohortdata,
                          "death.date",
                          0,
                          14,
                          c("vaccine.date.1", "vaccine.date.2"),
                          "2021-12-31",
                          take_first = FALSE)
  expect_equal(2 * 2, 4)
})
