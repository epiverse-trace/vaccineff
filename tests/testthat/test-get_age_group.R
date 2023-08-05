#### Tests for age group binning works

# load data
data("cohortdata")

# check for basic working
test_that("`get_age_groups`: basic expectations", {
  # prepare a vector of binned ages
  max_val <- 80
  step_size <- 7

  age_groups <- get_age_group(
    data = cohortdata,
    col_age = "age",
    max_val = 80,
    step = 7
  )

  # basic checks for return type
  expect_s3_class(
    age_groups, "factor"
  )
  # check for number of factor levels
  expect_length(
    unique(age_groups),
    # manual cutting of age groups - ROUGHLY CORRECT
    length(
      levels(
        cut(
          cohortdata$age,
          breaks = c(
            -Inf,
            seq(step_size, max_val, step_size),
            Inf
          )
        )
      )
    )
  )
  # check that last value is the same as max_val
  expect_identical(
    tail(levels(age_groups), 1),
    sprintf(">%i", max_val) # hacky test to avoid regex extraction
  )

  # check that breaks are correct
  # expect 0-50 and >80
  expect_identical(
    levels(
      get_age_group(
        data = cohortdata,
        col_age = "age",
        max_val = 80,
        step = 50
      )
    ),
    c("0-50", ">80")
  )
})

# tests to check for abuse cases
test_that("`get_age_groups`: Input checking", {
  # data frame is not passed
  expect_error(
    get_age_group(
      data = "data",
      col_age = "age",
      max_val = 80,
      step = 7
    ),
    regexp = "(Must be of type 'data.frame')"
  )

  # age column not present
  expect_error(
    get_age_group(
      data = cohortdata,
      col_age = "somecol",
      max_val = 80,
      step = 7
    ),
    regexp = "(Names must include the elements)*('somecol')"
  )

  # maximum age is less than the minimum age
  min_age <- 10
  expect_error(
    get_age_group(
      data = cohortdata,
      col_age = "age",
      min_val = min_age,
      max_val = 1,
      step = 1
    ),
    regexp = sprintf(
      "Assertion on 'max_val' failed: Element 1 is not >= %i",
      min_age
    )
  )

  # max age is missing
  expect_error(
    get_age_group(
      data = cohortdata,
      col_age = "age",
      step = 1
    ),
    regexp = "(max_val)*(is missing, with no default)"
  )

  # minimum age is badly formed
  expect_error(
    get_age_group(
      data = cohortdata,
      col_age = "age",
      min_val = NA_integer_,
      max_val = 1,
      step = 1
    ),
    regexp = "Assertion on 'min_val' failed: May not be NA"
  )

  # step size is larger than difference in age limits
  # maximum age is less than the minimum age
  max_age <- 80
  min_age <- 1
  expect_error(
    get_age_group(
      data = cohortdata,
      col_age = "age",
      min_val = min_age,
      max_val = max_age,
      step = 90
    ),
    regexp = sprintf(
      "Assertion on 'step' failed: Element 1 is not <= %i",
      max_age
    )
  )
})
