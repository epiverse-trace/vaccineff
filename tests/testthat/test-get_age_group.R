#### Tests for age group binning works

# load data
data("cohortdata")

# check for basic working
test_that("`get_age_groups`: basic expectations", {
  # prepare a vector of binned ages
  max_val <- 80
  step_size <- 10

  age_groups <- get_age_group(
    data = cohortdata,
    col_age = "age",
    max_val = max_val,
    step = step_size
  )

  # basic checks for return type
  expect_s3_class(
    age_groups, "factor"
  )
  # check for number of factor levels
  expect_length(
    unique(age_groups),
    # manual cutting of age groups - ROUGHLY CORRECT
    nlevels(
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

  # check that breaks are correct
  # expect 0-50 and >80
  expect_warning(
    get_age_group(
      data = cohortdata,
      col_age = "age",
      max_val = 80,
      step = 50
    )
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

  # non-integer values passed
  expect_error(
    get_age_group(
      data = cohortdata,
      col_age = "age",
      min_val = 0.7,
      max_val = 1,
      step = 1
    )
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

# tests to check for min_val != 0
test_that("`get_age_groups`: non-zero min_val", {
  min_val <- 10
  cohortdata$age_group <- get_age_group(
    data = cohortdata,
    col_age = "age",
    max_val = 80,
    min_val = min_val,
    step = 10
  )

  #expect none NA values are expected
  expect_length(cohortdata[is.na(cohortdata$age_group), ]$age_group, 0)

  #Check for registers < min_val
  expect_true(
    all(cohortdata[cohortdata$age < min_val, ]$age_group == "<9")
  )
})
