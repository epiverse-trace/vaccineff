#### Tests for check_dataset ####
## prepare data
data("cohortdata")

# Test for basic expectations
test_that("`check_dataset`: basic expectations", {

  # passing other type of element to data_set
  vector <- c("v1", "v2", "v3")
  expect_error(
    check_dataset(data_set = vector, columns = vector),
    regexp =
      paste0("Assertion on 'data_set' failed: Must be of type 'data.frame',",
             " not 'character'.")
  )
  # No error expected when passing NULL argument in name of cols
  expect_silent(
    check_dataset(data_set = cohortdata, columns = c("death_date", NULL, "age"))
  )
  # Expect error if columns name not included in data_set
  expect_error(
    check_dataset(data_set = cohortdata, columns = "wrong_name")
  )
})
