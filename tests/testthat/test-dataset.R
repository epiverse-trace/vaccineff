# Test for generic function dataset
get_dataset.data.frame <- function(object, ...) {
  return("data.frame method called")
}
# Basic expectations for dataset
test_that("get_dataset dispatches the correct method", {
  # Create objects of different classes
  df <- data.frame(x = 1:10)

  # Test that the correct method is called for a data.frame
  expect_identical(get_dataset(df), "data.frame method called")
})
