# Test for generic function dataset
dataset.data.frame <- function(object, ...) {
  return("data.frame method called")
}
# Basic expectations for dataset
test_that("dataset dispatches the correct method", {
  # Create objects of different classes
  df <- data.frame(x = 1:10)

  # Test that the correct method is called for a data.frame
  expect_equal(dataset(df), "data.frame method called")
})
