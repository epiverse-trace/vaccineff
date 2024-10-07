#### Tests for iterate_match()
## create test function

# Example function that generates warnings
example_function <- function() {
  warning("This is warning 1")
  warning("This is warning 2")
  return("Function completed")
}

# Test for basic expectations and correctness
test_that("`capture_warnings`: Correctness", {
  output <- capture_warnings(example_function())
  expect_identical(
    output$result, "Function completed"
  )

  expect_snapshot(
    output$warnings
  )
})
