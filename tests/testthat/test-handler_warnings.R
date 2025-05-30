#### Tests for handler_warnings.R module ####

# Example function that generates warnings
example_function <- function() {
  warning("This is warning 1", call. = FALSE)
  warning("This is warning 2", call. = FALSE)
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
