match_cohort <- function(data,
                         outcome_date_col,
                         censoring_date_col,
                         immunization_date_col,
                         vacc_status_col,
                         start_cohort,
                         end_cohort,
                         method = "static",
                         nearest,
                         exact) {
  if (method == "static") {
    match_obj <- static_match(data,
                              outcome_date_col,
                              censoring_date_col,
                              immunization_date_col,
                              vacc_status_col,
                              start_cohort,
                              end_cohort,
                              nearest,
                              exact)
  }
  class(match_obj) <- "match"
  return(match_obj)
}


summary.match <- function(match) {
  # Check if the input object is of class "match"
  if (!inherits(match, "match")) {
    stop("Input must be an object of class 'match'")
  }
  cat("Balance all:\n")
  print(match$balance_all)
  cat("\nBalance matched:\n")
  print(match$balance_match)
  cat("\nSummary:\n")
  print(match$summary)
}

data.match <- function(match) {
  # Check if the input object is of class "match"
  if (!inherits(match, "match")) {
    stop("Input must be an object of class 'match'")
  }
  return(match$match)
}
