#' @title Static Matching
#'
#' @description This function calls `match_cohort_` once and then
#' removes the pairs whose exposition times do not coincide.
#' It returns the adjusted cohort, a summary of the matching result,
#' and the balance of the cohort before and after matching.
#'
#' @inheritParams match_cohort
#' @return List with results from static match:
#' `match`: adjusted cohort, `summary`: matching summary,
#' `balance_all`: balance of the cohort before matching,
#' `balance_matched`: balance of the cohort after matching.
#' @keywords internal
static_match <- function(data_set,
                         outcome_date_col,
                         censoring_date_col,
                         immunization_date_col,
                         vacc_status_col,
                         vaccinated_status,
                         unvaccinated_status,
                         start_cohort,
                         end_cohort,
                         nearest,
                         exact) {
  # create temporal id for match
  data_set$match_id <- seq_len(nrow(data_set))

  # match cohort
  matched <- match_cohort_(
    data_set = data_set,
    vacc_status_col = vacc_status_col,
    nearest = nearest,
    exact = exact
  )

  # adjust exposition times of cohort
  adjusted_0 <- adjust_exposition(matched_cohort = matched,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    immunization_date = immunization_date_col,
    start_cohort = start_cohort,
    end_cohort = end_cohort
  )
  removed_0 <- nrow(matched) - nrow(adjusted_0)
  warning_1 <- paste("Matches before iterating:",  nrow(adjusted_0), "\n")
  warning_2 <- paste("Removed before iterating", removed_0, "\n")

  # iterate match after first exposition times adjusting
  adjusted_f <- iterate_match(
    all = data_set,
    matched = matched,
    adjusted = adjusted_0,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    immunization_date_col = immunization_date_col,
    vacc_status_col = vacc_status_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status,
    nearest = nearest,
    exact = exact,
    start_cohort = start_cohort,
    end_cohort = end_cohort
  )
  removed_f <- nrow(matched) - nrow(adjusted_f)
  warning_3 <- paste("Matches after iterating:",  nrow(adjusted_f), "\n")
  warning_4 <- paste("Removed after iterating", removed_f, "\n")


  # Matching summary
  summary <- match_summary(all = data_set,
    matched = adjusted_f,
    vacc_status_col = vacc_status_col
  )

  # Balance summary all
  balace_all <- balance_summary(
    data_set = as.data.frame(data_set),
    nearest = nearest,
    exact = exact,
    vacc_status_col = vacc_status_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status
  )

  # Balance summary match
  balace_match <- balance_summary(
    data_set = as.data.frame(adjusted_f),
    nearest = nearest,
    exact = exact,
    vacc_status_col = vacc_status_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status
  )

  # Remove match_id col
  adjusted_f <- adjusted_f[, -which(names(adjusted_f) == "match_id")]

  warning(
    warning_1, warning_2, warning_3, warning_4
  )

  # Match object
  match <- list(
    match = adjusted_f,
    summary = summary,
    balance_all = balace_all,
    balance_match = balace_match
  )

  return(match)
}
