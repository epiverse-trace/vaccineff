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
  # match cohort
  matched <- match_cohort_(
    data_set = data_set,
    vacc_status_col = vacc_status_col,
    nearest = nearest,
    exact = exact
  )

  # adjust exposition times of cohort
  adjusted <- adjust_exposition(matched_cohort = matched,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    immunization_date = immunization_date_col,
    start_cohort = start_cohort,
    end_cohort = end_cohort
  )

  # Matching summary
  summary <- match_summary(all = data_set,
    matched = matched,
    adjusted = adjusted,
    vacc_status_col = vacc_status_col
  )

  # Balance summary all
  balace_all <- balance_summary(data_set = data_set,
    nearest = nearest,
    exact = exact,
    vacc_status_col = vacc_status_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status
  )
  # Balance summary match
  balace_match <- balance_summary(data_set = adjusted,
    nearest = nearest,
    exact = exact,
    vacc_status_col = vacc_status_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status
  )

  # Match object
  match <- list(
    match = adjusted,
    summary = summary,
    balance_all = balace_all,
    balance_match = balace_match
  )

  return(match)
}
