#' @title Adjust Exposition for Static Matching
#'
#' @description This function removes the couples whose exposition times
#' do not match. This happens when the outcome of the unvaccinated individual
#' occurs before the vaccination date of their partner.
#'
#' @inheritParams match_cohort
#' @param matched_cohort Dataset with matched cohort from `match_cohort_`.
#' @return Data frame with matched population and corrected exposure times.
#' @keywords internal

adjust_exposition <- function(matched_cohort,
                              outcome_date_col,
                              censoring_date_col,
                              immunization_date,
                              start_cohort,
                              end_cohort) {
  # 1. Check that individual censoring occurs before event
  matched_cohort$censoring_individual <- as.Date(ifelse(
    (matched_cohort[[censoring_date_col]] <
       matched_cohort[[outcome_date_col]]) |
      is.na(matched_cohort[[outcome_date_col]]),
    as.character(matched_cohort[[censoring_date_col]]),
    as.Date(NA)
  ))

  # 2. Match minimum censoring date as censoring date for couple
  matched_cohort$censoring_couple <-  as.Date(match_couple_info(
    data = matched_cohort,
    column_to_match = "censoring_individual",
    criteria = "min"
  ))

  # 3. If an outcome happens before censoring_couple
  # no censoring must be assigned
  matched_cohort$censoring_accepted <-
    as.Date(ifelse(
      (matched_cohort$censoring_couple > matched_cohort[[outcome_date_col]]) &
        (!is.na(matched_cohort$censoring_couple)) &
        (!is.na(matched_cohort[[outcome_date_col]])),
      as.Date(NA),
      as.character(matched_cohort$censoring_couple)
    ))

  # 4. Beginning of the follow-up period for couples is the immunization date
  matched_cohort$t0_follow_up <-  as.Date(match_couple_info(
    data = matched_cohort,
    column_to_match = immunization_date,
    criteria = "min"
  ))

  # 5. Calculate time-to-event
  matched_cohort$time_to_event <- get_time_to_event(
    data = matched_cohort,
    outcome_date_col = outcome_date_col,
    censoring_date_col = "censoring_accepted",
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    start_from_immunization = TRUE,
    immunization_date_col = "t0_follow_up"
  )

  # 6. Individuals with negative exposure and their couples must be removed
  matched_cohort$min_exposure_time_couple <- match_couple_info(
    data = matched_cohort,
    column_to_match = "time_to_event",
    criteria = "min"
  )
  neg_times <- matched_cohort[matched_cohort$min_exposure_time_couple < 0, ]

  head(neg_times[order(neg_times$subclass), ], n = 4)

  adjusted_match <- matched_cohort[
    matched_cohort$min_exposure_time_couple > 0,
  ]

  # 7. Remove unnecessary columns and reorder
  col_names <- names(adjusted_match)
  col_names <- col_names[! col_names %in%
      c("censoring_individual",
        "censoring_couple", "censoring_accepted",
        "min_exposure_time_couple")
  ]

  adjusted_match <- subset(adjusted_match, select = col_names)

  return(adjusted_match)
}

#' @title Static Matching
#'
#' @description This function calls `match_cohort_` once and then
#' removes the couples whose exposition times do not coincide.
#' It returns the adjusted cohort, a summary of the matching result,
#' and the balance of the cohort before and after matching.
#'
#' @inheritParams match_cohort
#' @return List with results from static match:
#' `match`: adjusted cohort, `summary`: matching summary,
#' `balance_all`: balance of the cohort before matching,
#' `balance_matched`: balance of the cohort after matching.
#' @keywords internal
static_match <- function(data,
                         outcome_date_col,
                         censoring_date_col,
                         immunization_date_col,
                         vacc_status_col,
                         start_cohort,
                         end_cohort,
                         nearest,
                         exact) {
  # match cohort
  matched <- match_cohort_(
    data = data,
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
  summary <- match_summary(all = data,
    matched = matched,
    adjusted = adjusted,
    vacc_status_col = vacc_status_col
  )

  # Balance summary all
  balace_all <- balance_summary(data = data,
    nearest = nearest,
    exact = exact,
    vacc_status_col = vacc_status_col
  )
  # Balance summary match
  balace_match <- balance_summary(data = adjusted,
    nearest = nearest,
    exact = exact,
    vacc_status_col = vacc_status_col
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
