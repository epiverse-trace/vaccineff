#' @title Adjust Exposition for Static Matching
#'
#' @description This function removes the pairs whose exposition times
#' do not match. This happens when the outcome of the unvaccinated individual
#' occurs before the vaccination date of their partner.
#'
#' @inheritParams match_cohort
#' @param matched_cohort `data.frame` with matched cohort from `match_cohort_`.
#' @return `data.frame` with matched population and corrected exposure times.
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

  # 2. Match minimum censoring date as censoring date for pair
  matched_cohort$censoring_pair <-  as.Date(match_pair_info(
    data_set = matched_cohort,
    column_to_match = "censoring_individual",
    criteria = "min"
  ))

  # 3. If an outcome happens before censoring_pair
  # no censoring must be assigned
  matched_cohort$censoring_accepted <-
    as.Date(ifelse(
      (matched_cohort$censoring_pair > matched_cohort[[outcome_date_col]]) &
        (!is.na(matched_cohort$censoring_pair)) &
        (!is.na(matched_cohort[[outcome_date_col]])),
      as.Date(NA),
      as.character(matched_cohort$censoring_pair)
    ))

  # 4. Beginning of the follow-up period for pairs is the immunization date
  matched_cohort$t0_follow_up <-  as.Date(match_pair_info(
    data_set = matched_cohort,
    column_to_match = immunization_date,
    criteria = "min"
  ))

  # 5. Calculate time-to-event
  matched_cohort$time_to_event <- get_time_to_event(
    data_set = matched_cohort,
    outcome_date_col = outcome_date_col,
    censoring_date_col = "censoring_accepted",
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    start_from_immunization = TRUE,
    immunization_date_col = "t0_follow_up"
  )

  # 6. Individuals with negative exposure and their pairs must be removed
  matched_cohort$min_exposure_time_pair <- match_pair_info(
    data_set = matched_cohort,
    column_to_match = "time_to_event",
    criteria = "min"
  )

  adjusted_match <- matched_cohort[
    matched_cohort$min_exposure_time_pair > 0,
  ]

  # 7. Generate outcome status
  adjusted_match$outcome_status <- set_event_status(
    data_set = adjusted_match,
    outcome_date_col = outcome_date_col,
    censoring_date_col = "censoring_accepted"
  )

  # 8. Remove unnecessary columns and reorder
  col_names <- names(adjusted_match)
  col_names <- col_names[! col_names %in%
      c("censoring_individual",
        "censoring_pair", "censoring_accepted",
        "min_exposure_time_pair")
  ]

  adjusted_match <- subset(adjusted_match, select = col_names)

  return(adjusted_match)
}
