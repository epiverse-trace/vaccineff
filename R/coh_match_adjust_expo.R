#' @title Adjust Exposure for Static Matching
#'
#' @description This function removes the pairs whose exposure times
#' do not match. This happens when the outcome of the unvaccinated individual
#' occurs before the vaccination date of their partner.
#'
#' @inheritParams match_cohort
#' @param matched_cohort `data.frame` with matched cohort from `match_cohort_`.
#' @return `data.frame` with matched population and corrected exposure times.
#' @keywords internal

adjust_exposure <- function(matched_cohort,
                            outcome_date_col,
                            censoring_date_col,
                            immunization_date,
                            start_cohort,
                            end_cohort) {

  # 1. Beginning of the follow-up period for pairs is the immunization date
  matched_cohort$t0_follow_up <-  as.Date(match_pair_info(
    data_set = matched_cohort,
    column_to_match = immunization_date,
    criteria = "min"
  ))

  # 2. Pairs share censoring date when appropiated
  # (see criteria in get_censoring_pair)
  matched_cohort$censoring_after_match <- get_censoring_after_match(
    data_set = matched_cohort,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col
  )

  # 3. Check for exposure times to ensure no negative exposures
  matched_cohort$exposure_time <- get_exposure_time(
    data_set = matched_cohort,
    outcome_date_col = outcome_date_col,
    censoring_date_col = "censoring_after_match",
    end_cohort = end_cohort
  )

  # Individuals with negative exposure and their pairs must be removed
  matched_cohort$min_exposure_time_pair <- match_pair_info(
    data_set = matched_cohort,
    column_to_match = "exposure_time",
    criteria = "min"
  )

  adjusted_match <- matched_cohort[
    matched_cohort$min_exposure_time_pair > 0,
  ]

  # Remove unnecessary columns and reorder
  col_names <- names(adjusted_match)
  col_names <- col_names[! col_names %in%
      c("min_exposure_time_pair", "exposure_time")
  ]

  adjusted_match <- subset(adjusted_match, select = col_names)

  return(adjusted_match)
}
