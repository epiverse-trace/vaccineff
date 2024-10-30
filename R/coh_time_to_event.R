
#' @title Calculate Exposition Time
#'
#' @description This auxiliary function calculates the exposition time of
#' individuals starting from the t0_follow_up date defined based on whether
#' a matching strategy is invoked or not. The end of the exposition time
#' is assigned based on the follow hierarchy
#' outcome_status -> censoring_date -> end_cohort
#'
#' @inheritParams match_cohort
#' @return `column` with exposition time per individual.
#' @keywords internal

get_exposition_time <- function(data_set,
                                outcome_date_col,
                                censoring_date_col,
                                end_cohort) {
  ### Same structure as get_time_to_event
  tf <- rep(end_cohort, nrow(data_set))
  # replace informed outcome dates
  tf <- as.Date(ifelse(!is.na(data_set[[outcome_date_col]]),
    yes = as.character(data_set[[outcome_date_col]]),
    no = as.character(tf)
  ))

  # replace censoring dates if provided
  if (!is.null(censoring_date_col)) {
    tf <- as.Date(ifelse(!is.na(data_set[[censoring_date_col]]),
      yes = as.character(data_set[[censoring_date_col]]),
      no = as.character(tf)
    ))
  }

  #exposition time the difference between tf and t0_follow_up
  exposition <- as.numeric(tf - data_set$t0_follow_up)
  return(exposition)
}

#' @title Construct Time-to-Event at
#'
#' @description This function returns both the time-to-event until a
#' reference number of days, as provided in `at`, and the outcome status
#' at the same point. It uses the exposition time as an auxiliary variable
#' to calculate the time-to-event. The starting point for counting the
#' time-to-event is `t0_follow_up`, which is determined based on whether
#' a matching strategy is used or not. If the event occurs before the
#' reference date, the end date of the exposition period is used to
#' calculate the time-to-event. This accounts for whether censoring or an
#' event occurred. The outcome status is determined based on whether the
#' outcome date coincides with the end of the follow-up period.
#'
#' @inheritParams make_immunization
#' @param at Time to truncate the follow-up period
#' @return `data.frame` containing time_to_event and outcome_status
#' @keywords internal

get_time_to_event_at <- function(data_set,
                                 outcome_date_col,
                                 censoring_date_col,
                                 end_cohort,
                                 at) {
  #Calculate total exposition time
  data_set$exposition_time <- get_exposition_time(
    data_set = data_set,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    end_cohort = end_cohort
  )

  #Calculate final date of exposition time from t0
  data_set$tf_exposition <- data_set$t0_follow_up + data_set$exposition_time

  #Calculate final date of follow up period at from t0
  data_set$tf_follow_up_at <- data_set$t0_follow_up + at

  # If tf_follow up exceeds the end of the cohort use this date
  data_set$tf_follow_up_at <- as.Date(
    ifelse(data_set$tf_follow_up_at > end_cohort,
      yes = as.character(end_cohort),
      no = as.character(data_set$tf_follow_up_at)
    )
  )

  # Use the minimum between tf of exposition period and follow up period at
  data_set$tf_follow_up_at <- pmin(data_set$tf_follow_up_at,
    data_set$tf_exposition,
    na.rm = TRUE
  )

  # Time to event is calculated up to minimum
  data_set$time_to_event <-
    as.numeric(data_set$tf_follow_up_at - data_set$t0_follow_up)

  # If tf of follow up coincides with outcome date, event status is positive
  data_set$outcome_status <- as.numeric(
    data_set$tf_follow_up_at == data_set[[outcome_date_col]] &
      !is.na(data_set[[outcome_date_col]]),
    1,
    0
  )

  # Remove auxiliary columns
  data_set <- data_set[, -which(names(data_set)
      %in% c("tf_follow_up_at", "tf_exposition")
    )
  ]

  return(data_set)
}
