#' @title Create Status Column
#'
#' @description This function generates a binary status column using the set of
#' columns passed through the variable `col_names`. This column must contain the
#' information of the outcomes or the vaccine dates. It generates a logical
#' condition using the operators `&` or `|`, and evaluates if the registers in
#' the columns contain or not information. If the logical operator is `&`, the
#' function returns a success only if all the columns contain information. On
#' the other hand, if the logical operator is `|`, it is enough to find one
#' column with information indicating success. It is recommended to use this
#' method when working with several outcomes or several vaccine doses. By
#' default, it returns a binary column where `0` means no outcome or no vaccine
#' and `1` means the opposite. However, it can also receive custom
#' options, e.g., `c("v", "u")` for vaccinated and unvaccinated.
#'
#' @param data_set `data.frame` with at least one column from which to
#' generate the status specified in `status`.
#' @param col_names Name(s) of the column(s) as a string or a character
#' vector containing the information from which the status is calculated.
#' @param operator A single logical operator to evaluate the condition.
#' @param status A two-element vector specifying the values to be assigned that
#' indicate whether the individual is vaccinated or not, e.g., `c("v","u")`. The
#' first element of the vector must be the status when the condition is
#' satisfied, i.e., vaccinated, while the second element is the value indicating
#' that the individual is not vaccinated.
#' @return Status
#' @keywords internal

set_status <- function(data_set,
                       col_names,
                       operator = c("&", "|"),
                       status = c(1, 0)) {
  # input checking
  checkmate::assert_data_frame(
    data_set,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_character(col_names, any.missing = FALSE, min.len = 1)
  checkmate::assert_names(
    names(data_set),
    must.include = col_names
  )

  # check the operator
  operator <- match.arg(operator, several.ok = FALSE)
  checkmate::assert_string(
    operator,
    n.chars = 1L
  )

  # check the status vector
  checkmate::assert_vector(
    status,
    len = 2L, unique = TRUE,
    any.missing = FALSE
  )
  condition <- "ifelse("
  int0 <- "(!is.na(data_set[["
  intf <- "]]))"
  i <- 1
  for (col_name in col_names) {
    if (i == length(col_names)) {
      sep <- ""
    } else {
      sep <- operator
    }
    condition <- paste0(condition, int0, "'", col_name, "'", intf, sep)
    i <- i + 1
  }
  if (is.numeric(status)) {
    lst <- paste0(",", status[1], ",", status[2], ")")
  } else {
    lst <- paste0(",", "'", status[1], "'", ",", "'", status[2], "'", ")")
  }
  condition <- paste0(condition, lst)
  status_col <- eval(parse(text = condition))
  return(status_col)
}

#' @title Create Event Status
#'
#' @description This function generates a binary status (1,0) associated
#' with an outcome. The value returned is 0 if a register is censored before
#' the outcome occurs or if the outcome does not occur during the follow-up
#' period. If the outcome occurs and the register is not censored, the value
#' returned is 1.
#' @inheritParams get_immunization_date
#' @return Status
#' @keywords internal

set_event_status <- function(data_set,
                             outcome_date_col,
                             censoring_date_col = NULL) {
  checkmate::assert_data_frame(
    data_set,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_character(outcome_date_col,
    any.missing = FALSE, min.len = 1
  )
  checkmate::assert_names(
    names(data_set), must.include = outcome_date_col
  )

  data_set$outcome_status <- set_status(data_set = data_set,
    col_names = outcome_date_col,
    status = c(1, 0)
  )

  checkmate::assert_character(censoring_date_col,
    any.missing = FALSE, min.len = 1, null.ok = TRUE
  )
  checkmate::assert_names(
    names(data_set), must.include = censoring_date_col
  )

  if (!is.null(censoring_date_col)) {
    data_set$outcome_status <- ifelse(
      (!is.na(data_set[[censoring_date_col]])) &
        (!is.na(data_set[[outcome_date_col]])) &
        (data_set[[censoring_date_col]] <= data_set[[outcome_date_col]]),
      yes = "0",
      no = data_set$outcome_status
    )
  }
  return(as.numeric(data_set$outcome_status))
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

#' @title Internal function to truncate data based on start_cohort
#'
#' @inheritParams make_vaccineff_data
#' @return `data.frame` with truncated data
#' @keywords internal
truncate_from_start_cohort <- function(data_set,
                                       outcome_date_col,
                                       censoring_date_col,
                                       start_cohort) {
  n0 <- nrow(data_set)

  # Check for immunization date
  data_set <- data_set[
    data_set$immunization_date >= start_cohort |
      is.na(data_set$immunization_date),
  ]

  # Check for outcome date
  data_set <- data_set[
    is.na(data_set[[outcome_date_col]]) |
      data_set[[outcome_date_col]] >= start_cohort,
  ]

  data_set <- data_set[
    is.na(data_set[[outcome_date_col]]) |
      is.na(data_set$immunization_date) |
      data_set[[outcome_date_col]] >= data_set$immunization_date,
  ]

  # Check for censoring date
  data_set <- data_set[
    is.na(data_set[[censoring_date_col]]) |
      data_set[[censoring_date_col]] >= start_cohort,
  ]

  data_set <- data_set[
    is.na(data_set[[censoring_date_col]]) |
      is.na(data_set$immunization_date) |
      data_set[[censoring_date_col]] >= data_set$immunization_date,
  ]

  nf <- nrow(data_set)

  msg <- paste0("\nThe start date of the cohort was defined as",
    " the mininimum immunization date. \n",
    n0 - nf, " registers were removed with outcomes before the start date.\n"
  )
  warning(msg)

  return(data_set)
}
