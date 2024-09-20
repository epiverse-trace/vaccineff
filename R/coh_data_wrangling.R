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
#' default, it returns a binary column where 0 means no outcome or no vaccine
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

#' @title Construct Time-to-Event
#'
#' @description This function returns a column with the time-to-event in days
#' until a reference outcome. The starting point to count the time-to-event
#' can be the immunization date, assuming that the vaccinated population enters
#' the study when they are vaccinated (start_from_immunization=TRUE). Or the
#' beginning of the study, if all the cohort is known at this point
#' (start_from_immunization=FALSE). In this last case, it is not necessary to
#' pass the argument `immunization_date_col`.
#'
#' @inheritParams get_immunization_date
#' @param start_cohort Start date of the study.
#' @param start_from_immunization `TRUE`: starts counting time-to-event from the
#' immunization date if available. `FALSE`: starts counting time-to-event from
#' the start date of the cohort study.
#' @param immunization_date_col Name of the column that contains the
#' immunization date. Required if start_from_immunization = TRUE.
#' Default is NULL.
#' @return Time-to-event
#' @keywords internal

get_time_to_event <- function(data_set,
                              outcome_date_col,
                              censoring_date_col = NULL,
                              start_cohort, end_cohort,
                              start_from_immunization = FALSE,
                              immunization_date_col = NULL) {
  # add input checking
  checkmate::assert_data_frame(
    data_set,
    min.rows = 1L
  )

  checkmate::assert_string(outcome_date_col)

  checkmate::assert_names(
    colnames(data_set),
    must.include = outcome_date_col
  )
  # check date types
  checkmate::assert_date(
    start_cohort, any.missing = FALSE, len = 1
  )
  checkmate::assert_date(
    end_cohort, any.missing = FALSE, len = 1
  )

  # check if date columns are date type
  checkmate::assert_date(
    data_set[[outcome_date_col]]
  )

  checkmate::assert_logical(
    start_from_immunization,
    len = 1L, any.missing = FALSE
  )

  #Checks of censoring_date_col if provided
  checkmate::assert_string(censoring_date_col, null.ok = TRUE)
  checkmate::assert_names(
    colnames(data_set),
    must.include = censoring_date_col
  )
  if (!is.null(censoring_date_col)) {
    checkmate::assert_date(
      data_set[[censoring_date_col]]
    )
  }

  # check immnunization date col if asked
  if (start_from_immunization) {
    stopifnot(
      "`immunization_date_col` must be provided, and a column name in `data_set`" = #nolint
        (!missing(immunization_date_col) &&
         checkmate::test_string(immunization_date_col) &&
         immunization_date_col %in% colnames(data_set))
    )

    # Check for date type column
    checkmate::assert_date(
      data_set[[immunization_date_col]]
    )
  }

  # Initialize vector with start point to calculate time-to-event
  # cohort start by default
  t0 <- rep(start_cohort, nrow(data_set))
  if (start_from_immunization) {
    # if start from immunization replace informed immunization dates
    t0 <- as.Date(ifelse(is.na(data_set[[immunization_date_col]]),
      yes = as.character(t0),
      no = as.character(data_set[[immunization_date_col]])
    ))
  }

  # Initialize vector with end point to calculate time-to-event
  # cohort end by default
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

  #time to event is simply the difference between tf and t0
  time_to_event <- as.numeric(tf - t0)
  return(time_to_event)
}

#' @title Internal function to truncate time to events
#'
#' @inheritParams make_vaccineff_data
#' @param at Time to truncate the follow-up period
#' @return `data.frame` with truncated data
#' @keywords internal
truncate_time_to_event <- function(data_set,
                            outcome_date_col,
                            end_cohort,
                            at) {
  data_set$tf_follow_up <- data_set$t0_follow_up + data_set$time_to_event

  data_set$t_follow_up_at <- data_set$t0_follow_up + at
  data_set$t_follow_up_at <- as.Date(
    ifelse(data_set$t_follow_up_at > end_cohort,
      yes = as.character(end_cohort),
      no = as.character(data_set$t_follow_up_at)
    )
  )

  data_set$t_follow_up_at <- pmin(data_set$t_follow_up_at,
    data_set$tf_follow_up,
    na.rm = TRUE
  )

  data_set$time_to_event <-
    as.numeric(data_set$t_follow_up_at - data_set$t0_follow_up)

  data_set$outcome_status <- as.numeric(
    data_set$t_follow_up_at == data_set[[outcome_date_col]] &
      !is.na(data_set[[outcome_date_col]]),
    1,
    0
  )

  data_set <- data_set[, -which(names(data_set)
      %in% c("t_follow_up_at", "tf_follow_up")
    )
  ]

  return(data_set)
}

#' @title Internal function to truncate data based on start_cohort
#'
#' @inheritParams make_vaccineff_data
#' @return `data.frame` with truncated data
#' @keywords internal
adjust_time_to_event <- function(data_set) {
  n0 <- nrow(data_set)
  data_set <- data_set[data_set$time_to_event > 0, ]
  nf <- nrow(data_set)

  msg <- paste0("\nThe start date of the cohort was defined as",
    " the mininimum immunization date. \n",
    n0 - nf, " registers were removed with outocomes before the start date.\n"
  )
  warning(msg)

  return(data_set)
}
