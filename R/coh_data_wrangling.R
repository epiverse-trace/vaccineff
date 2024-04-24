#' @title Assign outcome status or vaccination status
#'
#' @description This function generates a binary status column using the set of
#' columns, passed through the
#' variable `col_names`. This column must contain the information of
#' the outcomes or the vaccine dates.
#' It generates a logical condition using the operators `&` or `|`, and
#' evaluates if the registers in
#' the columns contain or not information.
#' If the logical operator is `&`, the function returns a success, only if all
#' the columns contain information.
#' On the other hand, if the logical operator is `|`, it is enough to find one
#' column with information indicating success.
#' It is recommended to use this method when working with several outcomes or
#' several vaccine doses.
#' By default, it returns a binary column where 0 means no outcome or no
#' vaccine and `1` means the opposite.
#' However, it can also receive personalized options, e.g. `c("v", "u")` for
#' vaccinated and unvaccinated.
#'
#' @param data Dataset, as a `data.frame` with at least one column from which
#' to generate the status specified in `status`.
#' @param col_names Name or names of the column(s) as a string or a character
#' vector containing the information from which the status is calculated.
#' @param operator A single logical operator to evaluate the condition.
#' @param status A two-element vector specifying the values to be assigned that
#' indicate whether the individual is vaccinated or not e.g. c("v","u").
#' The first element of the vector must be the status when the condition is
#' satisfied, i.e., vaccinated, while the second element is the value indicating
#' that the individual is not vaccinated.
#' @return status
#' @examples
#' # load package example data for cohort studies
#' data("cohortdata")
#'
#' # assign vaccination status
#' cohortdata$vaccine_status <- set_status(
#'   data = cohortdata,
#'   col_names = c("vaccine_date_1", "vaccine_date_2"),
#'   status = c("v", "u")
#' )
#'
#' # view data with added column
#' head(cohortdata)
#' @export
set_status <- function(data,
                       col_names,
                       operator = c("&", "|"),
                       status = c(1, 0)) {
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_character(col_names, any.missing = FALSE, min.len = 1)
  checkmate::assert_names(
    names(data),
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
  int0 <- "(!is.na(data[["
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

#' Function to construct the inmunization date
#'
#' This function returns a column with the immunization date that corresponds
#' to an analyzed outcome.
#' The function searches for vaccine dates that satisfy the condition:
#' vacc_date_col <= outcome_date_col - delay_time - immunization_delay.
#' This condition allows to discriminate the vaccine dates in
#' terms of characteristic times in days (delay_time)  associated to an
#' outcome, from the onset of symptoms or from any reference event, and the
#' characteristic time in days before the patient is considered
#' immune (immunization_delay).
#' Both parameters can be set to zero by the user without
#' affecting the results.
#' When take_first = FALSE, the function uses the vaccine date that is closest
#' to the outcome to calculate the immunization. On the other hand,
#' when take_first = TRUE the functions uses the first vaccine date.
#'
#' If a register does not present an outcome, the function searches
#' for vaccine dates that satisfy the condition:
#' vacc_date_col <= end_cohort - delay_time - immunization_delay.
#' The immunization date is constructed using the closest vaccine
#' date to the end of the study, when take_first = FALSE; or the
#' first vaccination date found, when take_first = TRUE.
#' Notice that the function works for one or several vaccines.
#' In case of several vaccines, the parameter
#' {outcome_date_col must} be passed as a vector (see example).
#'
#' @param data dataset with cohort information (see example)
#' @param outcome_date_col name of the column that contains
#' the outcome dates
#' @param censoring_date_col name of the column that contains
#' the censoring date. NULL by default
#' @param outcome_delay characteristic time in days of the outcome
#' from the reference event
#' @param immunization_delay characteristic time in days before the patient
#' is considered immune
#' @param vacc_date_col name of the column(s) that contains the vaccine dates
#' @param end_cohort end date of the study
#' @param take_first TRUE: takes the minimum vaccine date .
#' FALSE: takes the closest to end_cohort
#' @return immunization date
#' @examples
#' # load package example data
#' data("cohortdata")
#' # define end date of the study as type date
#' end_cohort <- as.Date("2044-12-31")
#' # get immunization dates
#' cohortdata$immunization <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = end_cohort,
#'   take_first = FALSE
#' )
#'
#' # view head of data
#' head(cohortdata)
#' @export
get_immunization_date <- function(data,
                                  outcome_date_col,
                                  censoring_date_col = NULL,
                                  outcome_delay,
                                  immunization_delay,
                                  vacc_date_col,
                                  end_cohort,
                                  take_first = FALSE) {
  # check inputs
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  checkmate::assert_string(
    outcome_date_col
  )
  checkmate::assert_character(
    vacc_date_col,
    min.len = 1L
  )

  # check data has expected column names
  checkmate::assert_names(
    names(data),
    must.include = c(outcome_date_col, vacc_date_col)
  )

  # check if date columns are date type
  checkmate::assert_date(
    data[[outcome_date_col]]
  )
  for (i in seq_along(vacc_date_col)) {
    checkmate::assert_date(
      data[[vacc_date_col[i]]]
    )
  }

  # check outcome and immunization delay
  stopifnot(
    "Please provide a non-null integer number greater or equal than 0
    in `outcome_delay`. Use round(`outcome_delay`,0)" =
      checkmate::test_integerish(outcome_delay, lower = 0, null.ok = FALSE),
    "Please provide a non-null integer number greater or equal than 0
    in `outcome_delay`. Use round(`immunization_delay`,0)" =
      checkmate::test_integerish(immunization_delay, lower = 0, null.ok = FALSE)
  )

  # expect end cohort is a date
  checkmate::assert_date(
    end_cohort, any.missing = FALSE, len = 1
  )

  # check take_first
  checkmate::assert_logical(
    take_first,
    len = 1L
  )

  # Check censoring_date_col if provided
  if (!is.null(censoring_date_col)) {
    checkmate::assert_names(
      colnames(data),
      must.include = censoring_date_col
    )
    checkmate::assert_date(
      data[[censoring_date_col]]
    )
    checkmate::assert_string(censoring_date_col)
  }

  # warn on year of cohort end date date
  max_year <- 2100 # a plausible maximum year
  end_year <- as.numeric(format(end_cohort, "%Y"))
  if (end_year > max_year) {
    warning(
      sprintf(
        "`end_cohort` has a date with year > %s, please check the date!",
        as.character(max_year)
      )
    )
  }

  # set vaccination status
  data$outcome_col <- set_status(data,
    outcome_date_col,
    operator = "&",
    status = c(1, 0)
  )

  # get immunization limit for individuals with available data
  # This can depend both on the characteristic delay time of the
  # outcome and the characteristic delay for immunization
  delta_limit <- outcome_delay + immunization_delay

  # Limit date
  # If censoring date provided, use it. If not, use outcome date
  limit_date <- data[[outcome_date_col]]

  if (!is.null(censoring_date_col)) {
    limit_date <- as.Date(ifelse(!is.na(data[[censoring_date_col]]),
      yes = as.character(data[[censoring_date_col]]),
      no = as.character(limit_date)
    ))
  }

  # get difference with outcome date

  data$imm_limit <- limit_date - delta_limit

  # all other individuals' limit is set to end_cohort
  data[is.na(data$imm_limit), "imm_limit"] <- end_cohort - delta_limit

  # get differences from vaccination dates
  cols_delta <- sprintf("delta_%i", seq_along(vacc_date_col))

  for (i in seq_along(cols_delta)) {
    # calculate values
    vals <- as.numeric(
      data$imm_limit - data[[vacc_date_col[i]]]
    )
    # set any values less than 0 to NA
    vals[vals < 0] <- NA
    # assign values
    data[[cols_delta[i]]] <- vals
  }

  # assign the lower value as delta_imm, keeping NAs where
  # only NAs are present
  # this `apply` replaces the earlier implementation and avoids
  # a dependency
  data$delta_imm <- apply(
    # apply min over each row of a dataframe of the delta columns
    # returning a vector of minimum delta values or NAs
    # set data[, cols_delta] as dataframe to avoid errors when
    # working with only one vaccination column
    data.frame(data[, cols_delta]),
    MARGIN = 1, FUN = function(x) {
      #When several vaccines the function returns the first date
      #that satisfies the limit date constraint if take_first = TRUE
      #(i.e. the max delta). If take_firt = FALSE it returns the
      #closest date to the event (i.e. the min delta).
      if (all(is.na(x))) {
        NA_real_
      } else {
        if (take_first) {
          max(x, na.rm = TRUE)
        } else {
          min(x, na.rm = TRUE)
        }
      }
    }
  )

  # immunization outcome as a vector
  imm_out_date <- data$imm_limit - data$delta_imm + immunization_delay

  return(imm_out_date)
}

#' Function to construct the time-to-event
#'
#' This function returns a column with the time-to-event in days occurred
#' until a reference outcome. The starting point to count the time-to-event
#' can be the immunization date, supposing that the vaccinate population
#' enters to the study, when they are vaccinated
#' (start_from_immunization=TRUE). Or the beginning of the study, if all the
#' cohort is known at this point (start_from_immunization=FALSE). In this last
#' case, it is not necessary to pass the argument {immunization_date_col}
#'
#' @inheritParams get_immunization_date
#' @param start_cohort start date of the study
#' @param start_from_immunization TRUE: starts counting time-to-event from
#' immunization date if available
#' FALSE: starts counting time-to-event from the start date of the cohort study
#' @param immunization_date_col name of the column that contains the
#' immunization date. Required if start_from_immunization = TRUE
#' @return time-to-event
#' @examples
#' # load package example data
#' data("cohortdata")
#' # define start and end dates of the study as type date
#' start_cohort <- as.Date("2044-01-01")
#' end_cohort <- as.Date("2044-12-31")
#' cohortdata$immunization <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = end_cohort,
#'   take_first = FALSE
#' )
#' cohortdata$time_to_death <- get_time_to_event(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   start_cohort = start_cohort,
#'   end_cohort = end_cohort,
#'   start_from_immunization = TRUE,
#'   immunization_date_col = "immunization"
#' )
#'
#' # view data
#' head(cohortdata)
#' @export
get_time_to_event <- function(data,
                              outcome_date_col,
                              censoring_date_col = NULL,
                              start_cohort, end_cohort,
                              start_from_immunization = FALSE,
                              immunization_date_col) {
  # add input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )

  checkmate::assert_string(outcome_date_col)

  checkmate::assert_names(
    colnames(data),
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
    data[[outcome_date_col]]
  )

  checkmate::assert_logical(
    start_from_immunization,
    len = 1L, any.missing = FALSE
  )

  #Checks of censoring_date_col if provided
  if (!is.null(censoring_date_col)) {
    checkmate::assert_names(
      colnames(data),
      must.include = censoring_date_col
    )
    checkmate::assert_date(
      data[[censoring_date_col]]
    )
    checkmate::assert_string(censoring_date_col)
  }

  # check immnunization date col if asked
  if (start_from_immunization) {
    stopifnot(
      "`immunization_date_col` must be provided, and a column name in `data`" =
        (!missing(immunization_date_col) &&
         checkmate::test_string(immunization_date_col) &&
         immunization_date_col %in% colnames(data))
    )

    # Check for date type column
    checkmate::assert_date(
      data[[immunization_date_col]]
    )
  }

  # Initialize vector with start point to calculate time-to-event
  # cohort start by default
  t0 <- rep(start_cohort, nrow(data))
  if (start_from_immunization) {
    # if start from immunization replace informed immunization dates
    t0 <- as.Date(ifelse(is.na(data[[immunization_date_col]]),
      yes = as.character(t0),
      no = as.character(data[[immunization_date_col]])
    ))
  }

  # Initialize vector with end point to calculate time-to-event
  # cohort end by default
  tf <- rep(end_cohort, nrow(data))
  # replace informed outcome dates
  tf <- as.Date(ifelse(!is.na(data[[outcome_date_col]]),
    yes = as.character(data[[outcome_date_col]]),
    no = as.character(tf)
  ))
  # replace censoring dates if provided
  if (!is.null(censoring_date_col)) {
    tf <- as.Date(ifelse(!is.na(data[[censoring_date_col]]),
      yes = as.character(data[[censoring_date_col]]),
      no = as.character(tf)
    ))
  }

  #time to event is simply the difference between tf and t0
  time_to_event <- as.numeric(tf - t0)
  return(time_to_event)
}

#' Function to construct dose associated to the immunization date
#'
#' This function returns the names of the columns associated
#' to the immunization date.
#' To avoid mistakes, it is necessary to set the same value of
#' immunization_delay that was used in the previous functions.
#'
#' @param data dataset with cohort information (see example)
#' @param immunization_date_col name of the column that contains the
#' immunization date.
#' @param vacc_date_col name of the column(s) that contains the vaccine date
#' @param immunization_delay characteristic time in days before the patient
#' is considered immune
#' @return dose: a column with the names of the columns that are associated to
#' the doses of each register
#' @examples
#' data("cohortdata")
#' cohortdata$immunization <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = as.Date("2044-12-31"),
#'   take_first = FALSE
#' )
#' cohortdata$immunization_dose <- get_immunization_dose(
#'   data = cohortdata,
#'   immunization_date_col = "immunization",
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   immunization_delay = 14
#' )
#' @export
get_immunization_dose <- function(data,
                                  immunization_date_col,
                                  vacc_date_col,
                                  immunization_delay) {
  # input checking
  # add input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  checkmate::assert_string(immunization_date_col)
  checkmate::assert_character(
    vacc_date_col,
    min.len = 1L
  )
  stopifnot(
    "Please provide a non-null integer number greater or equal than 0
    in `outcome_delay`. Use round(`immunization_delay`,0)" =
      checkmate::test_integerish(immunization_delay, lower = 0, null.ok = FALSE)
  )
  checkmate::assert_names(
    colnames(data),
    must.include = c(vacc_date_col, immunization_date_col)
  )
  checkmate::assert_date(
    data[[immunization_date_col]]
  )
  # The function can receive one or more column names in vacc_date_col
  vacc_date_col_ <- vacc_date_col # hard coded to return right error message
  for (vacc_date_col in vacc_date_col_) {
    checkmate::assert_date(
      data[[vacc_date_col]]
    )
  }

  # calculate the expected date of immunizing vaccination
  data$delta_imm <- data[[immunization_date_col]] - immunization_delay

  # get the first dose corresponding to immunization date - delay
  dose_number <- apply(data[, c(vacc_date_col_, "delta_imm")], 1, function(x) {
    which(x == x[length(x)])[1] # hard coded to get first value
  })

  # get names of the vaccination columns corresponding to the dose
  return(vacc_date_col_[dose_number])
}

#' Function to construct vaccine biologic associated to the immunization date
#'
#' This function returns the names of the vaccines
#' associated to the immunization date.
#' To avoid mistakes, it is necessary to set the same
#' value of immunization_delay that was used in the previous
#' functions.
#' The arguments vacc_date_col and vacc_name_col must be passed in the same
#' order, i.e. every name of column date must correspond
#' to a name of vaccine column (see example)
#'
#' @inheritParams get_immunization_dose
#' @param vacc_name_col name of the column(s) that contains the vaccine names
#' @return dose: a column with the names of the columns that are associated to
#' the doses of each register
#' @return vaccine name
#' @examples
#' # load example data
#' data("cohortdata")
#' cohortdata$immunization <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = as.Date("2044-12-31"),
#'   take_first = FALSE
#' )
#' cohortdata$immunization_vaccine <- get_immunization_vaccine(
#'   data = cohortdata,
#'   immunization_date_col = "immunization",
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   vacc_name_col = c("vaccine_1", "vaccine_2"),
#'   immunization_delay = 14
#' )
#'
#' # view data
#' head(cohortdata)
#' @export
get_immunization_vaccine <- function(data,
                                     immunization_date_col,
                                     vacc_date_col,
                                     vacc_name_col,
                                     immunization_delay) {
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  checkmate::assert_string(immunization_date_col)
  checkmate::assert_character(
    vacc_date_col,
    min.len = 1L
  )
  checkmate::assert_character(
    vacc_name_col,
    len = length(vacc_date_col)
  )
  stopifnot(
    "Please provide a non-null integer number greater or equal than 0
    in `outcome_delay`. Use round(`immunization_delay`,0)" =
      checkmate::test_integerish(immunization_delay, lower = 0, null.ok = FALSE)
  )
  checkmate::assert_names(
    colnames(data),
    must.include = c(vacc_date_col, immunization_date_col, vacc_name_col)
  )
  checkmate::assert_date(
    data[[immunization_date_col]]
  )
  # The function can receive one or more column names in vacc_date_col
  vacc_date_col_ <- vacc_date_col # hard coded to return right error message
  for (vacc_date_col in c(vacc_date_col_)) {
    checkmate::assert_date(
      data[[vacc_date_col]]
    )
  }

  # get the vaccine date corresponding to the immunizing dose
  immunizing_dose <- get_immunization_dose(
    data,
    immunization_date_col = immunization_date_col,
    vacc_date_col = vacc_date_col, immunization_delay = immunization_delay
  )

  # position of immunizing dose
  data$dose_index <- match(immunizing_dose, vacc_date_col)

  # get the vaccine name at the immunizing dose index
  immunizing_vaccine <- apply(
    data[, c(vacc_name_col, "dose_index")],
    MARGIN = 1, FUN = function(x) {
      index <- as.numeric(x[length(x)])
      x[index]
    }
  )
  return(immunizing_vaccine)
}

#' Function to calculate the vaccine coverage per dose
#'
#' This function returns the vaccination coverage of a dose along
#' the cohort study. The coverage can be calculated grouped by
#' year, day and month. This most by specified in the parameter unit.
#' If there are not registers for some dates, the function assigns 0,
#' instead of NA, to be able to calculate the cumulative coverage.
#'
#' @param data dataset with cohort information (see example)
#' @param vacc_date_col name of the column(s) that contains the vaccine date
#' to calculate the coverage
#' @param unit aggregation unit, must be either "year" or "month" or "day"
#' @param date_interval if NULL, the function calculates the coverage interval
#' based on the min() and max() of the vacc_date_col.
#' It is also possible to pass a custom date interval to truncate or expand the
#' date interval (see example)
#' @return data.frame with vaccine number of doses per date, cumulative count
#' of doses and vaccine coverage
#' @examples
#' data("cohortdata")
#' start_cohort <- as.Date("2044-01-01")
#' end_cohort <- as.Date("2044-12-31")
#' date_interval <- c(start_cohort, end_cohort)
#' coh_coverage(
#'   data = cohortdata,
#'   vacc_date_col = "vaccine_date_1",
#'   unit = "month",
#'   date_interval = date_interval
#' )
#' @export
coh_coverage <- function(data,
                         vacc_date_col,
                         unit = c("day", "month", "year"),
                         date_interval = NULL) {
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  checkmate::assert_character(
    vacc_date_col,
    min.len = 1L
  )
  checkmate::assert_names(
    colnames(data),
    must.include = c(vacc_date_col)
  )
  checkmate::assert_date(
    data[[vacc_date_col]]
  )
  unit <- match.arg(unit, several.ok = FALSE)
  checkmate::assert_string(
    unit
  )

  # Create continuous date column
  # For fixed intervals, use date_intervales
  # In other case use min and max of data
  if (is.null(date_interval)) {
    start <- min(data[[vacc_date_col]], na.rm = TRUE)
    end <- max(data[[vacc_date_col]], na.rm = TRUE)
  } else {
    checkmate::assert_date(
      date_interval
    )
    start <- date_interval[1]
    end <- date_interval[2]
  }
  start <- as.Date(trunc(as.POSIXlt(start), units = unit))
  dates <- seq(from = start, to = end, by = unit)
  coverage <- data.frame(date  = dates)

  count <- as.data.frame(
    table(
      cut(data[[vacc_date_col]], breaks = unit),
      dnn = "date"
    ),
    responseName = "doses"
  )
  count$date <- as.Date(count$date)
  coverage <- merge(x = coverage, y = count,
    by = "date", all.x = TRUE
  )
  coverage$doses[is.na(coverage$doses)] <- 0
  coverage$cum_doses <- cumsum(coverage$doses)
  coverage$coverage <- coverage$cum_doses / nrow(data)
  return(coverage)
}

#' @title Assign event status
#'
#' @description This function generates a binary status (1,0) associated
#' with an outcome. The value returned is 0 if a register is censored
#' before the outcome occurs or if the outcome does not occur during the
#' follow-up period. If the outcome occurs and the register is not
#' censored the value returned is 1.
#' @inheritParams get_immunization_date
#' @return status
#' @examples
#' # load package example data for cohort studies
#' data("cohortdata")
#'
#' # assign death status
#' cohortdata$death_status <- set_event_status(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   censoring_date_col = "death_other_causes"
#' )
#'
#' # view data with added column
#' head(cohortdata)
#' @export
set_event_status <- function(data,
                             outcome_date_col,
                             censoring_date_col = NULL) {
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_character(outcome_date_col,
    any.missing = FALSE, min.len = 1
  )
  checkmate::assert_names(
    names(data), must.include = outcome_date_col
  )

  data$outcome_status <- set_status(data = data,
    col_names = outcome_date_col,
    status = c(1, 0)
  )
  if (!is.null(censoring_date_col)) {
    checkmate::assert_character(censoring_date_col,
      any.missing = FALSE, min.len = 1
    )
    checkmate::assert_names(
      names(data), must.include = censoring_date_col
    )
    data$outcome_status <- ifelse(
      (!is.na(data[[censoring_date_col]])) &
        (!is.na(data[[outcome_date_col]])) &
        (data[[censoring_date_col]] <= data[[outcome_date_col]]),
      yes = "0",
      no = data$outcome_status
    )
  }
  return(data$outcome_status)
}
