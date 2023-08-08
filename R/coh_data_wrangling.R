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
  # TODO: more specific checks on the operator
  operator <- match.arg(operator, several.ok = FALSE)
  checkmate::assert_string(
    operator,
    n.chars = 1L
  )

  # check the status vector
  # TODO: ideally limit status to character vector
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
#' If a register presents an outcome, the function search for the closest
#' vaccine date before the outcome that satisfies the condition:
#' vacc_date_col <= outcome_date_col - delay_time - immunization_delay.
#' This condition allows to discriminate the vaccine dates in
#' terms of characteristic times in days (delay_time)  associated to an
#' outcome, from the onset of symptoms or from any reference event, and the
#' characteristic time in days before the patient is considered
#' immune (immunization_delay).
#' Both parameters can be set to zero by the user without
#' affecting the results.
#' If a register does not present an outcome, the immunization
#' date can be construct using the closest vaccine
#' date to the end of the study (take_first = FALSE), or the
#' first vaccination date found (take_first = TRUE).
#' Notice that the function works for one or several vaccines.
#' In case of several vaccines, the parameter
#' {outcome_date_col must} be passed as a vector (see example)
#'
#' @param data dataset with cohort information (see example)
#' @param outcome_date_col name of the column that contains
#' the outcome dates
#' @param outcome_delay characteristic time in days of the outcome
#' from the reference event
#' @param immunization_delay characteristic time in days before the patient
#' is considered immune
#' @param vacc_date_col name of the column(s) that contains the vaccine dates
#' @param end_cohort end date of the study
#' @param take_first TRUE: takes the minimum vaccine date for
#' registers without outcome.
#' FALSE: takes closest to end_cohort
#' @return immunization date
#' @examples
#' # load package example data
#' data("cohortdata")
#'
#' # get immunization dates
#' cohortdata$immunization_death <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = "2021-12-31",
#'   take_first = FALSE
#' )
#'
#' # view head of data
#' head(cohortdata)
#' @export
get_immunization_date <- function(data,
                                  outcome_date_col,
                                  outcome_delay,
                                  immunization_delay,
                                  vacc_date_col,
                                  end_cohort,
                                  take_first = TRUE) {
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
  # TODO: check that outcome, vax, and end date columns are of type Date
  # or coercible to date

  # check outcome and immunization delay as a number - may be decimal for now
  # TODO: consider restricting to a count or integerish
  checkmate::assert_number(
    outcome_delay,
    lower = 0, finite = TRUE
  )
  checkmate::assert_number(
    immunization_delay,
    lower = 0, finite = TRUE
  )

  # expect end cohort is a string - will be coerced to date later
  checkmate::assert_string(
    end_cohort
  )

  # check take_first
  checkmate::assert_logical(
    take_first,
    len = 1L
  )

  # convert end_cohort column
  # TODO: expect specific format
  end_cohort <- as.Date(end_cohort)

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

  # get immunisation limit for individuals with available data
  # first get outcome-immunisation delay difference
  # assume both are in days
  outcome_imm_difference <- outcome_delay - immunization_delay
  # get difference with outcome date
  data$imm_limit <- as.Date(data[[outcome_date_col]]) - outcome_imm_difference

  # all other individuals' limit is set to end_cohort
  data[is.na(data$imm_limit), "imm_limit"] <- end_cohort

  # get differences from vaccination dates
  cols_delta <- sprintf("delta_%i", seq_along(vacc_date_col))

  for (i in seq_along(cols_delta)) {
    # calculate values
    vals <- as.numeric(
      data$imm_limit - as.Date(data[[vacc_date_col[i]]])
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
    data[, cols_delta],
    MARGIN = 1, FUN = function(x) {
      if (all(is.na(x))) {
        NA_real_
      } else {
        min(x, na.rm = TRUE)
      }
    }
  )

  # immunization outcome as a vector
  imm_out_date <- data$imm_limit - data$delta_imm + immunization_delay

  # for option `take_first`
  if (take_first) {
    ## Take the minimum immunization date
    data$min_imm <- apply(
      data[, vacc_date_col],
      MARGIN = 1,
      FUN = function(x) {
        if (all(is.na(x))) {
          NA_character_
        } else {
          min(x, na.rm = TRUE)
        }
      }
    )
    data$min_imm <- as.Date(data$min_imm) + immunization_delay

    # get the immunization date based on vax status as a vector
    imm_date <- data$min_imm
    # if individuals are vaccinated, assign the `imm_out_date`
    imm_date[data$outcome_col] <- imm_out_date

    return(imm_date)
  } else {
    ## Take the closest date to end_cohort
    return(imm_out_date)
  }
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
#' @param data dataset with cohort information (see example)
#' @param outcome_date_col name of the column that contains
#' the outcome dates
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
#' @param start_from_immunization TRUE: starts counting time-to-event from
#' immunization date if available
#' FALSE: starts counting time-to-event from the start date of the cohort study
#' @param immunization_date_col name of the column that contains the
#' immunization date. Required if start_from_immunization = TRUE
#' @return time-to-event
#' @examples
#' # load package example data
#' data("cohortdata")
#' cohortdata$immunization_death <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = "2021-12-31",
#'   take_first = FALSE
#' )
#' cohortdata$time_to_death <- get_time_to_event(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   start_cohort = "2021-01-01",
#'   end_cohort = "2021-12-31",
#'   start_from_immunization = TRUE,
#'   immunization_date_col = "immunization_death"
#' )
#'
#' # view data
#' head(cohortdata)
#' @export
get_time_to_event <- function(data, outcome_date_col,
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
  # check strings, converted to date later
  checkmate::assert_string(start_cohort)
  checkmate::assert_string(end_cohort)

  checkmate::assert_logical(
    start_from_immunization,
    len = 1L, any.missing = FALSE
  )

  # check immnunization date col if asked
  if (start_from_immunization) {
    stopifnot(
      "`immunization_date_col` must be provided, and a column name in `data`" =
        (!missing(immunization_date_col) &&
         checkmate::test_string(immunization_date_col) &&
         immunization_date_col %in% colnames(data))
    )
  }

  # convert strings to dates
  start_cohort <- as.Date(start_cohort)
  end_cohort <- as.Date(end_cohort)

  # convert outcome date to Date type
  data[[outcome_date_col]] <- as.Date(data[[outcome_date_col]])

  if (start_from_immunization) {
    # convert date columns to Date type
    data[[immunization_date_col]] <- as.Date(data[[immunization_date_col]])

    # calculate time to event as a vector
    time_to_event <- data[[outcome_date_col]] -
      data[[immunization_date_col]]

    # handle different cases of NAs
    time_to_event <- ifelse(
      (is.na(time_to_event)) &
        (is.na(data[[immunization_date_col]])),
      yes = data[[outcome_date_col]] - start_cohort,
      no = time_to_event
    )

    time_to_event <- ifelse(
      (is.na(time_to_event)) &
        (is.na(data[[immunization_date_col]])) &
        (is.na(data[[outcome_date_col]])
        ),
      yes = end_cohort - start_cohort,
      no = time_to_event
    )

    time_to_event <- ifelse(
      (is.na(time_to_event)) &
        (is.na(data[[outcome_date_col]])
        ),
      yes = end_cohort - data[[immunization_date_col]],
      no = time_to_event
    )

    # handle case of immunization date being after cohort end date
    # return NA here
    time_to_event <- ifelse(
      !is.na(data[[immunization_date_col]]) &
        data[[immunization_date_col]] > end_cohort,
      yes = NA_real_,
      no = time_to_event
    )

    # handle case of full immunization being after death date
    # i.e., person dies after last dose but within the immunity delay time
    # return NA as well
    time_to_event <- ifelse(
      !is.na(data[[outcome_date_col]]) &
        !is.na(data[[immunization_date_col]]) &
        (data[[outcome_date_col]] < data[[immunization_date_col]]),
      yes = NA_real_,
      no = time_to_event
    )

    return(time_to_event)
  } else {
    # condition where the start date is not from immunization
    # difference between outcomes and start of the cohort study
    time_to_event <- data[[outcome_date_col]] - start_cohort

    time_to_event <- ifelse(
      (is.na(time_to_event)) &
        (is.na(data[[outcome_date_col]])
        ),
      yes = end_cohort - start_cohort,
      no = time_to_event
    )
    return(time_to_event)
  }
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
#' \dontrun{
#' data("cohortdata")
#' cohortdata$immunization_death <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = "2021-12-31",
#'   take_first = FALSE
#' )
#' cohortdata$immunization_dose <- get_immunization_dose(
#'   data = cohortdata,
#'   immunization_date_col = "immunization_death",
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   immunization_delay = 14
#' )
#' }
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
  checkmate::assert_number(
    immunization_delay,
    lower = 0, finite = TRUE
  )
  checkmate::assert_names(
    colnames(data),
    must.include = c(vacc_date_col, immunization_date_col)
  )

  # calculate the expected date of immunizing vaccination
  data$delta_imm <- data[[immunization_date_col]] - immunization_delay

  # get the first dose corresponding to immunization date - delay
  dose_number <- apply(data[, c(vacc_date_col, "delta_imm")], 1, function(x) {
    which(x == x[length(x)])[1] # hard coded to get first value
  })

  # get names of the vaccination columns corresponding to the dose
  return(vacc_date_col[dose_number])
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
#' cohortdata$immunization_death <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = "2021-12-31",
#'   take_first = FALSE
#' )
#' cohortdata$immunization_vaccine <- get_immunization_vaccine(
#'   data = cohortdata,
#'   immunization_date_col = "immunization_death",
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
  checkmate::assert_number(
    immunization_delay,
    lower = 0, finite = TRUE
  )
  checkmate::assert_names(
    colnames(data),
    must.include = c(vacc_date_col, immunization_date_col, vacc_name_col)
  )

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
