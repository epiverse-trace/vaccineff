#' @title Construct Information of Immunization
#'
#' @description This function returns a `data.frame` with the relevant
#' information on immunization for the study. An individual is considered
#' immunized after the characteristic time for the immune response of the
#' vaccine passes. This time is provided to the function by the parameter
#' `immunization_delay`. By default, it is set to 0. The function searches
#' for vaccine dates based on the end of the follow-up period of each
#' individual defined by the censoring date (if provided), outcome delay
#' (if present), and end of the study.
#'
#' The function also works with vaccination information spread across several
#' columns. If this is the case, the parameter `vacc_date_col` must be passed
#' as a vector with the names of all the columns to use (see example).
#' The function uses by default the latest date found. However, it can also
#' select the first date by setting `take_first = TRUE`.
#'
#' The function returns a column with the immunization date (`immunization`)
#' and a vaccine status column (`vaccine_status`) that is constructed based on
#' `immunization`. For several vaccines, the function also returns the name of
#' the column of the vaccine that was selected as immunizing
#' (`immunizing_dose`). If different custom names (e.g., brands) are associated
#' with each vaccine date, the function can return the custom name of the
#' vaccine selected as immunizing. This information must be passed in the
#' parameter `vacc_name_col`, as a vector in the same order as `vacc_date_col`.
#'
#' @param data_set `data.frame` with cohort information (see example).
#' @param outcome_date_col Name of the column that contains the outcome dates.
#' @param censoring_date_col Name of the column that contains the censoring
#' date. NULL by default.
#' @param immunization_delay Characteristic time in days before the patient
#' is considered immune.
#' @param vacc_date_col Name of the column(s) that contain the vaccine dates.
#' @param vacc_name_col Name of the column(s) that contain custom vaccine
#' names for the vaccines (e.g. brand name, type of vaccine)
#' @param vaccinated_status Status assigned to the vaccinated population.
#' Default is `v`.
#' @param unvaccinated_status Status assigned to the unvaccinated population.
#' Default is `u`.
#' @param end_cohort End date of the study.
#' @param take_first `FALSE`: takes the latest vaccine date. `TRUE`: takes the
#' earliest vaccine date.
#' @return Original `data.frame` passed in `data_set` and additional columns
#' containing information on the immunization.
#' @keywords internal

make_immunization <- function(data_set,
                              outcome_date_col,
                              censoring_date_col = NULL,
                              vacc_date_col,
                              vacc_name_col = NULL,
                              vaccinated_status = "v",
                              unvaccinated_status = "u",
                              immunization_delay = 0,
                              end_cohort,
                              take_first = FALSE) {
  # check inputs
  checkmate::assert_data_frame(
    data_set,
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
    names(data_set),
    must.include = c(outcome_date_col, vacc_date_col)
  )

  # check if date columns are date type
  checkmate::assert_date(
    data_set[[outcome_date_col]]
  )
  for (i in seq_along(vacc_date_col)) {
    checkmate::assert_date(
      data_set[[vacc_date_col[i]]]
    )
  }

  # check outcome and immunization delay
  stopifnot(
    "Please provide a non-null integer number greater or equal than 0
    in `immunization_delay`. Use round(`immunization_delay`,0)" =
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

  checkmate::assert_string(censoring_date_col, null.ok = TRUE)
  checkmate::assert_names(
    colnames(data_set),
    must.include = censoring_date_col
  )
  # check censoring_date_col if provided
  if (!is.null(censoring_date_col)) {
    checkmate::assert_date(
      data_set[[censoring_date_col]]
    )
  }

  # check vacc_name_col if provided
  checkmate::assert_character(
    vacc_name_col,
    min.len = length(vacc_date_col),
    null.ok = TRUE
  )
  checkmate::assert_names(
    names(data_set),
    must.include = c(vacc_name_col)
  )

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

  # get immunization date
  data_set$immunization_date <- get_immunization_date(
    data_set = data_set,
    outcome_date_col = outcome_date_col,
    immunization_delay = immunization_delay,
    vacc_date_col = vacc_date_col,
    end_cohort = end_cohort,
    take_first = take_first
  )

  # name of column
  if (length(vacc_date_col) > 1) {
    data_set$immunizing_vaccine <- get_immunization_dose(
      data_set = data_set,
      immunization_date_col = "immunization_date",
      vacc_date_col = vacc_date_col,
      immunization_delay = immunization_delay
    )
  }

  # name of vaccine
  if (!is.null(vacc_name_col)) {
    data_set$immunizing_vaccine_name <- get_immunization_vaccine(
      data_set = data_set,
      immunization_date_col = "immunization_date",
      vacc_date_col = vacc_date_col,
      vacc_name_col = vacc_name_col,
      immunization_delay = immunization_delay
    )
  }

  # set vaccine status
  data_set$vaccine_status <- set_status(
    data_set = data_set,
    col_names = "immunization_date",
    status = c(vaccinated_status, unvaccinated_status)
  )

  # return `data.frame` with information on immunization
  return(data_set)

}

#' @title Construct Immunization Date
#'
#' @description This function constructs the immunization date per individual
#' by searching for the vaccine dates that satisfy the condition:
#' `vacc_date_col + immunization_delay <= limit_date`, where
#' `limit_date` is defined following the hierarchy:
#' `censoring_date_col`, `outcome_date_col`, `end_cohort`. If a date is not
#' provided/found the function takes the next one in the hierarchy.
#' If several columns with vaccine dates are provided, the function selects
#' by default the closest vaccine date to `limit_date`. However, it can also
#' select the first vaccine date by setting `take_first = TRUE`.
#'
#' @inheritParams make_immunization
#' @return Immunization date
#' @keywords internal
get_immunization_date <- function(data_set,
                                  outcome_date_col,
                                  censoring_date_col = NULL,
                                  immunization_delay,
                                  vacc_date_col,
                                  end_cohort,
                                  take_first = FALSE) {

  # get immunization limit for individuals with available data
  # This can depend both on the characteristic delay time of the
  # outcome and the characteristic delay for immunization
  delta_limit <- immunization_delay

  # Limit date
  # If censoring date provided, use it. If not, use outcome date
  limit_date <- data_set[[outcome_date_col]]

  if (!is.null(censoring_date_col)) {
    limit_date <- as.Date(ifelse(!is.na(data_set[[censoring_date_col]]),
      yes = as.character(data_set[[censoring_date_col]]),
      no = as.character(limit_date)
    ))
  }

  # get difference with outcome date

  data_set$imm_limit <- limit_date - delta_limit

  # all other individuals' limit is set to end_cohort
  data_set[is.na(data_set$imm_limit), "imm_limit"] <- end_cohort - delta_limit

  # get differences from vaccination dates
  cols_delta <- sprintf("delta_%i", seq_along(vacc_date_col))

  data_set[cols_delta] <- lapply(vacc_date_col, function(col) {
    vals <- as.numeric(data_set$imm_limit - data_set[[col]])
    vals[vals < 0] <- NA
    vals
  })

  # assign the lower value as delta_imm, keeping NAs where
  # only NAs are present
  # this `apply` replaces the earlier implementation and avoids
  # a dependency
  data_set$delta_imm <- apply(
    # apply min over each row of a `data.frame` of the delta columns
    # returning a vector of minimum delta values or NAs
    # set data_set[, cols_delta] as dataframe to avoid errors when
    # working with only one vaccination column
    data.frame(data_set[, cols_delta]),
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
  imm_out_date <- data_set$imm_limit - data_set$delta_imm +
    immunization_delay

  return(imm_out_date)
}

#' @title Return Column Name Associated to Immunizing Vaccine
#'
#' @description This function is used in cases where several columns with
#' vaccine dates are provided by the user. It returns the name of the column
#' of the vaccine used as immunizing. The parameters are set in agreement with
#' `get_immunization_date`.
#'
#' @inheritParams make_immunization
#' @return Name of the column taken as immunizing vaccine for each register.
#' @keywords internal
get_immunization_dose <- function(data_set,
                                  immunization_date_col,
                                  vacc_date_col,
                                  immunization_delay) {
  # The function can receive one or more column names in vacc_date_col
  vacc_date_col_ <- vacc_date_col # hard coded to return right error message
  for (vacc_date_col in vacc_date_col_) {
    checkmate::assert_date(
      data_set[[vacc_date_col]]
    )
  }

  # calculate the expected date of immunizing vaccination
  data_set$delta_imm <- data_set[[immunization_date_col]] -
    immunization_delay

  # get the first dose corresponding to immunization date - delay
  dose_number <- apply(data_set[, c(vacc_date_col_, "delta_imm")], 1,
    function(x) {
      which(x == x[length(x)])[1] # hard coded to get first value
    }
  )

  # get names of the vaccination columns corresponding to the dose
  return(vacc_date_col_[dose_number])
}

#' @title Construct Vaccine Name Associated to Immunizing Vaccine
#'
#' @description This function is used in cases where several columns with
#' vaccine dates and different names are provided by the user. It returns a
#' column with the name of the vaccine used as immunizing. The parameters are
#' set in agreement with `get_immunization_date`.
#'
#' @inheritParams make_immunization
#' @return Custom vaccine names of the immunizing vaccine.
#' @keywords internal

get_immunization_vaccine <- function(data_set,
                                     immunization_date_col,
                                     vacc_date_col,
                                     vacc_name_col,
                                     immunization_delay) {

  # get the vaccine date corresponding to the immunizing dose
  immunizing_dose <- get_immunization_dose(
    data_set,
    immunization_date_col = immunization_date_col,
    vacc_date_col = vacc_date_col, immunization_delay = immunization_delay
  )

  # position of immunizing dose
  data_set$dose_index <- match(immunizing_dose, vacc_date_col)

  # get the vaccine name at the immunizing dose index
  immunizing_vaccine <- apply(
    data_set[, c(vacc_name_col, "dose_index")],
    MARGIN = 1, FUN = function(x) {
      index <- as.numeric(x[length(x)])
      x[index]
    }
  )
  return(immunizing_vaccine)
}
