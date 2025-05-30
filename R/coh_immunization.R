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
#' as a vector with the names of all the columns to use.
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
#' @param data_set `data.frame` with cohort information.
#' @param outcome_date_col Name of the column that contains the outcome dates.
#' @param censoring_date_col Name of the column that contains the censoring
#' date.
#' @param immunization_delay Characteristic time in days before the patient
#' is considered immune.
#' @param vacc_date_col Name of the column(s) that contain the vaccine dates.
#' @param vacc_name_col Name of the column(s) that contain custom vaccine
#' names for the vaccines (e.g. brand name, type of vaccine)
#' @param vaccinated_status Status assigned to the vaccinated population.
#' @param unvaccinated_status Status assigned to the unvaccinated population.
#' @param end_cohort End date of the study.
#' @param take_first `FALSE`: takes the latest vaccine date. `TRUE`: takes the
#' earliest vaccine date.
#' @return Original `data.frame` passed in `data_set` and additional columns
#' containing information on the immunization.
#' @keywords internal

make_immunization <- function(data_set,
                              outcome_date_col,
                              censoring_date_col,
                              vacc_date_col,
                              vacc_name_col,
                              vaccinated_status,
                              unvaccinated_status,
                              immunization_delay,
                              end_cohort,
                              take_first = FALSE) {
  # get immunization date
  data_set$immunization_date <- get_immunization_date(
    data_set = data_set,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
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
                                  censoring_date_col,
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
        return(NA_real_)
      }
      if (take_first) {
        max(x, na.rm = TRUE)
      } else {
        min(x, na.rm = TRUE)
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
  # calculate the expected date of immunizing vaccination
  data_set$delta_imm <- data_set[[immunization_date_col]] -
    immunization_delay

  # get the first dose corresponding to immunization date - delay
  dose_number <- apply(data_set[, c(vacc_date_col, "delta_imm")], 1,
    function(x) {
      which(x == x[length(x)])[1] # hard coded to get first value
    }
  )

  # get names of the vaccination columns corresponding to the dose
  return(vacc_date_col[dose_number])
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
