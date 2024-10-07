#' @title Check Correctness of `data_set`
#'
#' @description This auxiliary function checks that the type of the object
#' provided in `data_set` is a `data.frame` and that the specified columns
#' are included in it.
#'
#' @param data_set `data.frame`.
#' @param columns A vector of column names that should be present in
#' `data_set`.
#'
#' @return This function does not return a value.
#'
#' @keywords internal

check_dataset <- function(data_set, columns) {
  # check for data.frame type
  checkmate::assert_data_frame(
    data_set,
    min.rows = 1L
  )

  # check for columns included
  checkmate::assert_names(
    names(data_set),
    must.include = columns
  )
}

#' @title Check Correctness of Inputs in `make_vaccineff_data`
#'
#' @description This auxiliary function checks the correctness of the inputs
#' provided to `make_vaccineff_data`.
#'
#' @inheritParams make_vaccineff_data
#' @return This function does not return a value.
#'
#' @keywords internal

check_vaccineff_inputs <- function(data_set,
                                   outcome_date_col,
                                   censoring_date_col,
                                   vacc_date_col,
                                   vacc_name_col,
                                   vaccinated_status,
                                   unvaccinated_status,
                                   immunization_delay,
                                   end_cohort,
                                   match,
                                   exact,
                                   nearest,
                                   take_first) {
  # Check names of variables
  checkmate::assert_string(outcome_date_col)
  checkmate::assert_string(censoring_date_col, null.ok = TRUE)
  checkmate::assert_character(vacc_date_col, min.len = 1L)
  checkmate::assert_character(
    vacc_name_col,
    min.len = length(vacc_date_col),
    null.ok = TRUE
  )

  # Check data.frame
  check_dataset(data_set = data_set,
    columns = c(outcome_date_col, censoring_date_col,
                vacc_date_col, vacc_name_col)
  )

  # Check variable types
  checkmate::assert_date(
    data_set[[outcome_date_col]]
  )

  for (i in seq_along(vacc_date_col)) { #loop along vacc_date_col (can be many)
    checkmate::assert_date(
      data_set[[vacc_date_col[i]]]
    )
  }

  if (!is.null(censoring_date_col)) { #check censoring_date_col if provided
    checkmate::assert_date(
      data_set[[censoring_date_col]]
    )
  }

  stopifnot(
    # check for vaccinated/unvaccinated statues
    "`vaccinated_status` and `unvaccinated_status` cannot be equal" =
      (vaccinated_status != unvaccinated_status),
    # check immunization delay
    "Please provide a non-null integer number greater or equal than 0
    in `immunization_delay`. Use round(`immunization_delay`,0)" =
      checkmate::test_integerish(immunization_delay, lower = 0, null.ok = FALSE)
  )

  # check end dates
  # expect end cohort is a date
  checkmate::assert_date(
    end_cohort, any.missing = FALSE, len = 1
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

  # Checks for match if provided
  checkmate::assert_logical(
    match,
    len = 1L
  )

  if (match) {
    # Check if both 'exact' and 'nearest' are NULL or missing
    if (is.null(nearest) && is.null(exact)) {
      stop(
        "`exact` and `nearest` cannot be NULL. At least one must be provided"
      )
    }

    # checks for `nearest`
    checkmate::assert_numeric(
      nearest,
      any.missing = FALSE, min.len = 1, names = "named", null.ok = TRUE
    )
    checkmate::assert_names(
      names(data_set),
      must.include = names(nearest)
    )

    # checks for `exact`
    checkmate::assert_character(exact,
      any.missing = FALSE, min.len = 1, null.ok = TRUE
    )
    checkmate::assert_names(
      names(data_set),
      must.include = exact
    )
  }

  # check take_first
  checkmate::assert_logical(
    take_first,
    len = 1L
  )

}
