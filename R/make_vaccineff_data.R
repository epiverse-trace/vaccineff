#' @title Construct `vaccineff_data` Object
#'
#' @description This function constructs an S3 object of the class
#' `vaccineff_data` that contains all the relevant information for the study.
#' to estimate the effectiveness.
#'
#' @param data_set `data.frame` with cohort information (see example).
#' @param outcome_date_col Name of the column that contains the outcome dates.
#' @param censoring_date_col Name of the column that contains the censoring
#' date. NULL by default.
#' @param vacc_date_col Name of the column(s) that contain the vaccine dates.
#' @param vacc_name_col Name of the column(s) that contain custom vaccine
#' names for the vaccines (e.g. brand name, type of vaccine). If provided,
#' must be of the same length as `vacc_date_col`.
#' @param vaccinated_status Status assigned to the vaccinated population.
#' Default is `v`.
#' @param unvaccinated_status Status assigned to the unvaccinated population.
#' Default is `u`.
#' @param immunization_delay Characteristic time in days before the patient
#' is considered immune. Default is 0.
#' @param end_cohort End date of the study.
#' @param match `TRUE`: cohort matching is performed. Default is `FALSE`
#' @param exact Name(s) of column(s) for `exact` matching. Default is `NULL`.
#' @param nearest Named vector with name(s) of column(s) for `nearest` matching
#' and caliper(s) for each variable (e.g., `nearest = c("characteristic1" = n1,
#' "characteristic2" = n2)`, where `n1` and `n2` are the calipers). Default is
#' `NULL`.
#' @param take_first `FALSE`: takes the latest vaccine date. `TRUE`: takes the
#' earliest vaccine date.
#' @param t0_follow_up Column with the initial dates of the follow-up period.
#' This column is only used if `match = FALSE`. If not provided, the follow-up
#' period starts at `start_cohort`. Default is NULL.
#' @return An S3 object of class `vaccineff_data` with all the information and
#' characteristics of the study. `data.frames` are converted into an object of
#' class `linelist` to easily handle with the data.
#' @examples
#'
#' # Load example data
#' data("cohortdata")
#'
#' # Create `vaccineff_data`
#' vaccineff_data <- make_vaccineff_data(data_set = cohortdata,
#'   outcome_date_col = "death_date",
#'   censoring_date_col = "death_other_causes",
#'   vacc_date_col = "vaccine_date_2",
#'   vaccinated_status = "v",
#'   unvaccinated_status = "u",
#'   immunization_delay = 15,
#'   end_cohort = as.Date("2044-12-31"),
#'   match = TRUE,
#'   exact = c("age", "sex"),
#'   nearest = NULL
#' )
#'
#' # Print summary of data
#' summary(vaccineff_data)
#' @export

make_vaccineff_data <- function(data_set,
                                outcome_date_col,
                                censoring_date_col = NULL,
                                vacc_date_col,
                                vacc_name_col = NULL,
                                vaccinated_status = "v",
                                unvaccinated_status = "u",
                                immunization_delay = 0,
                                end_cohort,
                                match = FALSE,
                                exact = NULL,
                                nearest = NULL,
                                take_first = FALSE,
                                t0_follow_up = NULL) {
  # check inputs
  check_vaccineff_inputs(
    data_set = data_set,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    vacc_date_col = vacc_date_col,
    vacc_name_col = vacc_name_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status,
    immunization_delay = immunization_delay,
    end_cohort = end_cohort,
    match = match,
    exact = exact,
    nearest = nearest,
    take_first = take_first
  )

  cohort_data <- linelist::make_linelist(
    x = as.data.frame(
      make_immunization(
        data_set = data_set,
        outcome_date_col = outcome_date_col,
        censoring_date_col = censoring_date_col,
        vacc_date_col = vacc_date_col,
        vacc_name_col = vacc_name_col,
        vaccinated_status = vaccinated_status,
        unvaccinated_status = unvaccinated_status,
        immunization_delay = immunization_delay,
        end_cohort = end_cohort,
        take_first = take_first
      )
    )
  )

  # Define linelist object
  cohort_data <- linelist::set_tags(
    x = cohort_data,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    vacc_date_col = vacc_date_col,
    vacc_name_col = vacc_name_col,
    immunization_date_col = "immunization_date",
    vacc_status_col = "vaccine_status",
    allow_extra = TRUE
  )

  # Define start date of the cohort
  start_cohort <- min(cohort_data$immunization_date, na.rm = TRUE)

  if (match) {
    output <- capture_warnings(match_cohort(
      data_set = cohort_data,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      start_cohort = start_cohort,
      end_cohort = end_cohort,
      method = "static",
      exact = exact,
      nearest = nearest,
      immunization_date_col = "immunization_date",
      vacc_status_col = "vaccine_status",
      vaccinated_status = vaccinated_status,
      unvaccinated_status = unvaccinated_status
    ))

    matching <- output$result
    warnings_log <- output$warnings

    matching$match <- linelist::set_tags(
      x = matching$match,
      time_to_event_col = "time_to_event",
      outcome_status_col = "outcome_status",
      t0_follow_up_col = "t0_follow_up",
      allow_extra = TRUE
    )
  } else {
    matching <- NULL

    if (!is.null(t0_follow_up)) {
      start_from_immunization <- TRUE
    } else {
      start_from_immunization <- FALSE
    }

    cohort_data$outcome_status <- set_event_status(
      data_set = cohort_data,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col
    )

    cohort_data$time_to_event <- get_time_to_event(
      data_set = cohort_data,
      outcome_date_col = outcome_date_col,
      start_cohort = start_cohort,
      end_cohort = end_cohort,
      start_from_immunization = start_from_immunization,
      immunization_date_col = t0_follow_up
    )

    output <- capture_warnings(adjust_time_to_event(
      data_set = cohort_data
    ))

    cohort_data <- output$result
    warnings_log <- output$warnings

    cohort_data <- linelist::set_tags(
      x = cohort_data,
      time_to_event_col = "time_to_event",
      outcome_status_col = "outcome_status",
      allow_extra = TRUE
    )
  }

  vaccineff_data <- list(
    cohort_data = cohort_data,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status,
    immunization_delay = immunization_delay,
    matching = matching,
    warnings_log = warnings_log
  )

  class(vaccineff_data) <- "vaccineff_data"

  return(vaccineff_data)
}

#' @title Summarize Vaccineff Data
#'
#' @description Summarizes the results of `make_vaccineff_data`.
#'
#' @param object Object of the class `vaccineff_data`.
#' @param warnings_log If `TRUE`, prints the warnings log.
#' @param ... Additional arguments passed to other functions.
#' @return Summary of the results from vaccineff data
#' @export

summary.vaccineff_data <- function(object, warnings_log = FALSE, ...) {
  # Check if the input object is of class "match"
  stopifnot("Input must be an object of class 'vaccineff_data'" =
      checkmate::test_class(object, "vaccineff_data")
  )

  cat(paste0("\nCohort start: ", object$start_cohort))
  cat(paste0("\nCohort end: ", object$end_cohort, "\n"))

  # Extract tags
  tags <- linelist::tags(object$cohort_data)

  # Summary
  if (!is.null(object$matching)) {
    cat("\nNearest neighbors matching iteratively performed.\n")
    summary.match(object$matching)

    if (warnings_log && !is.null(object$matching)) {
      cat("\nWarnings:\n")
      cat(object$warnings_log, sep = "")
    }

  } else {
    summ_cohort <- match_summary(
      all = object$cohort_data,
      matched = NULL,
      vacc_status_col = tags$vacc_status_col
    )
    cat("\nNo matching routine invoked.\n")
    print(summ_cohort)

    cat(object$warnings_log, sep = "")
  }

  # Print tags from linelist object
  tags_txt <- paste(names(tags), unlist(tags), sep = ":", collapse = ", ")
  if (tags_txt == "") {
    tags_txt <- "[no tagged variable]"
  }
  cat("\n// tags:", tags_txt, "\n")

}
