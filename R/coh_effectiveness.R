#' @title Estimate Vaccine Effectiveness (VE)
#'
#' @description  This function provides methods for estimating VE. It relies
#' on the implementation of the Kaplan-Meier estimator and the Cox model for
#' proportional hazards in the package `{survival}`. Currently, the default
#' method of the function is VE = 1 - HR, where HR is the Hazard Ratio
#' calculated using the Cox model. The proportional hazards hypothesis is
#' tested using the Schoenfeld test, and the resultant p-value is provided in
#' the results. Log-log plots are also generated using the Kaplan-Meier
#' survival estimator to provide a visual test for the proportional hazards
#' hypothesis. The functions uses the names of columns provided in the tags
#' outcome_status_col, time_to_event_col and vaccine_status_col of the
#' `linelist` object and the status names provided in `make_vaccineff_data`.
#' The return is an `S3 class` object with the VE (CI95%),
#' the result of Cox model and the Kaplan-Meier estimator.
#' This object is compatible with the methods `summary` and `plot`.
#'
#' @param vaccineff_data Object of the class `vaccineff_data`, with the
#' cohort information.
#' @return Object of the class `effectiveness`: list with results from
#' estimation of VE.
#' `ve`: `data.frame` with VE(CI95%),
#' `cox_model`: `survival` object with results for cox model
#' `kaplan_meier`: `survival` object with Kaplan-Meier estimator
#' @examples
#' # Define start and end dates of the study
#' start_cohort <- as.Date("2044-01-01")
#' end_cohort <- as.Date("2044-12-31")
#'
#' # Create `data.frame` with information of immunization
#' cohortdata <- make_immunization(
#'   data_set = cohortdata,
#'   outcome_date_col = "death_date",
#'   censoring_date_col = "death_other_causes",
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_2"),
#'   end_cohort = end_cohort,
#'   take_first = FALSE
#' )
#' head(cohortdata)
#'
#' # Match the data
#' matching <- match_cohort(
#'   data_set = cohortdata,
#'   outcome_date_col = "death_date",
#'   censoring_date_col = "death_other_causes",
#'   start_cohort = start_cohort,
#'   end_cohort = end_cohort,
#'   method = "static",
#'   exact = "sex",
#'   nearest = c(age = 1)
#' )
#'
#' # Extract matched data
#' cohortdata_match <- get_dataset(matching)
#'
#' # Calculate vaccine effectiveness
#' ve <- effectiveness(
#'   data_set = cohortdata_match,
#'   start_cohort = start_cohort,
#'   end_cohort = end_cohort
#' )
#'
#' # View summary of VE
#' summary(ve)
#'
#' # Generate plot of method
#' plot(ve)
#' @export

effectiveness <- function(vaccineff_data) {

  stopifnot("Input must be an object of class 'vaccineff_data'" =
      checkmate::test_class(vaccineff_data, "vaccineff_data")
  )

  if (!is.null(vaccineff_data$matching)) {
    data_set <- vaccineff_data$matching$match
    tags <- linelist::tags(data_set)
  } else {
    data_set <- vaccineff_data$cohort_data
    tags <- linelist::tags(data_set)
  }

  eff_obj <- coh_eff_hr(
    data_set = data_set,
    outcome_status_col = tags$outcome_status_col,
    time_to_event_col = tags$time_to_event_col,
    vacc_status_col = tags$vacc_status_col,
    vaccinated_status = vaccineff_data$vaccinated_status,
    unvaccinated_status = vaccineff_data$unvaccinated_status,
    start_cohort = vaccineff_data$start_cohort,
    end_cohort = vaccineff_data$end_cohort
  )
  # effectiveness object
  class(eff_obj) <- "effectiveness"

  # return
  return(eff_obj)
}

#' @title Summarize VE Results
#'
#' @description Summarizes the results of `effectiveness`.
#'
#' @param object Object of the class `effectiveness`.
#' @param ... Additional arguments passed to other functions.
#' @return Summary of the results from effectiveness.
#' @export

summary.effectiveness <- function(object, ...) {
  # Check if the input object is of class "effectiveness"
  stopifnot("Input must be an object of class 'effectiveness'" =
      checkmate::test_class(object, "effectiveness")
  )
  cat(
    "Vaccine Effectiveness computed as VE = 1 - HR:\n"
  )
  print(object$ve)
  cat("\nSchoenfeld test for Proportional Hazards hypothesis:\n")
  cat(sprintf("p-value = %s\n", object$cox_model$p_value))
}

#' @title Function for Extracting VE plot
#'
#' @description This function extracts the plot generated
#' by `effectiveness`.
#'
#' @param x Object of the class `effectiveness`.
#' @param ... Additional arguments passed to other functions.
#' @return Plot extracted from `effectiveness`.
#' @export

plot.effectiveness <- function(x,
                               type = c("loglog", "surv"),
                               cumulative = FALSE,
                               percentage = FALSE,
                               ...) {
  # Check if the input object is of class "effectiveness"
  stopifnot("Input must be an object of class 'effectiveness'" =
      checkmate::test_class(x, "effectiveness")
  )
  # Check plot type options
  type <- match.arg(type)

  # Check percentage
  checkmate::assert_logical(
    percentage,
    len = 1L
  )

  # Check cumulative
  checkmate::assert_logical(
    cumulative,
    len = 1L
  )

  if (type == "loglog") {
    plt <- plot_loglog(x$kaplan_meier)
  } else if (type == "surv") {
    plt <- plot_survival(x$kaplan_meier,
      percentage = percentage,
      cumulative = cumulative
    )
  }
  return(plt)
}
