#' @title Estimate Vaccine Effectiveness (VE)
#'
#' @description  This function provides methods for estimating VE. It relies
#' on the implementation of the Kaplan-Meier estimator and the Cox model for
#' proportional hazards in the package `{survival}`. Currently, the default
#' method of the function is `HR` (Hazard Ratio). Thus, the VE = 1 - HR, where
#' HR is calculated using the Cox model. The proportional hazards hypothesis is
#' tested using the Schoenfeld test, and the resultant p-value is provided in
#' the results. Log-log plots are also generated using the Kaplan-Meier
#' survival estimator to provide a visual test for the proportional hazards
#' hypothesis. The functions uses the default name of columns
#' `outcome_status`, `time_to_event` and `vaccine_status`, and the  default
#' status names `v` and `u`. However, custom names can be provided through the
#' parameters `outcome_status_col`, `time_to_event_col`, `vacc_status_col`,
#' `vaccinated_status` and `unvaccinated_status`.
#' The return is a list with the call and the name of the method used for
#' the estimation of VE (CI95%), the result of the performance test, and
#' a suitable plot for the method.
#' The object returned is compatible with the methods `summary` and `plot`.
#'
#' @param data `data.frame` with cohort information (see example).
#' @param start_cohort Start date of the study.
#' @param end_cohort End date of the study.
#' @param method Method to estimate VE. Default is `HR`.
#' @param outcome_status_col Name of the column containing status of the event
#' (must be a binary column). Default is `outcome_status`.
#' @param time_to_event_col Name of the column containing the time-to-event.
#' Default is `time_to_event`.
#' @param vacc_status_col Name of the column containing the vaccination.
#' Default is `vaccine_status`.
#' @param vaccinated_status Status assigned to the vaccinated population.
#' Default is `v`.
#' @param unvaccinated_status Status assigned to the unvaccinated population.
#' Default is `u`.
#' @return Object of the class `effectiveness`: list with results from
#' estimation of VE.
#' `call`: call of `{survival}` method,
#' `ve`: `data.frame` with VE(CI95%),
#' `test`: result from test of performance,
#' `plot`: plot of method,
#' `method`: name of the method used for the estimation.
#' @examples
#' # Define start and end dates of the study
#' start_cohort <- as.Date("2044-01-01")
#' end_cohort <- as.Date("2044-12-31")
#'
#' # Create `data.frame` with information of immunization
#' cohortdata <- make_immunization(
#'   data = cohortdata,
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
#'   data = cohortdata,
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
#' cohortdata_match <- dataset(matching)
#'
#' # Calculate vaccine effectiveness
#' ve <- effectiveness(
#'   data = cohortdata_match,
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

effectiveness <- function(data,
                          start_cohort,
                          end_cohort,
                          method = "HR",
                          outcome_status_col = "outcome_status",
                          time_to_event_col = "time_to_event",
                          vacc_status_col = "vaccine_status",
                          vaccinated_status = "v",
                          unvaccinated_status = "u") {

  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  checkmate::assert_names(
    names(data),
    must.include = c(outcome_status_col, time_to_event_col, vacc_status_col)
  )
  checkmate::assert_names(
    data[[vacc_status_col]],
    must.include = c(vaccinated_status, unvaccinated_status)
  )

  # check date types
  checkmate::assert_date(
    start_cohort, any.missing = FALSE, len = 1
  )
  checkmate::assert_date(
    end_cohort, any.missing = FALSE, len = 1
  )

  # select estimation method
  if (method == "HR") {
    eff_obj <- coh_eff_hr(
      data = data,
      outcome_status_col = outcome_status_col,
      time_to_event_col = time_to_event_col,
      vacc_status_col = vacc_status_col,
      vaccinated_status = vaccinated_status,
      unvaccinated_status = unvaccinated_status,
      start_cohort = start_cohort,
      end_cohort = end_cohort
    )
  }
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
    sprintf("Vaccine Effectiveness computed as VE = 1 - %s:\n",
            object$method)
  )
  print(object$ve)
  if (object$method == "HR") {
    cat("\nSchoenfeld test for Proportional Hazards hypothesis:\n")
    cat(sprintf("p-value = %s\n", object$test))
  }
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

plot.effectiveness <- function(x, ...) {
  # Check if the input object is of class "effectiveness"
  stopifnot("Input must be an object of class 'effectiveness'" =
      checkmate::test_class(x, "effectiveness")
  )
  return(x$plot)
}
