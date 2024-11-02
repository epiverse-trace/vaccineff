#' @title Estimate Vaccine Effectiveness (VE)
#'
#' @description This function provides methods for estimating VE. It relies on
#' the Kaplan-Meier estimator and the Cox model for proportional hazards
#' from the `{survival}` package. Currently, the default method is VE = 1 - HR,
#' where HR is the Hazard Ratio calculated using the Cox model. The
#' proportional hazards assumption is tested using the Schoenfeld test,
#' with the p-value provided in the results. Log-log plots are also generated
#' using the Kaplan-Meier estimator for a visual test of the proportional
#' hazards hypothesis. The function uses column names provided in the tags
#' `outcome_status_col`, `time_to_event_col`, and `vaccine_status_col` of the
#' `linelist` object and status names from `make_vaccineff_data`.
#' The return is an `S3 class` object with the VE (CI95%), results from the Cox
#' model, and the Kaplan-Meier estimator. This object is compatible with
#' `summary` and `plot` methods.
#'
#' @param vaccineff_data Object of the class `vaccineff_data` with
#' vaccineff data.
#' @param at Number of days at which VE is estimated from the beginning of the
#' follow-up period.
#' @return Object of the class `effectiveness`: a list with results from the
#' estimation of VE.
#' `ve`: `data.frame` with VE(CI95%)
#' `cox_model`: `survival` object with Cox model results
#' `kaplan_meier`: `survival` object with Kaplan-Meier estimator
#' @examples
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
#' # Estimate the Vaccine Effectiveness (VE)
#' ve <- effectiveness(vaccineff_data, 90)
#'
#' # Print summary of VE
#' summary(ve)
#'
#' # Generate loglog plot to check proportional hazards
#' plot(ve, type = "loglog")
#'
#' # Generate Survival plot
#' plot(ve, type = "surv", percentage = FALSE, cumulative = FALSE)
#' @export

effectiveness <- function(vaccineff_data,
                          at) {

  stopifnot("Input must be an object of class 'vaccineff_data'" =
      checkmate::test_class(vaccineff_data, "vaccineff_data")
  )

  # Check numeric argument for at
  checkmate::test_integerish(at, lower = 0, null.ok = FALSE)

  if (!is.null(vaccineff_data$matching)) {
    data_set <- vaccineff_data$matching$match
  } else {
    data_set <- vaccineff_data$cohort_data
  }

  tags <- linelist::tags(data_set)

  data_set <- get_time_to_event_at(
    data_set = data_set,
    outcome_date_col = tags$outcome_date_col,
    censoring_date_col = tags$censoring_date_col,
    end_cohort = vaccineff_data$end_cohort,
    at = at
  )

  eff_obj <- coh_eff_hr(
    data_set = data_set,
    outcome_status_col = "outcome_status",
    time_to_event_col = "time_to_event",
    vacc_status_col = tags$vacc_status_col,
    vaccinated_status = vaccineff_data$vaccinated_status,
    unvaccinated_status = vaccineff_data$unvaccinated_status,
    start_cohort = vaccineff_data$start_cohort,
    end_cohort = vaccineff_data$end_cohort
  )
  # Estimation at
  eff_obj$at <- at
  # effectiveness object
  class(eff_obj) <- "effectiveness"

  ## return
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
  if (!is.null(object$at)) {
    cat(
      sprintf("Vaccine Effectiveness at %i days computed as VE = 1 - HR:\n",
              object$at)
    )
  } else {
    cat(
      "Vaccine Effectiveness at the end of study computed as VE = 1 - HR:\n"
    )
  }

  print(object$ve)
  cat("\nSchoenfeld test for Proportional Hazards assumption:\n")
  cat(sprintf("p-value = %s\n", object$cox_model$p_value))
  if (object$cox_model$p_value < 0.05) {
    message <- paste0("\np-value < 0.05. Please check loglog plot",
      " for Proportional Hazards assumption"
    )
    warning(message)
  }
}

#' @title Function for Extracting Vaccine Effectiveness Plot
#'
#' @description This function creates plots from an object of class
#' `effectiveness`. It returns a Log-Log plot when `type = "loglog"`,
#' or a Survival curve when `type = "surv"`. Survival plots can be
#' shown as cumulative incidence (`cumulative = TRUE`), and using
#' percentages (`percentage = TRUE`).
#'
#' @param x Object of class `effectiveness`.
#' @param type Type of plot. Options are `loglog` and `surv`.
#' @param cumulative If `TRUE`, the survival curve is shown as cumulative
#' incidence.
#' @param percentage If `TRUE`, results are shown on a percentage scale.
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
