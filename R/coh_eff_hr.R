#' @title Function to Estimate the Vaccine Effectiveness Using Hazard Ratio
#'
#' @description The function relies on the implementation of the Cox model for
#' proportional hazards from the package `{survival}` The proportional hazards
#' hypothesis is tested using the Schoenfeld test, and the resultant p-value is
#' provided in the results. Log-log plots are also calculated using the
#' Kaplan-Meier survival estimator to provide a visual test for the proportional
#' hazards hypothesis. The function returns a list with the method called, the
#' estimation of VE (CI95%), the p-value of the Schoenfeld test, and the log-log
#' plot.
#'
#' @inheritParams estimate_vaccineff
#' @return VE (CI95%),
#' output from `cox_model`,
#' and output from `km_model`
#' @keywords internal


coh_eff_hr <- function(data_set,
                       outcome_status_col,
                       time_to_event_col,
                       vacc_status_col,
                       vaccinated_status,
                       unvaccinated_status,
                       start_cohort,
                       end_cohort) {

  # Kaplan-Meier model for loglog curve
  km <- km_model(data_set = data_set,
    outcome_status_col = outcome_status_col,
    time_to_event_col = time_to_event_col,
    vacc_status_col = vacc_status_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status,
    start_cohort = start_cohort,
    end_cohort = end_cohort
  )

  # Cox model
  cx <- cox_model(data_set = data_set,
    outcome_status_col = outcome_status_col,
    time_to_event_col = time_to_event_col,
    vacc_status_col = vacc_status_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status
  )

  # Vaccine effectiveness = 1 - HR
  eff <- data.frame(
    VE = 1 - cx$hr,
    lower.95 = 1 - cx$upper,
    upper.95 = 1 - cx$lower
  )
  row.names(eff) <- NULL

  # output
  ve <- list(
    ve = eff,
    cox_model = cx,
    kaplan_meier = km
  )

  return(ve)
}
