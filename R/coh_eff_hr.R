#' @title Function to Estimate the Vaccine Effectiveness Using Hazard Ratio
#'
#' @description The function relies on the implementation of the Cox model for
#' proportional hazards from the survival package. The proportional hazards
#' hypothesis is tested using the Schoenfeld test, and the resultant p-value is
#' provided in the results. Log-log plots are also calculated using the
#' Kaplan-Meier survival estimator to provide a visual test for the proportional
#' hazards hypothesis. The function returns a list with the method called, the
#' estimation of VE (CI95%), the p-value of the Schoenfeld test, and the log-log
#' plot.
#'
#' @inheritParams effectiveness
#' @return method called, VE (CI95%), p-value from Schoenfeld test, and
#' log-log plot for proportional hazards
#' @keywords internal


coh_eff_hr <- function(data,
                       outcome_status_col,
                       time_to_event_col,
                       vacc_status_col,
                       vaccinated_status,
                       unvaccinated_status,
                       start_cohort,
                       end_cohort) {

  # Kapplan-Meier model for loglog curve
  km <- km_model(data = data,
    outcome_status_col = outcome_status_col,
    time_to_event_col = time_to_event_col,
    vacc_status_col = vacc_status_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status,
    start_cohort = start_cohort,
    end_cohort = end_cohort
  )

  # loglog plot
  loglog <- plot_loglog(km,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status
  )

  # Cox model
  cx <- cox_model(data = data,
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
    call = cx$model$call,
    ve = eff,
    test = cx$p_value,
    plot = loglog,
    method = "HR"
  )

  return(ve)
}
