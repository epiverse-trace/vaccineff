#' Function to estimate the vaccine effectiveness based on the vaccination
#' status.
#'
#' The function relies on the implementation of the Cox model for proportional
#' hazards on survival package. It returns a dataframe with the summary of the
#' estimation that includes the value of the hazard ratio (CI95%), the vaccine
#' effectiveness (CI95%), and the result of the test for the Proportional
#' Hazards hypothesis. If the test is rejected, it is recomended to try other
#' statistical strategies, e.g. stratifying the dataset by confounders or
#' including time-dependent variables.
#'
#' @inheritParams effectiveness
#' @return vaccine effectiveness (CI95%), Schoenfeld test,
#' Loglog plot for Proportional Hazards
#' @keywords internal
effectiveness <- function(data,
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
  effectiveness <- data.frame(
    VE = 1 - cx$hr,
    lower.95 = 1 - cx$upper,
    upper.95 = 1 - cx$lower
  )
  row.names(effectiveness) <- NULL

  # output
  ve <- list(
    call = cx$model$call,
    ve = effectiveness,
    p_value = cx$p_value,
    loglog = loglog
  )

  return(ve)
}
