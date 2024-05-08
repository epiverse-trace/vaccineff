#' Internal function to extract summary output from survival models.
#'
#' @inheritParams coh_effectiveness
#' @param model survival object with model
#' @return Data frame with survival data
#' @keywords internal
extract_surv_model <- function(model, start_cohort, end_cohort) {
  days <- end_cohort - start_cohort
  tte <- seq(0, as.numeric(days) - 1, by = 1)
  res <- summary(model, times = tte, scale = 1)
  cols <- lapply(c(2:6, 8:16), function(x) res[x])
  tbl <- do.call(data.frame, cols)
  tbl$date <- tbl$time + start_cohort
  return(tbl)
}

#' Internal function to calculate Kapplan-Meier model and related metrics.
#'
#' @inheritParams coh_effectiveness
#' @return Data frame with data from KM model
#' "time", "date", "strata",
#' "n.risk", "n.event", "n.censor",
#' "surv", "lower", "upper",
#' "cumincidence", "cumincidence_lower", "cumincidence_upper"
#' @keywords internal
km_model <- function(data,
                     outcome_status_col,
                     time_to_event_col,
                     vacc_status_col,
                     vaccinated_status,
                     unvaccinated_status,
                     start_cohort,
                     end_cohort) {
  # KM model time to event, outcome ~ vaccine status
  model <- survival::survfit(
    survival::Surv(
      data[[time_to_event_col]],
      data[[outcome_status_col]]
    ) ~ data[[vacc_status_col]]
  )
  # Extract data from survival element
  km <- extract_surv_model(model, start_cohort, end_cohort)

  # Construct cumulative incidence = 1 - S
  km$cumincidence <- 1 - km$surv
  km$cumincidence_lower <- 1 - km$upper
  km$cumincidence_upper <- 1 - km$lower

  # Construct strata data
  km$strata <- factor(km$strata,
    levels = c(
      paste0("data[[vacc_status_col]]=", vaccinated_status),
      paste0("data[[vacc_status_col]]=", unvaccinated_status)
    )
  )
  levels(km$strata) <- c(vaccinated_status, unvaccinated_status)

  km <- subset(km,
    select = c("time", "date", "strata",
      "n.risk", "n.event", "n.censor",
      "surv", "lower", "upper",
      "cumincidence", "cumincidence_lower", "cumincidence_upper"
    )
  )

  return(km)
}

#' Internal function to calculate Cox-PH model and related metrics.
#'
#' @inheritParams coh_effectiveness
#' @return List with data from Cox model:
#' hr - hazard ratio (CI95%)
#' p_value
#' survival object with model
#' survival object with Schoenfeld test
#' @keywords internal
cox_model <- function(data,
                      outcome_status_col,
                      time_to_event_col,
                      vacc_status_col,
                      vaccinated_status,
                      unvaccinated_status) {

  # Prepare data for model
  data[[vacc_status_col]] <- factor(
    data[[vacc_status_col]],
    levels = c(vaccinated_status, unvaccinated_status),
    ordered = FALSE
  )

  data[[vacc_status_col]] <- stats::relevel(
    data[[vacc_status_col]], ref = unvaccinated_status
  )

  # Cox model time to event, outcome ~ vaccine status
  # Regression
  model <- survival::coxph(
    survival::Surv( # nolint
      data[[time_to_event_col]], data[[outcome_status_col]]
    ) ~ data[[vacc_status_col]]
  )

  ## Hazard ratio
  hr <- round(exp(stats::coef(model)), digits = 4)
  # CI(95%): extract first and second element as limits
  lower <- round(exp(stats::confint(model)), 4)[1]
  upper <- round(exp(stats::confint(model)), 4)[2]

  ## Schoenfled test for Proportional Hazards hypothesis
  test <- survival::cox.zph(model)
  # extract from matrix by name
  p_value <- round(test$table["GLOBAL", "p"], 4)

  cx <- list(hr = hr,
    lower = lower,
    upper = upper,
    p_value = p_value,
    model = model,
    test = test
  )

  return(cx)
}

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
#' @param data dataset with cohort information (see example)
#' @param outcome_status_col name of the column containing status of the
#' event (most be a binary column)
#' @param time_to_event_col name of the column containing the time-to-event
#' @param vacc_status_col name of the column containing the vaccination
#' @param vaccinated_status string assigned to the vaccinated population
#' @param unvaccinated_status string assigned to the unvaccinated population
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
#' status
#' @return vaccine effectiveness (CI95%), Schoenfeld test,
#' Loglog plot for Proportional Hazards
#' @examples
#' # load example data from package
#' data("cohortdata")
#'
#' # add immunization dates
#' cohortdata$immunization <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = as.Date("2044-12-31"),
#'   take_first = FALSE
#' )
#'
#' # add vaccine status
#' cohortdata$vaccine_status <- set_status(
#'   data = cohortdata,
#'   col_names = c("immunization"),
#'   status = c("v", "u")
#' )
#'
#' # add death status
#' cohortdata$death_status <- set_status(
#'   data = cohortdata,
#'   col_names = c("death_date")
#' )
#'
#' # add time to death
#' cohortdata$time_to_death <- get_time_to_event(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   start_cohort = as.Date("2044-01-01"),
#'   end_cohort = as.Date("2044-12-31"),
#'   start_from_immunization = FALSE
#' )
#'
#' # estimate vaccine effectiveness
#' coh_effectiveness(
#'   data = cohortdata,
#'   outcome_status_col = "death_status",
#'   time_to_event_col = "time_to_death",
#'   vacc_status_col = "vaccine_status",
#'   start_cohort = start_cohort,
#'   end_cohort = end_cohort
#' )
#' @export
coh_effectiveness <- function(data,
                              outcome_status_col,
                              time_to_event_col,
                              vacc_status_col,
                              vaccinated_status = "v",
                              unvaccinated_status = "u",
                              start_cohort,
                              end_cohort) {

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

  # p-value for Schoenfeld test
  test <- paste0("Schoenfeld test for Proportional Hazards hypothesis: ",
    cx$p_value
  )

  # output
  ve <- list(
    call = cx$model$call,
    ve = effectiveness,
    prop_hazards = test,
    loglog = loglog
  )

  return(ve)
}
