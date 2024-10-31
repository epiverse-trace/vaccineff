#' @title Internal function to extract summary output from `{survival}` models.
#'
#' @inheritParams estimate_vaccineff
#' @param model `{survival}` object containing the model
#' @return `data.frame` with survival data
#' @keywords internal
extract_surv_model <- function(model, start_cohort, end_cohort) {
  days <- end_cohort - start_cohort
  tte <- seq(0, as.numeric(days) - 1, by = 1)
  res <- summary(model, times = tte, scale = 1)
  cols <- lapply(c(2:6, 8:16), function(x) res[x])
  tbl <- data.frame(cols)
  tbl$date <- tbl$time + start_cohort
  return(tbl)
}

#' @title Internal function to calculate Kaplan-Meier model and related metrics.
#'
#' @inheritParams estimate_vaccineff
#' @return `data.frame` with data from KM model:
#' "time", "date", "strata",
#' "n.risk", "n.event", "n.censor",
#' "surv", "lower", "upper",
#' "cumincidence", "cumincidence_lower", "cumincidence_upper"
#' @keywords internal
km_model <- function(data_set,
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
      data_set[[time_to_event_col]],
      data_set[[outcome_status_col]]
    ) ~ data_set[[vacc_status_col]]
  )
  # Extract data from `{survival}` object
  km <- extract_surv_model(model, start_cohort, end_cohort)

  # Construct cumulative incidence = 1 - S
  km$cumincidence <- 1 - km$surv
  km$cumincidence_lower <- 1 - km$upper
  km$cumincidence_upper <- 1 - km$lower

  # Construct strata data
  km$strata <- factor(km$strata,
    levels = c(
      paste0("data_set[[vacc_status_col]]=", vaccinated_status),
      paste0("data_set[[vacc_status_col]]=", unvaccinated_status)
    )
  )
  levels(km$strata) <- factor(
    c(vaccinated_status, unvaccinated_status),
    ordered = TRUE
  )

  km <- subset(km,
    select = c("time", "date", "strata",
      "n.risk", "n.event", "n.censor",
      "surv", "lower", "upper",
      "cumincidence", "cumincidence_lower", "cumincidence_upper"
    )
  )

  return(km)
}

#' @title Internal function to calculate Cox-PH model and related metrics.
#'
#' @inheritParams estimate_vaccineff
#' @return List with data from Cox model:
#' hr - hazard ratio (CI95%)
#' p_value
#' `{survival}` object with model
#' `{survival}` object with Schoenfeld test
#' @keywords internal
cox_model <- function(data_set,
                      outcome_status_col,
                      time_to_event_col,
                      vacc_status_col,
                      vaccinated_status,
                      unvaccinated_status) {

  # Prepare data for model
  data_set[[vacc_status_col]] <- factor(
    data_set[[vacc_status_col]],
    levels = c(vaccinated_status, unvaccinated_status),
    ordered = FALSE
  )

  data_set[[vacc_status_col]] <- stats::relevel(
    data_set[[vacc_status_col]], ref = unvaccinated_status
  )

  # Cox model time to event, outcome ~ vaccine status
  # Regression
  model <- survival::coxph(
    survival::Surv( # nolint
      data_set[[time_to_event_col]], data_set[[outcome_status_col]]
    ) ~ data_set[[vacc_status_col]]
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
