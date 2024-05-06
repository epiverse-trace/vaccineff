#' Internal function to extract summary output from survival models.
#'
#' @param model survival object with model
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
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
#' @inheritParams coh_eff_noconf
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
#' @return Data frame with data from KM model:
#' time to event
#' survivial probability (CI95%)
#' cumulative incidence (CI95%)
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

  return(km)
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
#' status
#' @return summary: hazards ratio (CI95%), vaccine effectiveness (CI95%),
#' and p-value from Schoenfeld test
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
#' coh_eff_noconf(
#'   data = cohortdata,
#'   outcome_status_col = "death_status",
#'   time_to_event_col = "time_to_death",
#'   vacc_status_col = "vaccine_status"
#' )
#' @export
coh_eff_noconf <- function(data,
                           outcome_status_col,
                           time_to_event_col,
                           vacc_status_col,
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

  data[[vacc_status_col]] <- factor(
    data[[vacc_status_col]],
    levels = c(vaccinated_status, unvaccinated_status),
    ordered = FALSE
  )
  data[[vacc_status_col]] <- stats::relevel(
    data[[vacc_status_col]], ref = unvaccinated_status
  )

  indiv_survival <- survival::Surv( # nolint
    data[[time_to_event_col]], data[[outcome_status_col]]
  )

  # cox regression
  cx <- survival::coxph(
    indiv_survival ~ data[[vacc_status_col]]
  )

  # Test the Proportional Hazards Assumption
  test <- survival::cox.zph(cx)
  hr <- round(exp(stats::coef(cx)), digits = 4)

  # extract first and second element as limits
  ci025 <- round(exp(stats::confint(cx)), 4)[1]
  ci975 <- round(exp(stats::confint(cx)), 4)[2]

  # extract from matrix by name
  p <- test$table["GLOBAL", "p"]

  eff <- data.frame(
    VE = 1 - hr,
    `lower .95` = 1 - ci975,
    `upper .95` = 1 - ci025
  )

  return(list(effectiveness = eff, ph = p))
}
