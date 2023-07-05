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
#' @param status_vacc_col name of the column containing the vaccination
#' status
#' @param p_thr p-value to test Proportional Hazards Hypothesis
#' @return summary: hazards ratio (CI95%), vaccine effectiveness (CI95%),
#' and Schoenfeld test
#' @examples
#' \dontrun{
#' data("cohortdata")
#' cohortdata$immunization_death <- get_immunization_date(
#'   date = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunisation_delay = 14,
#'   vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'   end_cohort = "2021-12-31",
#'   take_first = FALSE
#' )
#'
#' cohortdata$vaccine_status <- set_status(
#'   data = cohortdata,
#'   col_names = c("immunization_death"),
#'   status = c("v", "u")
#' )
#'
#' cohortdata$death_status <- set_status(
#'   data = cohortdata,
#'   col_names = c("death_date")
#' )
#'
#' cohortdata$time_to_death <- get_time_to_event(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   start_cohort = "2021-01-01",
#'   end_cohort = "2021-12-31",
#'   FALSE
#' )
#'
#' coh_eff_noconf(
#'   cohortdata,
#'   "death_status",
#'   "time_to_death",
#'   "vaccine_status"
#' )
#' }
#' @export
coh_eff_noconf <- function(data,
                           outcome_status_col,
                           time_to_event_col,
                           status_vacc_col,
                           p_thr = 0.05) {
  cx <- survival::coxph(survival::Surv(
    data[[time_to_event_col]],
    data[[outcome_status_col]]
  )
  ~ data[[status_vacc_col]])
  test <- survival::cox.zph(cx)
  hr <- c(round(exp(stats::coef(cx)), 4)[1])
  ci025 <- c(round(exp(stats::confint(cx)), 4)[1])
  ci975 <- c(round(exp(stats::confint(cx)), 4)[2])
  p <- test$table[5]
  p_value <- c(format(p, digits = 3))
  if (p < p_thr) {
    ph <- "reject"
  } else {
    ph <- "accept"
  }
  df_summ <- data.frame(
    HR = hr,
    HR_low = ci025,
    HR_high = ci975,
    V_eff = 1 - hr,
    V_eff_low = 1 - ci975,
    V_eff_high = 1 - ci025,
    PH = ph,
    p_value = p_value
  )
  return(df_summ)
}
