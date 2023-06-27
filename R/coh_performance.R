#' Function to test the proportional hazards hypothesis of the model.
#'
#' The function relies on the implementation of the Schoenfeld test for testing
#' the proportional hazards hypothesis on survival package.
#' It returns the result of the test and it can by plotted using plot().
#' If the test is rejected, it is recomended to try other statistical
#' strategies, e.g. stratifying the dataset by confounders or including
#' time-dependent variables.
#' @param data dataset with cohort information (see example)
#' @param outcome_status_col name of the column containing status of the
#' event (most be a binary column)
#' @param time_to_event_col name of the column containing the time-to-event
#' @param status_vacc_col name of the column containing the vaccination
#' status
#' @param p_thr p-value to test Proportional Hazards Hypothesis
#' @return Schoenfeld test: Survival element
#' @examples
#' \dontrun{
#' data(cohortdata)
#' cohortdata$immunization_death <- get_immunization_date(cohortdata,
#' "death_date",
#' 0,
#' 14,
#' c("vaccine_date_1", "vaccine_date_2"),
#' "2021-12-31", take_first = FALSE)
#' "2021-12-31",
#' take_first = FALSE)
#' cohortdata$vaccine_status <- set_status(cohortdata,
#' c("immunization_death"),
#' status = c("v", "u"))
#' cohortdata$death_status <- set_status(cohortdata,
#' c("death_date"))
#' cohortdata$time_to_death <- get_time_to_event(cohortdata, "death_date",
#' "2021-01-01", "2021-12-31",
#' FALSE)
#' test <- coh_test_noconf(cohortdata,
#' "death_status",
#' "time_to_death",
#' "vaccine_status")
#' plot(test)
#' }
#' @export
coh_test_noconf <- function(data, outcome_status_col,
                            time_to_event_col,
                            status_vacc_col,
                            p_thr = 0.05) {
  cx <- survival::coxph(survival::Surv(data[[time_to_event_col]],
                                       data[[outcome_status_col]])
                        ~ data[[status_vacc_col]])
  test <- survival::cox.zph(cx)
  return(test)
}