#' Function to test the proportional hazards hypothesis of the model.
#' The function relies on the implementation of the Scheunfeld test for testing
#' the proportional hazards hypothesis on survival package.
#' It returns the result of the test and it can by plotted using plot().
#' If the test is rejected, it is recomended to try other statistical
#' strategies, e.g. stratifying the dataset by confounders or including
#' time-dependent variables.
#' @param data dataset with at least one column to generate the status
#' @param outcome_status_col name of the column containing status of the
#' event (most be a binary column)
#' @param time_to_event_col name of the column containing the time-to-event
#' @param status_vacc_col name of the column containing the vaccination
#' status
#' @param p_thr p-value to test Proportional Hazards Hypothesis
#' @return age_group
#' @examples
#' \dontrun{
#' cohortdata <- data.frame()
#' cohortdata$immunization.death <- get_immunization_date(cohortdata,
#' "death.date",
#' 0,
#' 14,
#' c("vaccine.date.1", "vaccine.date.2"),
#' "2021-12-31", take_first = FALSE)
#' "2021-12-31",
#' take_first = FALSE)
#' cohortdata$vaccine.status <- set_status(cohortdata,
#' c("immunization.death"),
#' status = c("v", "u"))
#' cohortdata$death.status <- set_status(cohortdata,
#' c("death.date"))
#' cohortdata$time.to.death <- get_time_to_event(cohortdata, "death.date",
#' "2021-01-01", "2021-12-31",
#' FALSE)
#' test <- coh_test_noconf(cohortdata,
#' "death.status",
#' "time.to.death",
#' "vaccine.status")
#' plot(test)
#' }
#' @export
coh_test_noconf <- function(data, outcome_status_col,
                            time_to_event_col,
                            status_vacc_col,
                            p_thr = 0.05) {
  cx <- survival::coxph(survival::Surv(data[[time_to_event_col]], data[[outcome_status_col]])
              ~ data[[status_vacc_col]])
  test <- survival::cox.zph(cx)
  return(test)
}
