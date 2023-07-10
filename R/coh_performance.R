#' Function to test the proportional hazards hypothesis of the model.
#'
#' The function relies on the implementation of the Schoenfeld test for testing
#' the proportional hazards hypothesis on survival package.
#' It returns the result of the test and it can by plotted using plot().
#' If the test is rejected, it is recomended to try other statistical
#' strategies, e.g. stratifying the dataset by confounders or including
#' time-dependent variables.
#' @inheritParams coh_eff_noconf
#' @return Schoenfeld test: Survival element
#' @examples
#' # load example package data
#' data("cohortdata")
#'
#' # add useful columns
#' cohortdata$immunization_death <- get_immunization_date(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   outcome_delay = 0,
#'   immunization_delay = 14,
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
#' # perform the test
#' test <- coh_test_noconf(
#'   cohortdata,
#'   "death_status",
#'   "time_to_death",
#'   "vaccine_status"
#' )
#'
#' # View test object
#' test
#' @export
coh_test_noconf <- function(data, outcome_status_col,
                            time_to_event_col,
                            status_vacc_col,
                            p_thr = 0.05) {
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  checkmate::assert_names(
    names(data),
    must.include = c(outcome_status_col, time_to_event_col, status_vacc_col)
  )
  checkmate::assert_number(p_thr, lower = 0.0, upper = 1.0)

  # create survival object, exclude from object usage linting
  indiv_survival <- survival::Surv( # nolint
    data[[time_to_event_col]], data[[outcome_status_col]]
  )

  cx <- survival::coxph(indiv_survival ~ data[[status_vacc_col]])
  test <- survival::cox.zph(cx)

  return(test)
}
