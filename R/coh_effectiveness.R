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
effectiveness <- function(data,
                          outcome_status_col,
                          time_to_event_col,
                          vacc_status_col,
                          vaccinated_status,
                          unvaccinated_status,
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

  # class effectiveness

  # select estimation method

  # effectiveness object
  
}
