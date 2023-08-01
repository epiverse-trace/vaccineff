#' Checks data before calculating vaccine effectiveness using the screening
#' method [`st_effectiveness()`]
#'
#' @inheritParams st_effectiveness
#' @param screening_date Specific date the data was collected
#' @param lower_age_limit Lower bound of age range
#' @param upper_age_limit Upper bound of age range
#'
#' @return Nothing, used for side-effects, fails if not correct format
#' @export
check_minimum_dataset <- function(data,
                                  screening_date, lower_age_limit,
                                  upper_age_limit, vaccinated_population,
                                  unvaccinated_population, vaccinated_outcome,
                                  unvaccinated_outcome){
  checkmate::assertDate(data[[screening_date]],
                        any.missing = FALSE,
                        null.ok = FALSE)

  checkmate::assertNumeric(data[[lower_age_limit]],
                           any.missing = FALSE,
                           null.ok = FALSE)

  checkmate::assertNumeric(data[[upper_age_limit]],
                           any.missing = FALSE,
                           null.ok = FALSE)

  checkmate::assertNumeric(data[[vaccinated_population]],
                           any.missing = FALSE,
                           null.ok = FALSE)

  checkmate::assertNumeric(data[[unvaccinated_population]],
                           any.missing = FALSE,
                           null.ok = FALSE)

  checkmate::assertNumeric(data[[vaccinated_outcome]],
                           any.missing = FALSE,
                           null.ok = FALSE)

  checkmate::assertNumeric(data[[unvaccinated_outcome]],
                           any.missing = FALSE,
                           null.ok = FALSE)

}
