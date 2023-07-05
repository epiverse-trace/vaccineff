

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