#' Title
#'
#' @param contingency_list
#'
#' @return
#' @export
#'
#' @examples
effectiveness <- function(contingency_list) {

  effectiveness_list <- NULL
  for (var in names(contingency_list)) {
    contingency_table <- contingency_list[[var]]
    numerator <- ((contingency_table["Vaccinated", "Infected"])/(contingency_table["Unvaccinated", "Infected"]))
    denominator <- ((contingency_table["Vaccinated", "Non-infected"])/(contingency_table["Unvaccinated", "Non-infected"]))
    effectiveness_list[[var]] <- 1-(numerator/denominator)
  }
  effectiveness_list
}
