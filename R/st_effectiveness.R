#' Calculate vaccine effectiveness with the screening method
#'
#' @param data A data frame with case, hospitalisation, and ICU information
#' @param vaccinated_population Character string with the name of the data column
#' with the vaccinated population data
#' @param unvaccinated_population Character string with the name of the data column
#' with the unvaccinated population data
#' @param vaccinated_outcome Character string with the name of the data column
#' with the outcome data for vaccinated population data
#' @param unvaccinated_outcome Character string with the name of the data column
#' with the outcome data for unvaccinated population data
#' @param interval Character string with the name of the data column
#' with the time interval (e.g. week)
#' @param group_by Character string with the name of the data column
#' with the variable to group by, or NULL
#'
#' @return A data frame with vaccine effectiveness over time and optionally for
#' each group
#' @export
#'
#' @examples
#' filtered_data <- readRDS(
#'   system.file(
#'     "extdata", "filtered_data.rds",
#'     package = "vaccineff",
#'     mustWork = TRUE
#'   )
#' )
#' st_effectiveness(
#'   data = filtered_data,
#'   vaccinated_population = "pob_comp",
#'   unvaccinated_population = "pob_no",
#'   vaccinated_outcome = "casos_comp",
#'   unvaccinated_outcome = "casos_no",
#'   interval = "fecha"
#' )
st_effectiveness <- function(data, vaccinated_population,
                             unvaccinated_population, vaccinated_outcome,
                             unvaccinated_outcome,
                             interval,
                             group_by = NULL) {
  warning(
    "Screening method for vaccine effectiveness can produce negative values",
    call. = FALSE
  )

  # input checking
  stopifnot("Data must be a data frame" = is.data.frame(data))

  data <- as.data.frame(data)

  if (is.null(group_by)) {
    by <- list(interval = as.vector(data[, interval]))

    cols <- "interval"
  } else {
    by <- list(group_by = as.vector(data[, group_by]),
               interval = as.vector(data[, interval]))

    cols <- c("group_by", "interval")
  }

  # aggregate by variable
  data <- stats::aggregate(
    data[, c(vaccinated_population, unvaccinated_population, vaccinated_outcome, unvaccinated_outcome)],
    by = by,
    FUN = sum
  )

  ppv <- data[, vaccinated_population] / (data[, vaccinated_population] + data[, unvaccinated_population])

  p_outcome_v <- data[, vaccinated_outcome] / (data[, vaccinated_outcome] + data[, unvaccinated_outcome])

  ve_outcome <- p_outcome_v / (1 - p_outcome_v)
  ve_ppv <- (1 - ppv) / ppv
  ve_product <- ve_outcome * ve_ppv
  ve <- 1 - ve_product

  out <- as.data.frame(cbind(data[, cols], vaccine_effectiveness = ve))
  colnames(out) <- c(cols, "vaccine_effectiveness")
  out$interval <- as.Date(out$interval, origin = "1970-01-01")

  return(out)
}
