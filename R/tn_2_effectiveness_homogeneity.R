
#' Title
#'
#' @param data
#' @param vaccination_column
#' @param screening_column
#' @param confounders
#' @param design
#'
#' @return
#' @export
#'
#' @examples
effectiveness_homogeneity <- function(data, vaccination_column,
                                      screening_column, confounders,
                                      design) {

  # discretise the screening column
  data$screen <- 1
  idx = which(data[[screening_column]]=="Neg")
  data$screen[idx] = 0
  data$screen <- as.factor(as.character(data$screen))

  # get the 3D matrix of contingency tables
  d1 <- data[[vaccination_column]]
  d2 <- data[["screen"]]
  d3 <- data[[confounders]]
  mat_3D <- with(data, table(d1,
                             d2,
                             d3))

  # perform the test
  epiDisplay::mhor(mhtable=mat_3D, design = design)
}
