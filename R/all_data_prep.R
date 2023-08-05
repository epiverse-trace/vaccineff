#' @title Construct age-group variable from age column
#'
#' @description This method splits an age interval from `min_val` to `max_val`
#' into `(max_val - min_val) / step` intervals.
#' By default `min_val` is set to 0, however it can be assigned by
#' convenience.
#' If the method finds ages greater or equal than `max_val`
#' it assigns the string `">max_val"`.
#' To avoid errors it is necessary to set `step < max_val`.
#' It is also suggested to choose the step such
#' that `max_val %% (step + 1) == 0`.
#'
#' @param data dataset with at least a column containing the age
#' information
#' @param col_age name of the column containing the age
#' information
#' @param max_val maximum value of age interval to split
#' @param step step used to split the age interval
#' @param min_val minimum value of age interval to split
#' @return A `factor` object of the same length as the number of rows in `data`,
#' with levels corresponding to age bins between `min_val` and `max_val`.
#' Ages above `max_val` are represented as `>max_val`.
#' @export
#' @examples
#' # load data provided with the package
#' data(cohortdata)
#'
#' # assign age groups as a column of the data frame
#' cohortdata$age_group <- get_age_group(
#'   data = cohortdata,
#'   col_age = "age",
#'   max_val = 80,
#'   step = 9
#' )
#'
#' # view the data frame with new column
#' head(cohortdata)
get_age_group <- function(data, col_age, max_val, min_val = 0, step) {
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_string(col_age)
  checkmate::assert_names(
    names(data),
    must.include = col_age
  )
  checkmate::assert_number(min_val, lower = 0)
  checkmate::assert_number(max_val, lower = min_val)
  checkmate::assert_number(step, lower = 1, upper = max_val)

  # get breaks
  n_steps <- as.integer((max_val - min_val) / step) + 1
  limits_low <- seq.int(
    min_val, max_val,
    length.out = n_steps
  )
  limits_high <- limits_low + step

  # prepare labels
  lim_labels <- paste(limits_low, limits_high, sep = "-")
  lim_labels[length(lim_labels)] <- paste0(
    ">",
    limits_low[length(limits_low)]
  )
  lim_breaks <- c(-Inf, limits_low[seq(2, length(limits_low))] - 1, Inf)

  # cut the age data and apply labels
  age_group <- cut(data[[col_age]],
    breaks = lim_breaks,
    labels = lim_labels
  )

  return(age_group)
}
