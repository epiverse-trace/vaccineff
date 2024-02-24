#' @title Construct age-group variable from age column
#'
#' @description This method splits an age interval from `min_val` to `max_val`
#' into intervals of size `step`.
#' If the method finds ages greater or equal than `max_val`
#' it assigns the string `">max_val"`.
#' By default `min_val` is set to 0, however it can be assigned by
#' convenience. If the method finds ages lower or equal
#' than `min_val` it assigns the string `"<min_val-1"`.
#' The function warns when (max_val - min_val) is not an integer multiple of
#' step. In that case the last interval is truncated to the upper value
#' closest to max_val for which (closest_upper - min_val) is multiple of step.
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
  checkmate::assert_integerish(min_val, lower = 0, upper = max_val)
  checkmate::assert_integerish(max_val, lower = min_val)
  checkmate::assert_integerish(step, lower = 1, upper = max_val)

  # get breaks
  limits_low <- seq.int(min_val, max_val, by = step)
  limits_high <- limits_low + step - 1

  # prepare labels
  lim_labels <- paste(limits_low, limits_high, sep = "-")
  lim_labels[length(lim_labels)] <- paste0(
    ">", limits_low[length(limits_low)]
  )
  if (min_val == 0) {
    lim_breaks <- c(limits_low[seq(1, length(limits_low))] - 1, Inf)
  }  else {
    lim_labels <- c(paste0("<", min_val - 1), lim_labels)
    lim_breaks <- c(-Inf, limits_low[seq(1, length(limits_low))] - 1, Inf)
  }

  #Warning of module condition not satisfied
  if ((max_val - min_val) %% step != 0) {
    war_msg <- "(max_val - min_val) must be an integer multiple of step.
    The last interval will be truncated to "
    war_msg <- paste0(war_msg, lim_labels[length(lim_labels)])
    warning(war_msg)
  }

  # cut the age data and apply labels
  age_group <- cut(data[[col_age]],
    breaks = lim_breaks,
    labels = lim_labels
  )

  return(age_group)
}
