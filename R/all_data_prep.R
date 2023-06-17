#' Method for constructing age-group variable from age column
#' 
#' This method splits an age interval from min_val to max_val into (max_val-min_val)/step intervals. 
#' By default min_val is set 0, however it can be assigned by convenience. 
#' If the method finds ages greater or equal than max_val it assigns the string ">{max_val}".
#' To avoir errors it is necessary to set step < max_val. 
#' It is also suggested to choose the step such that max_val%%(step+1) = 0
#' @param data dataset with at least a column containing the age information
#' @param col_age name of the column containing the age information
#' @param  max_val maximum value of age interval to split
#' @param  step step used to split the age interval
#' @param  min_val minimum value of age interval to split
#' @return age_group
#' @examples
#' \dontrun{
#' data("cohortdata")
#' cohortdata$age.group <- get_age_group(cohortdata, "age", 80, 9)
#' }
#' @export
get_age_group <- function(data, col_age, max_val, step, min_val = 0) {
    n_steps <- as.integer((max_val-min_val)/step) + 1
    limits_low <- c(as.integer(seq(min_val, max_val, length.out = n_steps)))
    limits_hgh <- limits_low + step
    lim_labels <- paste(as.character(limits_low), as.character(limits_hgh),
                        sep = "-")
    lim_labels[length(lim_labels)] <- paste0("+",
                                            limits_low[length(limits_low)])
    lim_breaks <- c(-Inf, limits_low[2:length(limits_low)] - 1, Inf)

    age_group <- cut(data[[col_age]],
                    breaks = lim_breaks,
                    labels = lim_labels)
    return(age_group)
}