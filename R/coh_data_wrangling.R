library(dplyr)
#' Function to assign the outcome status or the vaccination status
#' 
#' This function generates a binary status column using the set of columns, passed through the 
#' variable {col_names}. This columns must contain the information of the outcomes or the vaccine dates. 
#' It generates a logical codition using the operators "&" or "|", and evalutes if the registers in 
#' the columns contain or not information. 
#' If the logical operator is "&", the function returns 1, only if all the columns contain information. 
#' On the other hand, if the logical operator is "|", it is enough to find one column with information.
#' It is recommended to use this method when working with several outcomes or severa vaccine doses.
#' By default, it returns a binary column 0 means no outcome or no vaccine and 1 means the opposite. 
#' However, it can also receive personalized options, e.g. ["v", "u"] for unvaccinated and vaccinated.
#' 
#' @param data dataset with at least one column to generate the status
#' @param col_names name of the column containing the age information
#' @param operator logical operator to evaluate the condition
#' @param status binary set of status, e.g. c(1,0)
#' @return age_group
#' @examples
#' \dontrun{
#' data("cohortdata")
#' cohortdata$vaccine.status <- set_status(cohortdata, 
#'                                          c("vaccine.date.1", "vaccine.date.2"), 
#'                                          status = c("v", "u"))
#' }
#' @export
set_status <- function(data, col_names, 
                        operator = "&",
                        status = c(1, 0)) {
    condition <- "ifelse("
    int0 <- "(!is.na(data[["
    intf <- "]]))"
    i <- 1
    for (col_name in col_names){
        if (i == length(col_names)) {
            sep <- ""
        } else {
           sep <- operator
        }
        condition <- paste0(condition, int0, "'", col_name, "'", intf, sep)
        i <- i + 1
    }
    lst <- paste0(",", "'", status[1], "'", ",", "'", status[2], "'", ")")
    condition <- paste0(condition, lst)
    status_col <- eval(parse(text = condition))
    return(status_col)
}

#' Function to construct the inmunization date
#' 
#' This function returns a column with the immunization that corresponds to an analysed outcome.
#' If a register presents a outcome the function search for the closest vaccine date before the outcome
#' that satisfies the condition: vacc_date_col <= outcome_date_col - delay_time - immunization_delay.
#' This condition allows to discriminate the vaccine dates in terms of characteristic time in days 
#' (delay_time) associated to an outcome, from the onset of symptoms or from any reference event, and the 
#' characteristic time in days before the pacient is considered immune (immunization_delay). 
#' Both parameters can be set to zero by the user without affecting the results.
#' If a register does not present an outcome, the immunization date can be construct using the closest vaccine 
#' date to the end of the study (take_first = FALSE), or the first vaccination date found (take_first = TRUE).
#' Notice that the function works for one or several vaccines. In case of several vaccines, the parameter must
#' be passed as a vector (see example) 
#' 
#' @param data dataset with at least one column to generate the status
#' @param outcome_date_col name of the column that contains the outcome dates
#' @param outcome_delay characteristic time in days of the outcome from reference event
#' @param immunization_delay characteristic time in days before the pacient is considered immune 
#' @param vacc_date_col name of the column(s) that contains the vaccine dates
#' @param end_cohort end date of the study 
#' @param take_first TRUE: takes the minimum vaccine date for registers without outcome. 
#'                   FALSE: takes closest to end_cohort
#' @return status_col
#' @examples
#' \dontrun{
#' data("cohortdata")
#' cohortdata$vaccine.status <- set_status(cohortdata, 
#'                                          c("vaccine.date.1", "vaccine.date.2"), 
#'                                          status = c("v", "u"))
#' }
#' @export
get_immunization_date <- function(data, outcome_date_col,
                                  outcome_delay, immunization_delay,
                                  vacc_date_col, end_cohort,
                                  take_first = TRUE
                                 ) {

    data$outcome_col <- set_status(data, outcome_date_col, 
                                        operator = "&",
                                        status = c(1, 0))
    data$imm_limit <- data.table::fifelse(data$outcome_col == 1,
                                            as.Date(data[[outcome_date_col]]) -
                                            as.difftime(outcome_delay, unit = "days") -
                                            as.difftime(immunization_delay, unit = "days"),
                                            end_cohort)
    
    deltas <- c()
    for (i in seq_along(vacc_date_col)) {
        data[[paste0("delta", i)]] <-
             data$imm_limit - as.Date(data[[vacc_date_col[i]]])
        deltas <- c(deltas, paste0("delta", i))
        }
    n_deltas <- length(deltas) - 1
    data[, (ncol(data) - n_deltas):ncol(data)][
        data[, (ncol(data) - n_deltas):ncol(data)] < 0] <- NA
    data <- data %>%
            dplyr::mutate(delta_imm = pmin(!!!rlang::syms(deltas), na.rm = TRUE))
    data$imm_out_date <- data$imm_limit - data$delta_imm + immunization_delay
    if (take_first) {
        ## Take the minimum immunization date
        data <- data %>%
             dplyr::mutate(min_imm = pmin(!!!rlang::syms(vacc_date_col), na.rm = TRUE))
        data$min_imm <- as.Date(data$min_imm) + immunization_delay
        data$imm_date <- data.table::fifelse(data$outcome_col == 1,
                                                data$imm_out_date,
                                                data$min_imm)
        return(data$imm_date)
    } else {
        ## Take the closest date to end_cohort
        return(data$imm_out_date)
    }
}
