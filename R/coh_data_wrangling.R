#' Function to assign the outcome status or the vaccination status
#'
#' This function generates a binary status column using the set of columns,
#' passed through the
#' variable {col_names}. This columns must contain the information of
#' the outcomes or the vaccine dates.
#' It generates a logical condition using the operators "&" or "|", and
#' evaluates if the registers in
#' the columns contain or not information.
#' If the logical operator is "&", the function returns 1, only if all the
#' columns contain information.
#' On the other hand, if the logical operator is "|", it is enough to find one
#' column with information.
#' It is recommended to use this method when working with several outcomes or
#' several vaccine doses.
#' By default, it returns a binary column where 0 means no outcome or no
#' vaccine and 1 means the opposite.
#' However, it can also receive personalized options, e.g. c("v", "u") for
#' vaccinated and unvaccinated.
#'
#' @param data dataset with at least one column to generate the status
#' @param col_names name of the column containing the information for the
#' status
#' @param operator logical operator to evaluate the condition
#' @param status binary set of status, e.g. c("v","u"). The first element of
#' the vector must be the status when the condition is satisfied.
#' @return status
#' @examples
#' \dontrun{
#' data(cohortdata)
#' cohortdata$vaccine.status <- set_status(cohortdata,
#'                                          c("vaccine_date_1",
#'                                            "vaccine_date_2"),
#'                                          status = c("v", "u"))
#' }
#' @export
set_status <- function(data,
                       col_names,
                       operator = "&",
                       status = c(1, 0)) {
  condition <- "ifelse("
  int0 <- "(!is.na(data[["
  intf <- "]]))"
  i <- 1
  for (col_name in col_names) {
    if (i == length(col_names)) {
      sep <- ""
    } else {
      sep <- operator
    }
    condition <- paste0(condition, int0, "'", col_name, "'", intf, sep)
    i <- i + 1
  }
  if (is.numeric(status)) {
    lst <- paste0(",", status[1], ",", status[2], ")")
  } else {
    lst <- paste0(",", "'", status[1], "'", ",", "'", status[2], "'", ")")
  }
  condition <- paste0(condition, lst)
  status_col <- eval(parse(text = condition))
  return(status_col)
}

#' Function to construct the inmunization date
#'
#' This function returns a column with the immunization date that corresponds
#' to an analysed outcome.
#' If a register presents an outcome, the function search for the closest
#' vaccine date before the outcome that satisfies the condition:
#' vacc_date_col <= outcome_date_col - delay_time - immunization_delay.
#' This condition allows to discriminate the vaccine dates in
#' terms of characteristic times in days (delay_time)  associated to an
#' outcome, from the onset of symptoms or from any reference event, and the
#' characteristic time in days before the patient is considered
#' immune (immunization_delay).
#' Both parameters can be set to zero by the user without
#' affecting the results.
#' If a register does not present an outcome, the immunization
#' date can be construct using the closest vaccine
#' date to the end of the study (take_first = FALSE), or the
#' first vaccination date found (take_first = TRUE).
#' Notice that the function works for one or several vaccines.
#' In case of several vaccines, the parameter
#' {outcome_date_col must} be passed as a vector (see example)
#'
#' @param data dataset with cohort information (see example)
#' @param outcome_date_col name of the column that contains
#' the outcome dates
#' @param outcome_delay characteristic time in days of the outcome
#' from the reference event
#' @param immunization_delay characteristic time in days before the patient
#' is considered immune
#' @param vacc_date_col name of the column(s) that contains the vaccine dates
#' @param end_cohort end date of the study
#' @param take_first TRUE: takes the minimum vaccine date for
#' registers without outcome.
#' FALSE: takes closest to end_cohort
#' @return immunzation date
#' @examples
#' \dontrun{
#' data(cohortdata)
#' cohortdata$immunization.death <- get_immunization_date(cohortdata,
#' "death_date", 1, 1,
#' c("vaccine_date_1", "vaccine_date_2"),
#' "2021-12-31",
#' take_first = FALSE)
#' }
#' @export
get_immunization_date <- function(data,
                                  outcome_date_col,
                                  outcome_delay,
                                  immunization_delay,
                                  vacc_date_col,
                                  end_cohort,
                                  take_first = TRUE) {
  end_cohort <- as.Date(end_cohort)
  data$outcome_col <- set_status(data,
                                 outcome_date_col,
                                 operator = "&",
                                 status = c(1, 0))
  data$imm_limit <- data.table::fifelse(data$outcome_col == 1,
                                        as.Date(data[[outcome_date_col]]) -
                                          as.difftime(outcome_delay,
                                                      units = "days") -
                                          as.difftime(immunization_delay,
                                                      units = "days"),
                                        end_cohort)

  deltas <- NULL
  for (i in seq_along(vacc_date_col)) {
    data[[paste0("delta", i)]] <-
      data$imm_limit - as.Date(data[[vacc_date_col[i]]])
    deltas <- c(deltas, paste0("delta", i))
  }
  n_deltas <- length(deltas) - 1
  data[, (ncol(data) - n_deltas):ncol(data)][
                                             data[,
                                                  (ncol(data) -
                                                     n_deltas):ncol(data)]
                                             < 0] <- NA
  data <- data %>%
    dplyr::mutate(delta_imm = pmin(!!!rlang::syms(deltas), na.rm = TRUE))
  data$imm_out_date <- data$imm_limit - data$delta_imm + immunization_delay
  if (take_first) {
    ## Take the minimum immunization date
    data <- data %>%
      dplyr::mutate(min_imm =
                      pmin(!!!rlang::syms(vacc_date_col),
                           na.rm = TRUE))
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

#' Function to construct the time-to-event
#'
#' This function returns a column with the time-to-event in days occurred
#' until a reference outcome. The starting point to count the time-to-event
#' can be the immunization date, supossing that the vaccinate population
#' enters to the study, when they are vaccinated
#' (start_from_immunization=TRUE). Or the beginning of the study, if all the
#' cohort is known at this point (start_from_immunization=FALSE). In this last
#' case, it is not necessary to pass the argument {immunization_date_col}
#'
#' @param data dataset with cohort information (see example)
#' @param outcome_date_col name of the column that contains
#' the outcome dates
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
#' @param start_from_immunization TRUE: starts counting time-to-event from
#' immunization date if available
#' FALSE: starts counting time-to-event from the start date of the cohort study
#' @param immunization_date_col name of the column that contains the 
#' immunization date. Required if start_from_immunization = TRUE
#' @return time-to-event
#' @examples
#' \dontrun{
#' data(cohortdata)
#' cohortdata$immunization_death <-
#'   get_immunization_date(cohortdata,
#'                        "death_date",
#'                        1,
#'                        1,
#'                        c("vaccine_date_1", "vaccine_date_2"),
#'                        "2021-12-31",
#'                        take_first = FALSE)
#' cohortdata$time_to_death <- get_time_to_event(cohortdata, "death_date",
#'                                             "2021-01-01", "2021-12-31",
#'                                             TRUE, "immunization_death")
#' }
#' @export
get_time_to_event <- function(data, outcome_date_col,
                              start_cohort, end_cohort,
                              start_from_immunization = FALSE,
                              immunization_date_col = FALSE) {
  start_cohort <- as.Date(start_cohort)
  end_cohort <- as.Date(end_cohort)
  if (start_from_immunization) {
    if (!isFALSE(immunization_date_col)) {
      data[[outcome_date_col]] <- as.Date(data[[outcome_date_col]])
      data[[immunization_date_col]] <- as.Date(data[[immunization_date_col]])
      data$time_to_event <-  data[[outcome_date_col]] -
        data[[immunization_date_col]]
      data$time_to_event <- ifelse((is.na(data$time_to_event)) &
                                     (is.na(data[[immunization_date_col]])),
                                   data[[outcome_date_col]] - start_cohort,
                                   data$time_to_event)
      data$time_to_event <- ifelse((is.na(data$time_to_event)) &
                                     (is.na(data[[immunization_date_col]])) &
                                     (is.na(data[[outcome_date_col]])),
                                   end_cohort - start_cohort,
                                   data$time_to_event)
      data$time_to_event <- ifelse((is.na(data$time_to_event)) &
                                     (is.na(data[[outcome_date_col]])),
                                   end_cohort - data[[immunization_date_col]],
                                   data$time_to_event)
      return(data$time_to_event)
    } else {
      stop("Variable immunization_date_col must be
           introduce if start_from_immunization=TRUE")
    }
  } else {
    data[[outcome_date_col]] <- as.Date(data[[outcome_date_col]])
    data$time_to_event <- data[[outcome_date_col]] - start_cohort
    data$time_to_event <- ifelse((is.na(data$time_to_event)) &
                                   (is.na(data[[outcome_date_col]])),
                                 end_cohort - start_cohort,
                                 data$time_to_event)
    return(data$time_to_event)
  }
}

#' Function to construct dose associated to the immunization date
#'
#' This function returns the names of the columns associated
#' to the immunization date.
#' To avoid mistakes, it is necessary to set the same value of
#' immunization_delay that was used in the previous functions.
#'
#' @param data dataset with cohort information (see example)
#' @param immunization_date_col name of the column that contains the
#' immunization date.
#' @param vacc_date_col name of the column(s) that contains the vaccine date
#' @param immunization_delay characteristic time in days before the patient
#' is considered immune
#' @return dose: a column with the names of the columns that are associated to
#' the doses of each register
#' @examples
#' \dontrun{
#' data(cohortdata)
#' cohortdata$immunization.death
#'     <- get_immunization_date(cohortdata,
#'                              "death_date",
#'                              1,
#'                              1,
#'                              c("vaccine_date_1", "vaccine_date_2"),
#'                              "2021-12-31",
#'                              take_first = FALSE)
#' cohortdata$immunization_dose <-
#'       get_immunization_dose(cohortdata,
#'                             "immunization_death",
#'                             c("vaccine_date_1", "vaccine_date_2"),
#'                             immunization_delay = 14)
#' }
#' @export
get_immunization_dose <- function(data,
                                  immunization_date_col,
                                  vacc_date_col,
                                  immunization_delay) {
  data$id <- seq_len(nrow(data))
  cols <- c("id", immunization_date_col, vacc_date_col)
  split <- data[!is.na(data[[immunization_date_col]])] %>%
    dplyr::select(dplyr::all_of(cols))
  long <- data.table::melt(data.table::setDT(split),
                           id.vars = c("id", immunization_date_col),
                           variable.name = "dose",
                           value.name = "vaccine_date")
  long[[immunization_date_col]] <- as.Date(long[[immunization_date_col]])
  long$vaccine_date <- as.Date(long$vaccine_date)

  long <- long[(long[[immunization_date_col]] - immunization_delay
                == long$vaccine_date)
               & !is.na(long$vaccine_date)]
  long <- long[order(long$id, long$dose), ]
  long <- long[!duplicated(long$id), ]
  long <- long %>% dplyr::select(dplyr::all_of(c("id", "dose")))
  data <- merge(x = data, y = long, by = "id", all.x = TRUE)
  data <- data[order(data$id), ]
  return(data$dose)
}

#' Function to construct vaccine biologic associated to the immunization date
#'
#' This function returns the names of the vaccines
#' associated to the immunization date.
#' To avoid mistakes, it is necessary to set the same
#' value of immunization_delay that was used in the previous
#' functions.
#' The arguments vacc_date_col and vacc_name_col must be passed in the same
#' order, i.e. every name of column date must correspond
#' to a name of vaccine column (see example)
#'
#' @param data dataset with cohort information (see example)
#' @param immunization_date_col name of the column that contains the
#' immunization date.
#' @param vacc_date_col name of the column(s) that contains the vaccine date
#' @param vacc_name_col name of the column(s) that contains the vaccine names
#' @param immunization_delay characteristic time in days before the patient
#' is considered immune
#' @return dose: a column with the names of the columns that are associated to
#' the doses of each register
#' @return vaccine name
#' @examples
#' \dontrun{
#' data(cohortdata)
#' cohortdata$immunization_death
#'   <- get_immunization_date(cohortdata,
#'                            "death_date",
#'                            1,
#'                            1,
#'                            c("vaccine_date_1","vaccine_date_2"),
#'                            "2021-12-31",
#'                            take_first = FALSE)
#' cohortdata$immunization.vaccine
#'   <- get_immunization_vaccine(cohortdata,
#'                               "immunization_death",
#'                               c("vaccine_date_1", "vaccine_date_2"),
#'                               c("vaccine_1", "vaccine_2"),
#'                               immunization_delay = 14)
#' }
#' @export
get_immunization_vaccine <- function(data,
                                     immunization_date_col,
                                     vacc_date_col,
                                     vacc_name_col,
                                     immunization_delay) {
  rdf <- data.frame("vaccine_name_col" = vacc_name_col,
                    "vaccine_date_col" = vacc_date_col)
  data$id <- seq_len(nrow(data))
  cols1 <- c("id", immunization_date_col, vacc_date_col)
  split1 <- data[!is.na(data[[immunization_date_col]])] %>%
    dplyr::select(dplyr::all_of(cols1))
  long1 <- data.table::melt(data.table::setDT(split1),
                            id.vars = c("id", immunization_date_col),
                            variable.name = "vaccine_date_col",
                            value.name = "vaccine_date")
  long1[[immunization_date_col]] <- as.Date(long1[[immunization_date_col]])
  long1$vaccine_date <- as.Date(long1$vaccine_date)
  long1 <- long1[(long1[[immunization_date_col]] - immunization_delay
                  == long1$vaccine_date)
                 & !is.na(long1$vaccine_date)]
  long1 <- long1[order(long1$id, long1$vaccine_date_col), ]
  long1 <- long1[!duplicated(long1$id), ]

  cols2 <- c("id", immunization_date_col, vacc_date_col, vacc_name_col)
  split2 <- data[!is.na(data[[immunization_date_col]])] %>%
    dplyr::select(dplyr::all_of(cols2))
  long2 <-
    data.table::melt(data.table::setDT(split2),
                     id.vars = c("id",
                                 immunization_date_col,
                                 vacc_date_col),
                     variable.name = "vaccine_name_col",
                     value.name = "vaccine_name")
  long3 <- data.table::melt(data.table::setDT(long2),
                            id.vars = c("id", immunization_date_col,
                                        "vaccine_name_col",
                                        "vaccine_name"),
                            variable.name = "vaccine_date_col",
                            value.name = "vaccine_date")
  long3 <- long3[(long3[[immunization_date_col]] - immunization_delay
                  == long3$vaccine_date)
                 & !is.na(long3$vaccine_date)]
  long3 <- merge(x = long3, y = rdf, by = "vaccine_name_col", all.x = TRUE)
  long3 <- long3[(long3$vaccine_date_col.x == long3$vaccine_date_col.y)]
  long3 <- long3 %>% dplyr::select(dplyr::all_of(c("id",
                                                   "vaccine_name",
                                                   "vaccine_date_col.x")))
  colnames(long3) <- c("id", "vaccine", "vaccine_date_col")

  long1 <- merge(x = long1, y = long3,
                 by = c("id", "vaccine_date_col"),
                 all.x = TRUE)
  long1 <- long1 %>% dplyr::select(dplyr::all_of(c("id", "vaccine")))
  data <- merge(x = data, y = long1, by = "id", all.x = TRUE)
  data <- data[order(data$id), ]
  return(data$vaccine)
}