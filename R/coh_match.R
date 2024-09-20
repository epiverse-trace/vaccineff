#' @title Match Cohort to Reduce Observational Bias
#'
#' @description This function builds pairs of vaccinated and unvaccinated
#' individuals with similar characteristics. The function relies on the matching
#' algorithm implemented in the package `{MatchIt}`, setting, by default,
#' `method = "nearest"`, `ratio = 1`, and `distance = "mahalanobis"`.
#' Exact and near characteristics are accepted, passed in the parameters `exact`
#' and `nearest`, respectively. The parameter `nearest` must be provided
#' together with the calipers as a named vector (e.g.,
#' `nearest = c(characteristic1 = n1, characteristic2 = n2)`, where `n1`
#' and `n2` are the calipers). The default matching `method` of the function is
#' `static`. This means that pairs are matched once, without taking into
#' account their vaccination, censoring, and outcome dates. After this, the
#' pairs whose exposition time do not coincide are removed to avoid negative
#' time-to-events.
#' The function returns a matched and adjusted by exposition cohort, with
#' information of the beginning of follow-up period of pairs (`t0_follow_up`),
#' corresponding to the vaccination date of the vaccinated individual, the
#' individual time-to-event (`time_to_event`) and the outcome status
#' (`outcome_status`), both taking into account the right-censoring dates.
#' Pairs are censored if the vaccinated or unvaccinated partner was previously
#' censored (i.e., if `censoring_date_col` is informed) and the censor occurs
#' before their outcomes. Rolling calendar matching method will be included in
#' future releases.
#'
#' @param data_set `data.frame` with cohort information (see example).
#' @param outcome_date_col Name of the column that contains the outcome dates.
#' @param censoring_date_col Name of the column that contains the censoring
#' date. `NULL` by default.
#' @param start_cohort Start date of the study.
#' @param end_cohort End date of the study.
#' @param method Method to match the cohort. Default is `static`.
#' @param exact Name(s) of column(s) for `exact` matching. Default is `NULL`.
#' @param nearest Named vector with name(s) of column(s) for `nearest` matching
#' and caliper(s) for each variable (e.g., `nearest = c("characteristic1" = n1,
#' "characteristic2" = n2)`, where `n1` and `n2` are the calipers). Default is
#' `NULL`.
#' @param immunization_date_col Name of the column that contains the
#' immunization date to set the beginning of the follow-up period
#' (`t0_follow_up`). Default is `immunization_date`.
#' @param vacc_status_col Name of the column containing the vaccination.
#' Default is `vaccine_status`.
#' @param vaccinated_status Status assigned to the vaccinated population.
#' Default is `v`.
#' @param unvaccinated_status Status assigned to the unvaccinated population.
#' Default is `u`.
#' @return object of the class `match`. List with results from static match:
#' `match`: `data.frame` with adjusted cohort,
#' `summary`: matching summary,
#' `balance_all`: balance of the cohort before matching,
#' `balance_matched`: balance of the cohort after matching.
#'
#' Four columns are added to the structure provided in `data_set`:
#' `subclass`: ID of, matched pair,
#' `t0_follow_up`: beginning of follow-up period for pair,
#' `time_to_event`: time to event,
#' and `outcome_status`: outcome status
#' (1:positive, 0: negative).
#' @keywords internal

match_cohort <- function(data_set,
                         outcome_date_col,
                         censoring_date_col,
                         start_cohort,
                         end_cohort,
                         method = "static",
                         nearest = NULL,
                         exact = NULL,
                         immunization_date_col = "immunization_date",
                         vacc_status_col = "vaccine_status",
                         vaccinated_status = "v",
                         unvaccinated_status = "u") {
  # input checking
  checkmate::assert_data_frame(
    data_set,
    min.rows = 1, min.cols = 1
  )

  checkmate::assert_names(
    names(data_set),
    must.include = c(outcome_date_col, immunization_date_col, vacc_status_col)
  )

  checkmate::assert_date(
    data_set[[outcome_date_col]]
  )

  checkmate::assert_date(
    data_set[[immunization_date_col]]
  )

  checkmate::assert_character(vacc_status_col,
    any.missing = FALSE, min.len = 1
  )

  checkmate::assert_names(
    data_set[[vacc_status_col]],
    must.include = c(vaccinated_status, unvaccinated_status)
  )

  #Checks of censoring_date_col if provided
  checkmate::assert_string(censoring_date_col, null.ok = TRUE)
  checkmate::assert_names(
    colnames(data_set),
    must.include = censoring_date_col
  )
  if (!is.null(censoring_date_col)) {
    checkmate::assert_date(
      data_set[[censoring_date_col]]
    )
  }

  # `exact` and `nearest` cannot be NULL. At least one must be provided
  stopifnot(
    "`exact` and `nearest` cannot be NULL. At least one must be provided" =
      (!missing(nearest) || !missing(exact))
  )

  # checks for `nearest`
  checkmate::assert_numeric(
    nearest,
    any.missing = FALSE, min.len = 1, names = "named", null.ok = TRUE
  )
  checkmate::assert_names(
    names(data_set),
    must.include = names(nearest)
  )

  # checks for `exact`
  checkmate::assert_character(exact,
    any.missing = FALSE, min.len = 1, null.ok = TRUE
  )
  checkmate::assert_names(
    names(data_set),
    must.include = exact
  )

  # check date types
  checkmate::assert_date(
    start_cohort, any.missing = FALSE, len = 1
  )
  checkmate::assert_date(
    end_cohort, any.missing = FALSE, len = 1
  )

  # check method
  checkmate::assert_choice(
    method, choices = "static"
  )

  if (method == "static") {
    match <- static_match(
      data_set = data_set,
      outcome_date_col = outcome_date_col,
      censoring_date_col = censoring_date_col,
      immunization_date_col = immunization_date_col,
      vacc_status_col = vacc_status_col,
      vaccinated_status = vaccinated_status,
      unvaccinated_status = unvaccinated_status,
      start_cohort = start_cohort,
      end_cohort = end_cohort,
      nearest = nearest,
      exact = exact
    )
    match_obj <- match
  }
  class(match_obj) <- "match"
  return(match_obj)
}

#' @title Summarize Matching Results
#'
#' @description Summarizes the results of `match_cohort`.
#'
#' @param object Object of the class `match`.
#' @param ... Additional arguments passed to other functions.
#' @return Summary of the results from matching.
#' @export

summary.match <- function(object, ...) {
  # Check if the input object is of class "match"
  stopifnot("Input must be an object of class 'match'" =
      checkmate::test_class(object, "match")
  )
  cat(paste0("Number of iterations: ", object$iterations, "\n"))
  cat("\nBalance all:\n")
  print(object$balance_all)
  cat("\nBalance matched:\n")
  print(object$balance_match)
  cat("\nSummary:\n")
  print(object$summary)
}

#' @title Function for Extracting Matched Dataset
#'
#' @description This function extracts the matched datasets generated
#' by `match_cohort`.
#'
#' @param object Object of the class `match`.
#' @param ... Additional arguments passed to other functions.
#' @return `data.frame` extracted from `match_cohort`.
#' @keywords internal

get_dataset.match <- function(object, ...) {
  # Check if the input object is of class "match"
  stopifnot("Input must be an object of class 'match'" =
      checkmate::test_class(object, "match")
  )
  return(object$match)
}
