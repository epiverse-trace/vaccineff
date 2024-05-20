#' @title Match Cohort to Reduce Observational Bias
#'
#' @description This function builds couples of vaccinated and unvaccinated
#' individuals with similar characteristics. The function relies on the
#' matching algorithm implemented in the `{MatchIt}` package, setting,
#' by default, `method = "nearest"`, `ratio = 1`,
#' and `distance = "mahalanobis"`.
#' Exact and near characteristics are accepted, passed in the parameters
#' `exact` and `nearest`, respectively. The parameter `nearest` must be provided
#' together with the calipers as a named vector
#' (e.g., `nearest = c(characteristic1 = n1, characteristic2 = n2)`,
#' where `n1` and `n2` are the calipers).
#' The default matching `method` of the function is `static`. This means that
#' couples are matched once, without taking into account their vaccination,
#' censoring, and outcome dates. After this, the couples whose exposition times
#' do not coincide are removed to avoid negative time-to-events.
#' The function returns a matched and adjusted by exposition cohort, with
#' information of the beginning of follow-up period (`t0_follow_up`) of couples,
#' corresponding to the vaccination date of the vaccinated individual,
#' and the individual time-to-event that takes into account right-censoring
#' dates (`time_to_event`).
#' Couples are censored if the vaccinated or unvaccinated partner was previously
#' censored (i.e., if `censoring_date_col` is informed) and the censor occurs
#' before their outcomes.
#' Rolling calendar matching method will be included in future releases.
#'
#' @inheritParams coh_effectiveness
#' @param immunization_date Name of the column that contains the immunization
#' date to set the beginning of the follow-up period (`t0_follow_up`).
#' @param exact Name(s) of column(s) for `exact` matching. Default is `NULL`.
#' @param nearest Named vector with name(s) of column(s) for `nearest` matching
#' and caliper(s) for each variable (e.g., `nearest = c("characteristic1" = n1,
#' "characteristic2" = n2)`, where `n1` and `n2` are the calipers). Default is
#' `NULL`.
#' @return `match` object. List with results from static match:
#' `match`: data frame with adjusted cohort,
#' `summary`: matching summary,
#' `balance_all`: balance of the cohort before matching,
#' `balance_matched`: balance of the cohort after matching.
#'
#' Four columns are added to the structure provided in `data`:
#' `subclass`: ID of matched couple,
#' `t0_follow_up`: beginning of follow-up period for couple, and
#' `time_to_event`: time to event.
#' @keywords internal

match_cohort <- function(data,
                         outcome_date_col,
                         censoring_date_col,
                         immunization_date_col,
                         vacc_status_col,
                         vaccinated_status = "v",
                         unvaccinated_status = "u",
                         start_cohort,
                         end_cohort,
                         method = "static",
                         nearest = NULL,
                         exact = NULL) {
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 1
  )

  checkmate::assert_names(
    names(data),
    must.include = c(outcome_date_col, immunization_date_col, vacc_status_col)
  )

  checkmate::assert_date(
    data[[outcome_date_col]]
  )

  checkmate::assert_date(
    data[[immunization_date_col]]
  )

  checkmate::assert_character(vacc_status_col,
    any.missing = FALSE, min.len = 1
  )

  checkmate::assert_names(
    data[[vacc_status_col]],
    must.include = c(vaccinated_status, unvaccinated_status)
  )

  #Checks of censoring_date_col if provided
  if (!is.null(censoring_date_col)) {
    checkmate::assert_names(
      colnames(data),
      must.include = censoring_date_col
    )
    checkmate::assert_date(
      data[[censoring_date_col]]
    )
    checkmate::assert_string(censoring_date_col)
  }

  # `exact` and `nearest` cannot be NULL. At least one must be provided
  stopifnot(
    "`exact` and `nearest` cannot be NULL. At least one must be provided" =
      (!missing(nearest) || !missing(exact))
  )

  # checks for `nearest`
  if (!is.null(nearest)) {
    checkmate::assert_numeric(
      nearest,
      any.missing = FALSE, min.len = 1, names = "named"
    )
    checkmate::assert_names(
      names(data),
      must.include = names(nearest)
    )
  }
  # checks for `exact`. Not else, both can be non-NULL
  if (!is.null(exact)) {
    checkmate::assert_character(exact,
      any.missing = FALSE, min.len = 1
    )
    checkmate::assert_names(
      names(data),
      must.include = exact
    )
  }

  # check date types
  checkmate::assert_date(
    start_cohort, any.missing = FALSE, len = 1
  )
  checkmate::assert_date(
    end_cohort, any.missing = FALSE, len = 1
  )

  if (method == "static") {
    match_obj <- static_match(
      data = data,
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
  }
  class(match_obj) <- "match"
  return(match_obj)
}

#' @title Summarize Matching Results
#'
#' @description Summarizes the results of `match_cohort`.
#'
#' @param match An object of class `match`.
#' @return A summary of the matching results.
#' @export

summary.match <- function(match) {
  # Check if the input object is of class "match"
  if (!inherits(match, "match")) {
    stop("Input must be an object of class 'match'")
  }
  cat("Balance all:\n")
  print(match$balance_all)
  cat("\nBalance matched:\n")
  print(match$balance_match)
  cat("\nSummary:\n")
  print(match$summary)
}

#' @title Function for Extracting Matched Dataset
#'
#' @description This function extracts the matched datasets generated
#' by `match_cohort`.
#'
#' @param match An object of class `match`.
#' @return The dataset extracted from `match_cohort`.
#' @export

dataset.match <- function(match) {
  # Check if the input object is of class "match"
  if (!inherits(match, "match")) {
    stop("Input must be an object of class 'match'")
  }
  return(match$match)
}