#' @title Match cohort using mahalanobis distance
#'
#' @description This function builds couples of vaccinated - unvaccinated
#' individuals with similar characteristics. The function relies on  the
#' matching algorithm implemented in the package `{MatchIt}`.
#' By default the function uses `method = "nearest"`, `ratio = 1`,
#' `distance = "mahalanobis"` to match the data.
#' Exact and near characteristics are accepted for the matching criteria.
#' These are passed in the parameters `exact` and `nearest`, respectively.
#' The parameter `nearest` must be provided together with the calipers
#' following as a named vector.
#' (e.g. `nearest = c("characteristic1", "characteristic2"),
#' caliper = c(characteristic1 = n1, characteristic2 = n2)`,
#' where `n1` and `n2` are the calipers).
#' @inheritParams coh_effectiveness
#' @param exact name(s) of column(s) for `exact` matching.
#' Default to `NULL`.
#' @param nearest named vector with name(s) of column(s) for `nearest`
#' matching and caliper(s) for each variable.
#' e.g. `nearest = c("characteristic1" = n1, "characteristic2" = n2)`,
#' where `n1` and `n2` are the calipers. Default to `NULL`.
#' @return data frame with matched population. Two columns are added
#' to the structure provided in `data`:
#' `prop_score` (propensity score of the match),
#' `subclass` (id of matched couple)
#' @keywords internal
match_cohort <- function(data,
                         vacc_status_col,
                         exact = NULL,
                         nearest = NULL) {

  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_character(vacc_status_col,
    any.missing = FALSE, min.len = 1
  )
  checkmate::assert_names(
    names(data),
    must.include = c(vacc_status_col)
  )

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

  #Formula
  variables <- c(exact, names(nearest))
  formula <- paste0(vacc_status_col, " ~ ")
  for (v in seq_along(variables)) {
    if (v == 1) {
      formula <- paste0(formula, variables[v])
    } else {
      formula <- paste0(formula, " + ", variables[v])
    }
  }
  formula_eval <- eval(parse(text = formula))
  data[[vacc_status_col]] <- as.factor(data[[vacc_status_col]])

  #Matching
  matchit <- MatchIt::matchit(
    formula_eval,
    data = data,
    method = "nearest",
    ratio = 1,
    exact = exact,
    nearest = names(nearest),
    caliper = nearest,
    distance = "mahalanobis"
  )
  match <- MatchIt::match.data(matchit)
  match <- match[, -which(names(match) == "weights")]
  return(match)
}

#' @title Censor couple after matching
#'
#' @description This function censors a couple whether the case or the control
#' have a censoring date. It imputes the censoring date to the whole couple
#' using the matching id provided in subclass. This column comes with the output
#' of `match_cohort`.
#'
#' @inheritParams get_immunization_date
#' @examples
#' # load package example data for cohort studies
#' data("cohortdata")
#'
#' # assign vaccination status
#' cohortdata$vaccine_status <- set_status(
#'   data = cohortdata,
#'   col_names = c("vaccine_date_1", "vaccine_date_2"),
#'   status = c("v", "u")
#' )
#'
#' # match cohort
#' matched_cohort <- match_cohort(data = cohortdata,
#'   status_vacc_col = "vaccine_status",
#'   nearest = c(age = 1),
#'   exact = "sex"
#' )
#'
#' # add column with censoring date for match
#' matched_cohort$censoring_date_match <-  get_censoring_date_match(
#'   data = matched_cohort,
#'   outcome_date_col = "death_date",
#'   censoring_date_col = "death_other_causes"
#' )
#'
#' # view data with added column
#' head(matched_cohort)
#' @export
get_censoring_date_match <- function(data,
                                     outcome_date_col,
                                     censoring_date_col) {
  # check for data frame type
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  # check for names in data
  checkmate::assert_names(
    colnames(data),
    must.include = c(outcome_date_col, censoring_date_col)
  )
  # check for subclass
  checkmate::expect_names(
    colnames(data),
    must.include = "subclass",
    info = "'subclass' column from match must be included in 'data' to \
      identify matched couples."
  )

  # check for date type
  checkmate::assert_date(data[[outcome_date_col]])
  checkmate::assert_date(data[[censoring_date_col]])

  # check for string type
  checkmate::assert_string(outcome_date_col)
  checkmate::assert_string(censoring_date_col)

  # create censoring date for every couple indexed by subclass
  censoring_date <- unlist(
    tapply(data[[censoring_date_col]],
      data$subclass,
      function(x) {
        if (all(is.na(x))) {
          return(as.Date(NA))
        } else {
          return(as.character(min(x, na.rm = TRUE)))
        }
      }
    )
  )
  # return data matched by subclass
  data$censoring_date_match <- as.Date(censoring_date[data$subclass])

  # if outcome happens before censoring_date_match
  # no censoring must be assigned
  data$censoring_date_match <-
    as.Date(ifelse(
      (data$censoring_date_match > data[[outcome_date_col]]) &
        (!is.na(data$censoring_date_match)) &
        (!is.na(data[[outcome_date_col]])),
      as.Date(NA),
      as.character(data$censoring_date_match)
    ))
  return(data$censoring_date_match)
}
