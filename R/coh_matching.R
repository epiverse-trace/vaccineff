#' @title Static match of cohort
#'
#' @description This function builds couples of vaccinated - unvaccinated
#' individuals with similar characteristics. The function relies on  the
#' propensity score matching algorithm implemented in MatchIt package.
#' Thus, it allows for `exact` and `nearest` methods.
#' When `method = "exact"` the function returns couples with the same
#' characteristics for the variables passed in the parameter `exact`.
#' In this case, any inputs in the parameter `nearest` are ignored.
#' The `nearest` method can be used both with `nearest` and exact coincidences.
#' The exact characteristics must be provided as inputs in the
#' parameter `exact` and the others as inputs in `nearest`. In addition,
#' the calipers must be provided as a named vector for each of the variables
#' provided in `nearest`
#' (e.g. `nearest = c("characteristic1", "characteristic2"),
#' caliper = c("characteristic1" = n1, "characteristic2" = n2)`,
#' where `n1` and `n2` are the calipers).
#' If no input is provided to `exact` when using `method = nearest `, only
#' near matches are returned.
#'
#' @param data dataset with cohort information (see example)
#' @param status_vacc_col name of the column containing the information
#' of the vaccination status.
#' @param method matching method. `exact` and `nearest` are allowed.
#' @param exact name(s) of column(s) for `exact` matching.
#' Default to `NULL`.
#' @param nearest name(s) of column(s) for `nearest` matching.
#' Default to `NULL`.
#' @param caliper named vector with caliper(s).
#' e.g. `nearest = c("characteristic1", "characteristic2"),
#' caliper = c("characteristic1" = n1, "characteristic2" = n2)`,
#' where `n1` and `n2` are the calipers.
#' @return data frame with matched population. Two columns are added
#' to the structure provided in `data`:
#' `prop_score` (propensity score of the match),
#' `subclass` (id of matched couple)
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
#' cohort_match <- match_cohort(data = cohortdata,
#'   status_vacc_col = "vaccine_status",
#'   method = "nearest",
#'   nearest = "age",
#'   exact = "sex",
#'   caliper = c("age" = 1)
#' )
#'
#' # view data with added columns
#' head(cohortdata)
#' @export
match_cohort <- function(data,
                         status_vacc_col,
                         exact = NULL,
                         nearest = NULL,
                         caliper = NULL) {

  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_character(status_vacc_col,
    any.missing = FALSE, min.len = 1
  )
  checkmate::assert_names(
    names(data),
    must.include = c(status_vacc_col)
  )

  # `exact` and `nearest` cannot be NULL. At least one must be provided
  stopifnot(
    "`exact` and `nearest` cannot be NULL. At least one must be provided" =
      (!missing(nearest) || !missing(exact))
  )

  # checks for `nearest`
  if (!is.null(nearest)) {
    stopifnot(
      "`caliper` must be provided together with `nearest`" =
        (!missing(caliper))
    )
    checkmate::assert_character(nearest,
      any.missing = FALSE, min.len = 1
    )
    checkmate::assert_names(
      names(data),
      must.include = nearest
    )
    checkmate::assert_numeric(
      caliper,
      len = length(nearest), names = "named"
    )
    checkmate::assert_names(
      nearest,
      must.include = names(caliper)
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

  # if `nearest` is NULL and `caliper` is provided, caliper is ignored
  if (is.null(nearest) && !is.null(caliper)) {
    warning("`caliper` ignored caused by `nearest` not provided")
    #caliper forced to null to avoid unexpected behavior
    caliper <- NULL
  }

  #Formula
  variables <- c(exact, nearest)
  formula <- paste0(status_vacc_col, " ~ ")
  for (v in seq_along(variables)) {
    if (v == 1) {
      formula <- paste0(formula, variables[v])
    } else {
      formula <- paste0(formula, " + ", variables[v])
    }
  }
  formula_eval <- eval(parse(text = formula))
  data[[status_vacc_col]] <- as.factor(data[[status_vacc_col]])

  #Matching
  matchit <- MatchIt::matchit(
    formula_eval,
    data = data,
    method = "nearest",
    ratio = 1,
    exact = exact,
    nearest = nearest,
    caliper = caliper,
    distance = "glm"
  )
  match <- MatchIt::match.data(matchit, distance = "prop.score")
  names(match) <- gsub(x = names(match),
    pattern = ".",
    replacement = "_",
    fixed = TRUE
  )
  match <- subset(match, select = -weights)
  return(match)
}

censore_match <- function(data, censoring_date_col) {
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
  return(censoring_date)
}