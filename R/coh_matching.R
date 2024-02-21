match <- function(data,
                  status_vacc_col,
                  method = c("exact", "nearest"),
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

  # check method
  method <- match.arg(method, several.ok = FALSE)
  checkmate::assert_string(
    method
  )

  # if nearest, nearest and caliper must be provided
  if (method == "nearest") {
    stopifnot(
      "`nearest` and `caliper` must be provided for `nearest` method" =
        (!missing(nearest) && !missing(caliper))
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

    if (!is.null(exact)) {
      checkmate::assert_character(exact,
        any.missing = FALSE, min.len = 1
      )
      checkmate::assert_names(
        names(data),
        must.include = nearest
      )
      checkmate::assert_names(
        exact,
        disjunct.from = nearest
      )
    }
  } else {
    stopifnot(
      "`exact` must be provided for `exact` method" =
        !missing(exact)
    )
    checkmate::assert_character(exact,
      any.missing = FALSE, min.len = 1
    )
    checkmate::assert_names(
      names(data),
      must.include = exact
    )
    if (!is.null(nearest) || !is.null(caliper)) {
      warning("`nearest` and `caliper` are ignored in `exact` method")
    }
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
  return(match)
}
