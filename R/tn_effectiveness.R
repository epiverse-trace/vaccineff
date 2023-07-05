#' Estimate vaccine effectiveness using the test negative design
#'
#' @inheritParams coh_eff_noconf
#'
#' @return The vaccine effectiveness value
#' @export
#'
#' @examples
#' # Load the dataset for this example
#' data(testnegdata)
#' testnegdata$pcr_result_bin <- set_binary_test(testnegdata,
#'                                               "pcr_result",
#'                                               "Neg",
#'                                               c(FALSE, TRUE))
#' testnegdata$vacc_status_bin <- set_binary_test(testnegdata,
#'                                               "vacc_status",
#'                                               "No",
#'                                               c(FALSE, TRUE))
#' estimate_tn_eff(testnegdata, "vacc_status_bin", "pcr_result_bin")
estimate_tn_eff <- function(data, status_vacc_col, outcome_status_col) {
  # Check that column names are characters
  checkmate::assert_string(outcome_status_col)
  checkmate::assert_string(status_vacc_col)

  stopifnot("outcome_status_col must be a column in dataset" =
              outcome_status_col %in% names(data),
            "status_vacc_col must be a column in dataset" =
              status_vacc_col %in% names(data),
            "status_vacc_col must be boolean (TRUE OR FALSE)" =
              checkmate::assert_logical(data[[status_vacc_col]]),
            "outcome_status_col must be boolean (TRUE OR FALSE)" =
              checkmate::assert_logical(data[[outcome_status_col]])
            )

  # Generate a 2x2 grid for vaccination status and test positivity
  status_grid <- table(data[[status_vacc_col]], data[[outcome_status_col]],)

  # Calculate odds of being vaccinated as a case (positive for infection)
  odds_case_vaxed <- status_grid[1, 1] / status_grid[2, 1]

  # Calculate odds of being vaccinated as a control (negative for infection)
  odds_noncase_vaxed <- status_grid[1, 2] / status_grid[2, 2]

  # Calculate the test negative vaccine effectiveness according to the formula
  # 1 - (odds of being vaccinated as a case / odds of being unvaccinated as a
  # case)
  tn_eff <- (1 - (odds_case_vaxed / odds_noncase_vaxed)) * 100

  return(tn_eff)
}
