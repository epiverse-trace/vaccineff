#' Function to assign the outcome status for the result of a test
#'
#' @param data dataset with at least one column to generate the binary status
#' @param col_name name of the column containing the information for the
#' status
#' @param neg_cond condition value in \code{data$colname} storing negative
#' test results
#' @param status_col binary set of status, e.g. c(1, 0). The first element of
#' the vector must be the status when the condition is satisfied.
#' @return status
#' @examples
#' data("testnegdata")
#' testnegdata$test_binary <- set_status(
#' data = testnegdata,
#' col_name = "pcr_result",
#' status = c(TRUE, FALSE))
#'
#' testnegdata$pcr_result_bin <- set_binary_test(testnegdata,
#'                                               "pcr_result",
#'                                               "Neg",
#'                                               c(FALSE, TRUE))
#' testnegdata$rapid_test_bin <- set_binary_test(testnegdata,
#'                                               "rapid_result",
#'                                               "Negative",
#'                                               c(FALSE, TRUE))
#' testnegdata$vacc_status_bin <- set_binary_test(testnegdata,
#'                                               "vacc_status",
#'                                               "No",
#'                                               c(FALSE, TRUE))
#' @export
set_binary_test <- function(data,
                            col_name,
                            neg_cond,
                            status = c(TRUE, FALSE)) {
  status_col <- ifelse(
    data[, c(col_name)] == neg_cond, status[1], status[2])
  return(status_col)
}
