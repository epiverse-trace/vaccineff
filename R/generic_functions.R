#' @title Generic Function for Extracting Datasets
#'
#' @description This function serves as a generic method to extract datasets
#' from objects of various classes. It dispatches to specific methods based
#' on the class of the input object.
#'
#' @param object An object from which to extract the dataset.
#' @param ... Additional arguments passed to other functions.
#' @return The get_dataset extracted from the object.
#' @keywords internal
get_dataset <- function(object, ...) {
  UseMethod("get_dataset")
}
