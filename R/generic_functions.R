#' @title Generic Function for Extracting Datasets
#'
#' @description This function serves as a generic method to extract datasets
#' from objects of various classes. It dispatches to specific methods based
#' on the class of the input object.
#'
#' @param object An object from which to extract the dataset.
#' @return The dataset extracted from the object.
#' @export
dataset <- function(object) {
  UseMethod("dataset")
}
