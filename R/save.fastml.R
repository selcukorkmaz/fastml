#' Save Model Function
#'
#' Saves the trained model object to a file.
#'
#' @param model An object of class \code{fastml}.
#' @param filepath A string specifying the file path to save the model.
#' @return No return value, called for its side effect of saving the model object to a file.
#' @export
save_fastml <- function(model, filepath) {
  saveRDS(model, filepath)
}

#' Save Model Function (Deprecated)
#'
#' \code{save.fastml} is deprecated in favour of \code{\link{save_fastml}}.
#' The old name resembled an S3 method for \code{base::save()}, which is not
#' a generic, leading to dispatch confusion.
#'
#' @inheritParams save_fastml
#' @return No return value, called for its side effect of saving the model object to a file.
#' @export
save.fastml <- function(model, filepath) {
  .Deprecated("save_fastml")
  save_fastml(model, filepath)
}


#' Load Model Function
#'
#' Loads a trained model object from a file.
#'
#' @param filepath A string specifying the file path to load the model from.
#' @return An object of class \code{fastml}.
#' @export
load_model <- function(filepath) {
  model <- readRDS(filepath)
  return(model)
}
