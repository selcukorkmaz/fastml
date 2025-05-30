#' Clean Column Names or Character Vectors by Removing Special Characters
#'
#' This function can operate on either a data frame or a character vector:
#' \itemize{
#'   \item \strong{Data frame}: Detects columns whose names contain any character
#'         that is not a letter, number, or underscore,
#'         removes colons, replaces slashes with underscores, and spaces
#'         with underscores.
#'   \item \strong{Character vector}: Applies the same cleaning rules to every
#'         element of the vector.
#' }
#'
#' @param x A data frame or character vector to be cleaned.
#'
#' @return
#' \itemize{
#'   \item If \code{x} is a data frame: returns a data frame with cleaned column names.
#'   \item If \code{x} is a character vector: returns a character vector with cleaned elements.
#' }
#'
#' @importFrom dplyr rename_with all_of
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_detect
#' @export
sanitize <- function(x) {

  # Helper function to detect special characters in a name
  has_special_chars <- function(name) {
    str_detect(name, "[^a-zA-Z0-9_]")
  }

  # Helper function that cleans a vector of names
  clean_names_vector <- function(name_vec) {
    # Use make_clean_names() with a custom 'replace' to handle mu, colons, slashes, and spaces
    sapply(name_vec, function(n) {
      make_clean_names(
        n,
        replace = c(
          ":"     = "",    # Remove colons
          "/"     = "_",   # Replace slashes with underscores
          " "     = "_"    # Replace spaces with underscores
        )
      )
    }, USE.NAMES = FALSE)
  }

  # If x is a data frame, rename its columns that contain special characters
  if (is.data.frame(x)) {
    # Identify columns needing cleaning
    columns_with_special_chars <- names(x)[has_special_chars(names(x))]

    if(length(columns_with_special_chars) > 0){
      x <- rename_with(
        x,
        ~ clean_names_vector(.),
        .cols = all_of(columns_with_special_chars)
      )
    }
    return(x)

    # If x is a character vector, simply clean each element
  } else if (is.character(x)) {
    return(clean_names_vector(x))

    # Otherwise, throw an error
  } else {
    stop("Input must be a data frame or a character vector.")
  }
}
