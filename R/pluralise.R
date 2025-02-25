#' Helper function for pluralisation
#'
#' This function provides support for the pluralisation syntax in gluey.
#' It is used internally by the preprocessing system but can also be
#' called directly.
#'
#' @param x The value to determine pluralisation (numeric or vector)
#' @param zero Optional text to use for zero quantity
#' @param singular Optional text to use for singular form (defaults to empty string)
#' @param plural Required text to use for plural form
#'
#' @return A character string with the appropriate pluralisation
#' @export
#'
#' @examples
#' # Simple suffix
#' pluralise(1, plural = "s")          # ""
#' pluralise(2, plural = "s")          # "s"
#' pluralise(c("a", "b"), plural = "s")  # "s"
#'
#' # Singular/plural forms
#' pluralise(1, singular = "y", plural = "ies")      # "y"
#' pluralise(2, singular = "y", plural = "ies")      # "ies"
#'
#' # With zero form
#' pluralise(0, zero = "no", singular = "one", plural = "many")  # "no"
pluralise <- function(x, zero = NULL, singular = NULL, plural) {
  qty <- make_quantity(x)

  # Default singular to empty string if not provided
  if (is.null(singular)) singular <- ""

  if (!is.null(zero) && is.finite(qty) && qty == 0) {
    return(zero)
  } else if (is.finite(qty) && qty == 1) {
    return(singular)
  } else {
    return(plural)
  }
}

#' Determine the quantity for pluralisation
#'
#' @param x Object to get quantity from
#' @return An integer quantity
#' @noRd
make_quantity <- function(x) {
  if (is.numeric(x)) {
    if (length(x) == 1 && is.finite(x)) {
      as.integer(x)
    } else {
      length(x)
    }
  } else {
    length(x)
  }
}
