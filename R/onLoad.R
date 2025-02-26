#' @importFrom utils installed.packages
.onLoad <- function(libname, pkgname) {
  # Set up default options
  op <- options()
  op.gluey <- list(
    gluey.enabled = TRUE
  )

  toset <- !(names(op.gluey) %in% names(op))
  if (any(toset)) options(op.gluey[toset])

  # Set up preprocessing by default
  setup_gluey_preprocessing()

  invisible()
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  invisible()
}
