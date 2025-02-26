#' Set up gluey preprocessing for R Markdown and Quarto documents
#'
#' @description
#' This function registers gluey_knit as the document processor.
#' It should be called in .onLoad to ensure gluey is properly initialized.
#'
#' @param enabled Whether to enable gluey preprocessing (default: TRUE)
#' @export
setup_gluey_preprocessing <- function(enabled = TRUE) {
  if (!enabled) {
    # Disable the preprocessing
    options(gluey.enabled = FALSE)
    return(invisible())
  }

  # Enable the preprocessing
  options(gluey.enabled = TRUE)

  # Register knit engines if needed
  if (requireNamespace("knitr", quietly = TRUE)) {
    # Register gluemd engine
    knitr::knit_engines$set(gluemd = function(options) {
      # Process the content with gluey
      out <- gluey(options$code, .envir = knitr::knit_global())

      # Return processed content
      knitr::asis_output(out)
    })
  }

  message("gluey preprocessing enabled")
}

#' Check if the document has gluey processing enabled via YAML header
#'
#' @param yaml_data YAML metadata as a list
#' @return Logical indicating whether gluey is enabled
#' @noRd
is_gluey_enabled <- function(yaml_data) {
  # Check for knit: gluey::gluey_knit or knit: gluey::gluey_render
  if (!is.null(yaml_data$knit)) {
    knit_func <- yaml_data$knit
    if (grepl("gluey::gluey_", knit_func)) {
      return(TRUE)
    }
  }

  # Check for params.gluey.enabled
  if (!is.null(yaml_data$params) && !is.null(yaml_data$params$gluey.enabled)) {
    return(isTRUE(yaml_data$params$gluey.enabled))
  }

  # Default to the global option
  return(getOption("gluey.enabled", FALSE))
}
