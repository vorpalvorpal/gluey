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
      out <- gluey(options$code, .envir = knit_global())

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

#' Extract YAML header from a document
#'
#' @param text Document text
#' @return List representation of YAML header
#' @noRd
parse_yaml_header <- function(text) {
  # Check for YAML header between --- markers
  if (grepl("^---\\s*$", text, multiline = TRUE)) {
    yaml_lines <- grep("^---\\s*$", strsplit(text, "\n")[[1]])

    if (length(yaml_lines) >= 2) {
      yaml_text <- paste(
        strsplit(text, "\n")[[1]][(yaml_lines[1] + 1):(yaml_lines[2] - 1)],
        collapse = "\n"
      )

      if (requireNamespace("yaml", quietly = TRUE)) {
        tryCatch(
          {
            return(yaml::yaml.load(yaml_text))
          },
          error = function(e) {
            warning("Failed to parse YAML header: ", e$message)
            return(list())
          })
      }
    }
  }

  return(list())
}

#' Process gluey syntax inside inline R code chunks
#'
#' @param options Chunk options
#' @return Processed text
#' @noRd
process_inline_gluey <- function(options) {
  # Extract the code
  code <- options$code

  # Check if it's a gluey call
  if (grepl("^gluey\\(", code)) {
    # Already using gluey, so no need to modify
    return(options)
  }

  # Wrap with gluey if it's not already a gluey call
  options$code <- paste0("gluey(\"", gsub("\"", "\\\\\"", code), "\")")

  return(options)
}
