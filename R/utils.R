#' Get or create the gluey state environment
#'
#' Retrieves the state environment for pluralization from knitr's global
#' environment, or creates a new one if it doesn't exist or explicitly requested.
#'
#' @param new_env Whether to create a fresh environment
#' @return Environment containing pluralization state
#' @noRd
get_gluey_state <- function(new_env = FALSE) {
  # Get the knitr global environment
  knit_env <- knitr::knit_global()

  if (!exists("gluey_state", envir = knit_env) || new_env) {
    # Initialize fresh state
    knit_env$gluey_state <- new.env(parent = emptyenv())
    knit_env$gluey_state$empty <- uuid::UUIDgenerate()
    knit_env$gluey_state$qty <- knit_env$gluey_state$empty
    knit_env$gluey_state$num_subst <- 0L
    knit_env$gluey_state$postprocess <- FALSE
    knit_env$gluey_state$pmarkers <- list()
  }

  return(knit_env$gluey_state)
}

#' Clean up the gluey state environment
#'
#' Removes the state environment from knitr's global environment.
#'
#' @return Invisible NULL
#' @noRd
cleanup_gluey_state <- function() {
  tryCatch(
    {
      rm("gluey_state", envir = knitr::knit_global())
    },
    error = function(e) {
      # Silently handle the case where it doesn't exist
    })

  invisible(NULL)
}

#' Check if a package is installed and load it if needed
#'
#' @param package Package name
#' @param message Message to display if package is missing
#' @param load Whether to load the package or just check
#'
#' @return TRUE if package is available, FALSE otherwise
#' @noRd
check_package <- function(package, message = NULL, load = FALSE) {
  if (is.null(message)) {
    message <- paste0(
      "Package '", package, "' is required but not installed.\n",
      "Please install it with: install.packages('", package, "')"
    )
  }

  if (requireNamespace(package, quietly = TRUE)) {
    if (load) {
      tryCatch(
        {
          eval(parse(text = paste0("library(", package, ", quietly = TRUE)")))
        },
        error = function(e) {
          warning("Could not load package '", package, "': ", e$message)
          return(FALSE)
        })
    }
    return(TRUE)
  } else {
    warning(message)
    return(FALSE)
  }
}

#' Detect if a document is a Quarto document
#'
#' @param text Document text
#' @param default Default value to return if all detection strategies fail
#' @return Logical indicating if the document is a Quarto document
#' @noRd
detect_quarto_document <- function(text, default = FALSE) {
  # Parse YAML
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (!grepl("^---$", lines[1])) {
    cli::cli_abort("Rmd/Qmd file parsed needs to have a yaml header",
      " starting from line 1 with: ---")
  }
  yaml_end <- which(grepl("^---", lines))[2]
  yaml_content <- yaml::yaml.load(lines[1:yaml_end])


  # Look for Quarto-specific YAML elements
  if (length(intersect(c("format", "quarto-required", "shortcodes"),
    names(yaml_content)) >= 1)) return(TRUE)

  # Look for Rmarkdown-specific YAML elements
  # if (???) return(FALSE)

  # If using rstudioapi, try getting editor context
  if (grepl("\\.qmd$", rstudioapi::getSourceEditorContext()$path, ignore.case = TRUE)) {
    return(TRUE)
  } else if (grepl("\\.rmd$", rstudioapi::getSourceEditorContext()$path, ignore.case = TRUE)) {
    return(FALSE)
  }

  return(default)
}

#' Extract a substring safely
#'
#' @param x String to extract from
#' @param start Start position
#' @param end End position
#'
#' @return Extracted substring or empty string if positions are invalid
#' @noRd
safe_substr <- function(x, start, end) {
  if (is.na(x) || is.null(x) || nchar(x) == 0) return("")
  if (start > nchar(x) || end < start) return("")

  substr(x, start, min(end, nchar(x)))
}

#' Check if a string ends with a pattern
#'
#' @param x String to check
#' @param pattern Pattern to look for at the end
#'
#' @return Logical indicating if string ends with pattern
#' @noRd
str_ends_with <- function(x, pattern) {
  pattern_len <- nchar(pattern)
  if (nchar(x) < pattern_len) return(FALSE)

  substr(x, nchar(x) - pattern_len + 1, nchar(x)) == pattern
}

#' Check if a string starts with a pattern
#'
#' @param x String to check
#' @param pattern Pattern to look for at the start
#'
#' @return Logical indicating if string starts with pattern
#' @noRd
str_starts_with <- function(x, pattern) {
  pattern_len <- nchar(pattern)
  if (nchar(x) < pattern_len) return(FALSE)

  substr(x, 1, pattern_len) == pattern
}

#' Get the rest of a string after the first character
#'
#' @param x String
#' @return String minus first character
#' @noRd
str_tail <- function(x) {
  substr(x, 2, nchar(x))
}

#' Get the last character of a string
#'
#' @param x String
#' @return Last character
#' @noRd
last_character <- function(x) {
  substr(x, nchar(x), nchar(x))
}

#' Escape special characters in a string for regex
#'
#' @param x String to escape
#'
#' @return String with special regex characters escaped
#' @noRd
escape_regex <- function(x) {
  gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\*\\+\\?\\{\\}\\\\])", "\\\\\\1", x)
}
