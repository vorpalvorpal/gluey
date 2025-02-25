#' Generate a random identifier
#'
#' Creates a unique random string for use as a temporary marker
#' in text processing.
#'
#' @param prefix Optional prefix for the ID
#' @param length Length of the random part
#'
#' @return A random string ID
#' @noRd
random_id <- function(prefix = "kw", length = 8) {
  paste0(
    prefix,
    "_",
    paste0(sample(c(letters, 0:9), length, replace = TRUE), collapse = "")
  )
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

#' Detect if a file is a Quarto document
#'
#' @param file File path or content
#' @return Logical indicating if the file is a Quarto document
#' @noRd
detect_quarto_document <- function(file) {
  # If file is a path, read it first
  if (length(file) == 1 && file.exists(file)) {
    content <- paste(readLines(file, warn = FALSE), collapse = "\n")
  } else {
    content <- paste(file, collapse = "\n")
  }

  # Look for Quarto-specific YAML elements
  has_quarto_yaml <- grepl("format:\\s*", content, perl = TRUE) ||
    grepl("engine:\\s*quarto", content, perl = TRUE)

  # Look for file extension if available
  if (length(file) == 1 && file.exists(file)) {
    file_ext <- tolower(tools::file_ext(file))
    is_qmd <- file_ext == "qmd"
    return(is_qmd || has_quarto_yaml)
  }

  return(has_quarto_yaml)
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

#' Escape special characters in a string for regex
#'
#' @param x String to escape
#'
#' @return String with special regex characters escaped
#' @noRd
escape_regex <- function(x) {
  gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\*\\+\\?\\{\\}\\\\])", "\\\\\\1", x)
}
