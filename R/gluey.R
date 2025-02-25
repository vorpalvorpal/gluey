#' Generate markdown text using glue-style syntax
#'
#' @description
#' `gluey()` generates markdown text using glue-style syntax for
#' interpolation, with additional markdown-specific formatting options.
#' It leverages the cli package's pluralization and interpolation
#' mechanisms but outputs plain markdown rather than styled console text.
#'
#' @param text Character vector with glue-style markup
#' @param ... Additional values to interpolate into text
#' @param .envir Environment in which to evaluate expressions
#'
#' @return A character string with markdown formatting
#'
#' @export
#'
#' @examples
#' # Basic interpolation
#' name <- "Alice"
#' gluey("Hello, {name}!")
#'
#' # Pluralization
#' n_files <- 3
#' gluey("Found {n_files} file{?s}")
#'
#' # List formatting
#' items <- c("apples", "bananas", "oranges")
#' gluey("{- items}")  # Unordered list
#' gluey("{1 items}")  # Ordered list
gluey <- function(text, ..., .envir = parent.frame()) {
  # Create a new environment with the passed values
  values <- list(...)
  if (length(values) > 0) {
    env_names <- names(values)
    if (is.null(env_names)) env_names <- rep("", length(values))

    for (i in seq_along(values)) {
      if (env_names[i] == "") {
        warning("Unnamed arguments to gluey() may not work as expected")
      } else {
        assign(env_names[i], values[[i]], envir = .envir)
      }
    }
  }

  # Process the text
  result <- process_gluey_text(text, .envir)

  # Return the result as a gluey object
  structure(result, class = "gluey")
}

#' Process gluey text with markdown-specific transformations
#'
#' @param text Text to process
#' @param envir Environment for evaluation
#' @return Processed text
#' @noRd
process_gluey_text <- function(text, envir) {
  # Create a transformer function that will handle our special markup
  transformer <- function(code, envir) {
    # Check if this is a special markdown formatter
    if (grepl("^[-=:1[]\\s+.+", code)) {
      format_type <- substr(code, 1, 1)
      expr_text <- trimws(substr(code, 2, nchar(code)))

      # Evaluate the expression
      expr <- tryCatch(
        eval(parse(text = expr_text, keep.source = FALSE), envir = envir),
        error = function(e) {
          warning("Failed to evaluate {", code, "}: ", e$message)
          return(NULL)
        }
      )

      if (is.null(expr)) return("")

      # Format according to the type
      return(glue_vec(expr, format = format_type))
    }

    # Check for alternative joining (or instead of and)
    if (grepl("^\\|\\s+.+", code)) {
      expr_text <- trimws(substr(code, 2, nchar(code)))
      expr <- tryCatch(
        eval(parse(text = expr_text, keep.source = FALSE), envir = envir),
        error = function(e) {
          warning("Failed to evaluate {", code, "}: ", e$message)
          return(NULL)
        }
      )

      if (is.null(expr)) return("")

      # Format with 'or' instead of 'and'
      return(glue_vec(expr, last = " or "))
    }

    # Process pluralization
    if (substr(code, 1, 1) == "?") {
      return(process_pluralization(code, envir))
    }

    # Handle data frames
    expr <- tryCatch(
      eval(parse(text = code, keep.source = FALSE), envir = envir),
      error = function(e) {
        warning("Failed to evaluate {", code, "}: ", e$message)
        return(NULL)
      }
    )

    if (is.null(expr)) return("")

    # Special handling for different object types
    if (inherits(expr, "data.frame")) {
      return(pander::pandoc.table(expr, justify = "left", style = "multiline"))
    }

    if (inherits(expr, "ggplot")) {
      tmp <- tempfile(fileext = ".png")
      ggplot2::ggsave(tmp, expr, width = 7, height = 5)
      return(paste0("![](", tmp, ")"))
    }

    # Default handling - convert to character and collapse
    return(glue_vec(expr))
  }

  # Use glue with our custom transformer

  # Process pluralization context
  qty <- NULL
  if (length(text) > 0) {
    # Look for variables that set the quantity
    matches <- gregexpr("\\{[^{}]+\\}", text, perl = TRUE)
    if (matches[[1]][1] != -1) {
      expr_matches <- regmatches(text, matches)[[1]]

      for (expr in expr_matches) {
        # Extract the expression without braces
        expr_content <- substr(expr, 2, nchar(expr) - 1)
        if (!grepl("^\\?", expr_content)) {
          # Try to evaluate as a quantity
          result <- tryCatch(
            eval(parse(text = expr_content), envir = envir),
            error = function(e) NULL
          )

          if (!is.null(result)) {
            if (is.numeric(result) || is.character(result)) {
              qty <- result
              break  # Use the first valid quantity found
            }
          }
        }
      }
    }
  }

  # Add quantity to the environment for pluralization
  if (!is.null(qty)) {
    envir$._gluey_qty <- qty
  }

  # Process the text with glue
  glue::glue(text, .transformer = transformer, .envir = envir)
}

#' Process pluralization directives
#'
#' @param code Pluralization directive (e.g., "?s" or "?y/ies")
#' @param envir Environment with quantity variable
#' @return Appropriate pluralization suffix
#' @noRd
process_pluralization <- function(code, envir) {
  # Get the quantity from the environment
  qty <- envir$._gluey_qty

  if (is.null(qty)) {
    warning("No quantity found for pluralization")
    return("")
  }

  # Determine the quantity value
  if (is.numeric(qty)) {
    n <- qty
  } else {
    n <- length(qty)
  }

  # Process the pluralization directive
  parts <- strsplit(code, "/", fixed = TRUE)[[1]]
  parts <- parts[-1]  # Remove the "?" part

  if (length(parts) == 0) {
    # Simple pluralization: ?s
    suffix <- substr(code, 2, nchar(code))
    return(if (n == 1) "" else suffix)
  } else if (length(parts) == 1) {
    # Simple suffix: ?s
    return(if (n == 1) "" else parts[1])
  } else if (length(parts) == 2) {
    # Singular/plural: ?y/ies
    return(if (n == 1) parts[1] else parts[2])
  } else if (length(parts) == 3) {
    # Zero/singular/plural: ?no/a/many
    if (n == 0) return(parts[1])
    if (n == 1) return(parts[2])
    return(parts[3])
  } else {
    warning("Invalid pluralization directive: ", code)
    return("")
  }
}

#' Print method for gluey objects
#'
#' @param x A gluey object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns the input object
#' @export
print.gluey <- function(x, ...) {
  cat(x)
  invisible(x)
}

#' Knit print method for gluey objects
#'
#' @param x A gluey object
#' @param ... Additional arguments passed to knitr::asis_output
#'
#' @return The result of knitr::asis_output
#' @export
knit_print.gluey <- function(x, ...) {
  knitr::asis_output(x, ...)
}
