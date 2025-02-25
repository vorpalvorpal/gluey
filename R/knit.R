#' Custom knit function for gluey processing
#'
#' This function reads the raw document, processes gluey expressions,
#' and passes the result to the appropriate rendering function.
#'
#' @param input Input file path
#' @param output Output file path (or NULL)
#' @param ... Additional arguments to pass to knit or quarto_render
#'
#' @return The result of the rendering process
#' @export
gluey_knit <- function(input, output = NULL, envir = parent.frame(), ...) {
  # Read input file
  content <- readLines(input, warn = FALSE)
  text <- paste(content, collapse = "\n")

  # Process document
  processed_text <- process_gluey_expressions(text, envir)

  # Write to temporary file
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(processed_text, temp_file)

  # Detect if we're in a Quarto document
  is_quarto <- detect_quarto_document(text)

  if (is_quarto) {
    checkmate::assert_package_installed("quarto")
    result <- quarto::quarto_render(temp_file, output_file = output, ...)
  } else {
    result <- knitr::knit(temp_file, output, ...)
  }

  return(result)
}

#' Process gluey expressions in a document
#'
#' @param text Document text
#' @param envir Environment for evaluation
#' @return Processed text
#' @noRd
process_gluey_expressions <- function(text, envir = parent.frame()) {
  # Create evaluation environment
  env <- new.env(parent = envir)

  # Split into lines to process line by line
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  # Process each line to maintain pluralisation context
  for (i in seq_along(lines)) {
    # Track the last non-plural expression on this line
    last_expr <- NULL

    # First handle raw passthrough with {{! var}}
    raw_pattern <- "\\{\\{\\!\\s*([^{}]*)\\}\\}"
    raw_matches <- gregexpr(raw_pattern, lines[i], perl = TRUE)

    if (raw_matches[[1]][1] != -1) {
      raw_match_info <- regmatches(lines[i], raw_matches)[[1]]

      for (expr in raw_match_info) {
        expr_content <- gsub("\\{\\{\\!\\s*([^{}]*)\\}\\}", "\\1", expr)
        expr_content <- trimws(expr_content)

        # Update the last expression used (for pluralisation)
        # Note: We track passthrough variables for pluralisation too
        last_expr <- expr_content

        # Determine if we're in a Quarto document
        is_quarto <- detect_quarto_document(text)

        # Convert to appropriate format
        if (is_quarto) {
          replacement <- paste0("{{", expr_content, "}}")
        } else {
          replacement <- paste0("`r ", expr_content, "`")
        }

        lines[i] <- sub(expr, replacement, lines[i], fixed = TRUE)
      }
    }

    # Process pluralisation expressions {{?singular/plural}}
    plural_pattern <- "\\{\\{\\?([^{}]*)\\}\\}"
    plural_matches <- gregexpr(plural_pattern, lines[i], perl = TRUE)

    if (plural_matches[[1]][1] != -1) {
      plural_match_info <- regmatches(lines[i], plural_matches)[[1]]

      for (expr in plural_match_info) {
        expr_content <- gsub("\\{\\{\\?([^{}]*)\\}\\}", "\\1", expr)
        expr_content <- trimws(expr_content)

        # Parse the plural pattern
        parts <- strsplit(expr_content, "/", fixed = TRUE)[[1]]

        # Generate the replacement expression using the last_expr
        if (is.null(last_expr)) {
          warning("Pluralisation directive found without a preceding value expression: ", expr)
          replacement <- "{{ERROR: No quantity for pluralisation}}"
        } else {
          # Create the appropriate replacement with the unified pluralise function
          if (length(parts) == 1) {
            # Simple suffix like {{?s}}
            replacement <- paste0("`r gluey::pluralise(", last_expr, ", plural=\"", parts[1], "\")`")
          } else if (length(parts) == 2) {
            # Singular/plural form like {{?y/ies}}
            replacement <- paste0("`r gluey::pluralise(", last_expr,
              ", singular=\"", parts[1], "\", plural=\"", parts[2], "\")`")
          } else if (length(parts) == 3) {
            # Zero/singular/plural form like {{?no/one/many}}
            replacement <- paste0("`r gluey::pluralise(", last_expr,
              ", zero=\"", parts[1], "\", singular=\"", parts[2],
              "\", plural=\"", parts[3], "\")`")
          } else {
            warning("Invalid pluralisation pattern: ", expr_content)
            replacement <- "{{ERROR: Invalid pluralisation pattern}}"
          }
        }

        lines[i] <- sub(expr, replacement, lines[i], fixed = TRUE)
      }
    }

    # Process regular double-brace expressions {{var}}
    pattern <- "\\{\\{([^\\!\\?{}][^{}]*)\\}\\}"
    matches <- gregexpr(pattern, lines[i], perl = TRUE)

    if (matches[[1]][1] != -1) {
      match_positions <- matches[[1]]
      match_lengths <- attr(matches[[1]], "match.length")
      match_info <- regmatches(lines[i], matches)[[1]]

      # Process matches from left to right to maintain context
      for (j in seq_along(match_info)) {
        expr <- match_info[j]
        expr_content <- gsub("\\{\\{([^{}]*)\\}\\}", "\\1", expr)
        expr_content <- trimws(expr_content)

        # Update the last expression used (for pluralization)
        last_expr <- expr_content

        # Check for format specifiers (-, 1, =, :, [)
        format_char <- substr(expr_content, 1, 1)
        if (format_char %in% c("-", "1", "=", ":", "[")) {
          # Handle special formatters
          expr_content <- trimws(substr(expr_content, 2, nchar(expr_content)))

          # Evaluate with gluey
          replacement <- paste0("`r gluey::glue_vec(", expr_content,
            ", format = \"", format_char, "\")`")
        } else {
          # Regular expression
          replacement <- paste0("`r gluey::gluey(\"", expr_content, "\")`")
        }

        lines[i] <- sub(expr, replacement, lines[i], fixed = TRUE)
      }
    }
  }

  # Rejoin the lines
  return(paste(lines, collapse = "\n"))
}

#' Detect if a document is a Quarto document
#'
#' @param text Document text
#' @return Logical indicating if the document is a Quarto document
#' @noRd
detect_quarto_document <- function(text) {
  # Look for Quarto-specific YAML elements
  has_quarto_yaml <- grepl("format:\\s*", text, perl = TRUE) ||
    grepl("engine:\\s*quarto", text, perl = TRUE)

  # Look for file extension if available
  if (hasName(attributes(text), "file")) {
    file_ext <- tools::file_ext(attr(text, "file"))
    is_qmd <- tolower(file_ext) == "qmd"
    return(is_qmd || has_quarto_yaml)
  }

  return(has_quarto_yaml)
}

#' @export
#' @rdname gluey_knit
gluey_render <- function(input, ...) {
  gluey_knit(input, ...)
}
