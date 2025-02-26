#' Custom knit function for gluey processing
#'
#' This function reads the raw document, processes gluey expressions,
#' and passes the result to the appropriate rendering function.
#'
#' @param input Input file path
#' @param output Output file path (or NULL)
#' @param envir Environment for evaluation
#' @param ... Additional arguments to pass to knit or quarto_render
#'
#' @return The result of the rendering process
#' @export
gluey_knit <- function(input, output = NULL, envir = parent.frame(), ...) {
  # Read input file
  content <- readLines(input, warn = FALSE)
  text <- paste(content, collapse = "\n")

  # Detect if we're in a Quarto document
  is_quarto <- detect_quarto_document(text)

  # Process document
  processed_text <- process_gluey_expressions(text, envir, is_quarto)

  # Write to temporary file
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(processed_text, temp_file)

  if (is_quarto) {
    result <- quarto::quarto_render(temp_file, output_file = output, ...)
  } else {
    result <- knitr::knit(temp_file, output, ...)
  }

  return(result)
}

#' @export
#' @rdname gluey_knit
gluey_render <- function(input, ...) {
  gluey_knit(input, ...)
}

#' Process gluey expressions in a document
#'
#' @param text Document text
#' @param envir Environment for evaluation
#' @param is_quarto Are we modifying a quarto document
#' @param test_env Optional test environment for testing
#' @return Processed text
#' @noRd
process_gluey_expressions <- function(text, envir = parent.frame(), is_quarto, test_env = NULL) {
  # Helper function to format expressions for R Markdown or Quarto
  format_expr <- function(expr, is_quarto) {
    if (is_quarto) paste0("{{", expr, "}}") else paste0("`r ", expr, "`")
  }

  # Process line by line
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  processed_lines <- character(length(lines))

  for (i in seq_along(lines)) {
    line <- lines[i]
    expr_pattern <- "\\{\\{([^{}]*)\\}\\}"
    expr_matches <- gregexpr(expr_pattern, line, perl = TRUE)

    if (expr_matches[[1]][1] == -1) {
      processed_lines[i] <- line
      next
    }

    # Extract all expressions from the line
    all_exprs <- regmatches(line, expr_matches)[[1]]
    expr_contents <- gsub("\\{\\{([^{}]*)\\}\\}", "\\1", all_exprs)

    # Create a substitution mapping
    original_to_gluey <- list()
    for (j in seq_along(all_exprs)) {
      original_to_gluey[[all_exprs[j]]] <- paste0("{", expr_contents[j], "}")
    }

    # Replace all original expressions with gluey-style expressions
    modified_line <- line
    for (orig in names(original_to_gluey)) {
      modified_line <- gsub(orig, original_to_gluey[[orig]], modified_line, fixed = TRUE)
    }

    # Escape quotes in the modified line
    escaped_line <- gsub('"', '\\\\"', modified_line, fixed = TRUE)

    # Wrap the entire line in a single gluey call
    gluey_call <- paste0('gluey("', escaped_line, '")')

    # Format for R Markdown or Quarto
    processed_lines[i] <- format_expr(gluey_call, is_quarto)
  }

  return(paste(processed_lines, collapse = "\n"))
}
