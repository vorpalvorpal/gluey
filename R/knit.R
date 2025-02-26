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

#' Process gluey expressions in a document
#'
#' @param text Document text
#' @param envir Environment for evaluation
#' @param is_quarto Are we modifying a quarto document
#' @return Processed text
#' @noRd
process_gluey_expressions <- function(text, envir = parent.frame(), is_quarto) {
  # Helper function to format expressions
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

    all_exprs <- regmatches(line, expr_matches)[[1]]
    expr_contents <- gsub("\\{\\{([^{}]*)\\}\\}", "\\1", all_exprs)

    # Process expressions on this line
    is_first_expr <- TRUE

    for (j in seq_along(all_exprs)) {
      orig_expr <- all_exprs[j]
      expr <- expr_contents[j]

      # Escape quotes in the expression
      expr_escaped <- gsub("\"", "\\\\\"", expr)

      # Create the replacement
      # First expression on a line creates a new environment
      replacement <- format_expr(
        paste0(
          "gluey_stateful(\"{", expr_escaped, "}\", new_env = ",
          ifelse(is_first_expr, "TRUE", "FALSE"), ")"
        ),
        is_quarto
      )

      # Replace in the line
      line <- sub(orig_expr, replacement, line, fixed = TRUE)
      is_first_expr <- FALSE
    }

    processed_lines[i] <- line
  }

  # Clean up the state environment
  cleanup_gluey_state()

  return(paste(processed_lines, collapse = "\n"))
}

#' @export
#' @rdname gluey_knit
gluey_render <- function(input, ...) {
  gluey_knit(input, ...)
}
