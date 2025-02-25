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
  # Helper functions
  format_expr <- function(expr, is_quarto) {
    if (is_quarto) paste0("{{", expr, "}}") else paste0("`r ", expr, "`")
  }

  create_gluey_expr <- function(expr, is_quarto) {
    expr_escaped <- gsub("\"", "\\\\\"", expr)
    format_expr(paste0("gluey(\"{", expr_escaped, "}\")"), is_quarto)
  }

  create_plural_expr <- function(qty_expr, plur_pattern, is_quarto) {
    qty_escaped <- gsub("\"", "\\\\\"", qty_expr)
    plur_escaped <- gsub("\"", "\\\\\"", plur_pattern)
    format_expr(paste0("gluey(\"{qty(", qty_escaped, ")}{?", plur_escaped, "}\")"), is_quarto)
  }

  extract_var_name <- function(expr) {
    format_char <- substr(expr, 1, 1)
    if (format_char %in% c("-", "1", "=", ":", "[", "|")) {
      trimws(substr(expr, 2))
    } else {
      expr
    }
  }

  unwrap_qty_no <- function(expr) {
    if (grepl("^\\s*qty\\((.*)\\)\\s*$", expr)) {
      gsub("^\\s*qty\\((.*)\\)\\s*$", "\\1", expr)
    } else if (grepl("^\\s*no\\((.*)\\)\\s*$", expr)) {
      gsub("^\\s*no\\((.*)\\)\\s*$", "\\1", expr)
    } else {
      expr
    }
  }

  # Determine document type and prepare processing
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

    # Special case: pluralization before quantity
    if (length(all_exprs) == 2 &&
      substr(expr_contents[1], 1, 1) == "?" &&
      substr(expr_contents[2], 1, 1) != "?") {

      plur_pattern <- substr(expr_contents[1], 2)
      expr_var2 <- extract_var_name(expr_contents[2])
      qty_expr <- unwrap_qty_no(expr_var2)

      replacement1 <- create_plural_expr(qty_expr, plur_pattern, is_quarto)
      replacement2 <- create_gluey_expr(expr_contents[2], is_quarto)

      line <- sub(all_exprs[1], replacement1, line, fixed = TRUE)
      line <- sub(all_exprs[2], replacement2, line, fixed = TRUE)

      processed_lines[i] <- line
      next
    }

    # Regular case: process expressions in order
    last_expr <- NULL

    for (j in seq_along(all_exprs)) {
      orig_expr <- all_exprs[j]
      expr <- expr_contents[j]

      if (substr(expr, 1, 1) == "!") {
        # Raw passthrough
        expr_content <- trimws(substr(expr, 2))
        replacement <- format_expr(expr_content, is_quarto)
        last_expr <- expr_content
      } else if (substr(expr, 1, 1) == "?") {
        # Pluralization
        if (is.null(last_expr)) {
          cli::cli_abort(c(
            "Pluralization directive without a preceding expression: {{?{substr(expr, 2)}}}",
            "i" = "Each pluralization directive needs an associated quantity expression"
          ))
        }

        plur_pattern <- substr(expr, 2)
        qty_expr <- unwrap_qty_no(last_expr)
        replacement <- create_plural_expr(qty_expr, plur_pattern, is_quarto)
      } else {
        # Regular expression
        expr_var <- extract_var_name(expr)
        replacement <- create_gluey_expr(expr, is_quarto)
        last_expr <- expr_var
      }

      line <- sub(orig_expr, replacement, line, fixed = TRUE)
    }

    processed_lines[i] <- line
  }

  paste(processed_lines, collapse = "\n")
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

#' @export
#' @rdname gluey_knit
gluey_render <- function(input, ...) {
  gluey_knit(input, ...)
}
