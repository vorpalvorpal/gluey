#' @title Gluey: Markdown Text Generation with Enhanced Templates
#' @description Generate markdown text using familiar glue syntax with enhanced
#' templating features and pluralization.
#'
#' @import glue
#' @importFrom uuid UUIDgenerate
#' @importFrom pander pandoc.table
"_PACKAGE"

#' Format text with glue syntax, vector formatting, and pluralization
#'
#' @param text Text template with glue-style interpolation
#' @param ... Additional values to use for interpolation
#' @param .envir Environment for evaluation
#' @return Formatted text
#' @export
#' @examples
#' name <- "World"
#' gluey("Hello, {name}!")
#'
#' # Pluralization
#' n_files <- 0
#' gluey("Found {n_files} file{?s}.")
#' n_files <- 1
#' gluey("Found {n_files} file{?s}.")
#'
#' # Vector formatting
#' fruits <- c("apples", "bananas", "oranges")
#' gluey("I like {fruits}.")
gluey <- function(text, ..., .envir = parent.frame()) {
  # Environment to store state for pluralization
  values <- new.env(parent = emptyenv())
  values$empty <- uuid::UUIDgenerate()
  values$qty <- values$empty
  values$num_subst <- 0L
  values$postprocess <- FALSE
  values$pmarkers <- list()

  # Create a transformer function that handles our special markup
  transformer <- function(code, envir) {
    # Check if this is a pluralization directive
    if (substr(code, 1, 1) == "?") {
      # The most recent expression sets the quantity
      if (identical(values$qty, values$empty)) {
        values$postprocess <- TRUE
        id <- uuid::UUIDgenerate()
        values$pmarkers[[id]] <- code
        return(id)
      } else {
        return(process_plural(make_quantity(values$qty), code))
      }
    }

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

      # Update quantity for pluralization
      values$num_subst <- values$num_subst + 1
      values$qty <- expr

      if (is.null(expr)) return("")

      # Format according to the type
      if (format_type == "-") {
        return(glue_vec(expr, .item = "- {.item}", .sep = "\n"))
      } else if (format_type == "1") {
        return(glue_vec(expr, .item = "1. {.item}", .sep = "\n"))
      } else if (format_type == "=") {
        if (is.null(names(expr)) || any(names(expr) == "")) {
          stop("Definition lists require named vectors")
        }
        return(glue_vec(expr, .item = "{.name}\n:    {.item}", .sep = "\n"))
      } else if (format_type == ":") {
        if (is.null(names(expr)) || any(names(expr) == "")) {
          stop("YAML formatting requires named vectors")
        }
        return(glue_vec(expr, .item = "{.name}: {.item}", .sep = "\n",
          .vec = "---\n{.vec}\n---"))
      } else if (format_type == "[") {
        if (is.null(names(expr))) {
          return(glue_vec(expr, .item = "- [ ] {.item}", .sep = "\n"))
        } else {
          return(glue_vec(expr, .sep = "\n",
            .item = "- [{if (.name == 'done') 'x' else ' '}] {.item}"))
        }
      }
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

      # Update quantity for pluralization
      values$num_subst <- values$num_subst + 1
      values$qty <- expr

      if (is.null(expr)) return("")

      # Format with 'or' instead of 'and'
      return(glue_vec(expr, .last = " or "))
    }

    # For normal expressions
    values$num_subst <- values$num_subst + 1

    # Evaluate the expression
    expr <- tryCatch(
      eval(parse(text = code, keep.source = FALSE), envir = envir),
      error = function(e) {
        warning("Failed to evaluate {", code, "}: ", e$message)
        return(NULL)
      }
    )

    # Update quantity for pluralization - this is key
    values$qty <- expr

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

    # Use our own vector formatting
    return(glue_vec(expr))
  }

  # Process with glue
  raw <- glue::glue(text, .envir = .envir, .transformer = transformer)

  # Post-process pluralization markers
  post_process_plurals(raw, values)
}

#' Process document with gluey syntax (double braces)
#'
#' @param text Document text to process
#' @param .envir Environment for evaluation
#' @return Processed document
#' @export
gluey_process <- function(text, .envir = parent.frame()) {
  # Convert double braces to single braces
  text <- gsub("\\{\\{([^{}]+)\\}\\}", "{\\1}", text)

  # Process with our custom pluralization
  gluey(text, .envir = .envir)
}

#' Custom knit function for R Markdown/Quarto
#'
#' @param input Input file
#' @param ... Additional parameters passed to knitr::knit
#' @return Knitted document output
#' @export
gluey_knit <- function(input, ...) {
  # Check if gluey preprocessing is enabled
  enabled <- getOption("gluey.enabled", TRUE)

  # Read input file
  text <- readLines(input, warn = FALSE)

  # Process with gluey if enabled
  if (enabled) {
    text <- gluey_process(paste(text, collapse = "\n"))
  }

  # Write to temporary file for knitr
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(text, tmp)

  # Process with knitr
  knitr::knit(tmp, ...)
}
