#' @title Gluey: Markdown Text Generation with Enhanced Templates
#' @description Generate markdown text using familiar glue syntax with enhanced
#' templating features and pluralization.
#'
#' @import glue
#' @importFrom uuid UUIDgenerate
#' @importFrom pander pandoc.table
"_PACKAGE"

#' Create a transformer function for gluey processing
#'
#' @param values Environment to store state for pluralization
#' @return A function that can be used as a glue transformer
#' @noRd
create_gluey_transformer <- function(values) {
  function(code, envir) {
    # Handle direct glue expression with ! prefix
    if (substr(code, 1, 1) == "!") {
      expr_text <- trimws(substr(code, 2, nchar(code)))
      return(glue::glue(expr_text, .envir = envir))
    }

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
        return(glue_vec(expr, .item = "- {.item}", .sep = "\n", .last = "\n"))
      } else if (format_type == "1") {
        return(glue_vec(expr, .item = "1. {.item}", .sep = "\n", .last = "\n"))
      } else if (format_type == "=") {
        if (is.null(names(expr)) || any(names(expr) == "")) {
          stop("Definition lists require named vectors")
        }
        return(glue_vec(expr, .item = "{.name}\n:    {.item}", .sep = "\n", .last = "\n"))
      } else if (format_type == ":") {
        if (is.null(names(expr)) || any(names(expr) == "")) {
          stop("YAML formatting requires named vectors")
        }
        return(glue_vec(expr, .item = "{.name}: {.item}", .sep = "\n", .last = "\n",
          .vec = "---\n{.vec}\n---"))
      } else if (format_type == "[") {
        if (is.null(names(expr))) {
          return(glue_vec(expr, .item = "- [ ] {.item}", .sep = "\n", .last = "\n"))
        } else {
          return(glue_vec(expr, .sep = "\n", .last = "\n",
            .item = "- [{if (.name == '') ' ' else .name}] {.item}"))
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
      return(pander::pandoc.table.return(expr, justify = "left", style = "multiline"))
    }

    if (inherits(expr, "ggplot")) {
      tmp <- tempfile(fileext = ".png")
      ggplot2::ggsave(tmp, expr, width = 7, height = 5)
      return(paste0("![](", tmp, ")"))
    }

    # Use our own vector formatting
    return(glue_vec(expr))
  }
}

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
  transformer <- create_gluey_transformer(values)

  # Process with glue
  raw <- glue::glue(text, .envir = .envir, .transformer = transformer)

  # Post-process pluralization markers
  if (values$postprocess) {
    raw <- post_process_plurals(raw, values)
  }

  return(raw)
}

#' Format text with glue syntax, maintaining state across multiple calls
#'
#' This function works like `gluey()` but maintains pluralization state
#' across multiple calls. This is primarily used for document processing
#' where expressions are split across multiple calls.
#'
#' @param text Text template with glue-style interpolation
#' @param ... Additional values to use for interpolation
#' @param .envir Environment for evaluation
#' @param new_env Whether to create a fresh state environment
#' @return Formatted text
#' @export
#' @examples
#' # First expression sets up the environment and stores the quantity
#' gluey_stateful("Found {2} file", new_env = TRUE)
#'
#' # Second expression uses the quantity from the first
#' gluey_stateful("{?s}")
gluey_stateful <- function(text, ..., .envir = parent.frame(), new_env = FALSE) {
  # Get or create the state environment
  values <- get_gluey_state(new_env)

  # Create a transformer function that handles our special markup
  transformer <- create_gluey_transformer(values)

  # Process with glue
  raw <- glue::glue(text, .envir = .envir, .transformer = transformer)

  # Post-process pluralization markers
  if (values$postprocess) {
    raw <- post_process_plurals(raw, values)
  }

  return(raw)
}
