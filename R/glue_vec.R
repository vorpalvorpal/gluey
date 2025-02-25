#' Format vectors for markdown output
#'
#' @description
#' Formats a vector into markdown text with styling options. Uses glue for interpolation
#' and follows a functional approach for vector processing.
#'
#' @param .x Vector to format
#' @param .item Template for each item using `{.item}` and `{.name}` for interpolation
#' @param .vec Template for the entire vector using `{.vec}` for interpolation
#' @param .sep Separator between items (default: ", ")
#' @param .last Separator before the last item (default: ", and ")
#' @param .width Maximum line width for collapsed output
#' @param .transformer_name Function to transform each name before interpolation
#' @param .transformer_item Function to transform each item before interpolation
#' @param .envir Environment for evaluating glue expressions
#' @param .trim Whether to trim the input template with `trim()` or not (default: TRUE)
#' @param ... Additional arguments passed to [glue::glue()]
#'
#' @return Formatted markdown text
#' @export
#'
#' @examples
#' # Simple vector formatting
#' fruits <- c("apples", "bananas", "oranges")
#' glue_vec(fruits)  # "apples, bananas, and oranges"
#'
#' # With custom separators
#' glue_vec(fruits, .sep = "; ", .last = " or ")  # "apples; bananas or oranges"
#'
#' # Unordered list
#' glue_vec(fruits, .item = "- {.item}", .sep = "\n")  # "- apples\n- bananas\n- oranges"
#'
#' # Ordered list
#' names(fruits) <- 1:3
#' glue_vec(fruits, .item = "{.name}. {.item}", .sep = "\n")
#'
#' # Definition list
#' terms <- c(R = "A language for statistical computing",
#'   Python = "A general-purpose programming language")
#' glue_vec(terms, .item = "{.name}\n:    {.item}", .sep = "\n")
#'
#' # Using item transformations
#' glue_vec(fruits, .transformer_item = toupper)  # "APPLES, BANANAS, and ORANGES"
glue_vec <- function(.x,
                     .item = "{.item}",
                     .vec = "{.vec}",
                     .sep = ", ",
                     .last = ", and ",
                     .width = Inf,
                     .transformer_name = identity,
                     .transformer_item = identity,
                     .envir = parent.frame(),
                     .trim = TURE,
                     ...) {
  # Input validation using checkmate::check and cli::cli_abort
  if (!checkmate::test_vector(.x)) {
    cli::cli_abort("Argument {.arg .x} must be a vector.")
  }
  if (!checkmate::test_string(.item)) {
    cli::cli_abort("Argument {.arg .item} must be a string.")
  }
  if (!checkmate::test_string(.vec)) {
    cli::cli_abort("Argument {.arg .vec} must be a string.")
  }
  if (!checkmate::test_string(.sep)) {
    cli::cli_abort("Argument {.arg .sep} must be a string.")
  }
  if (!checkmate::test_string(.last)) {
    cli::cli_abort("Argument {.arg .last} must be a string.")
  }
  if (!checkmate::test_number(.width, null.ok = TRUE)) {
    cli::cli_abort("Argument {.arg .width} must be an integer or NULL.")
  }
  if (!checkmate::test_function(.transformer_name)) {
    cli::cli_abort("Argument {.arg .transformer_name} must be a function.")
  }
  if (!checkmate::test_function(.transformer_item)) {
    cli::cli_abort("Argument {.arg .transformer_item} must be a function.")
  }

  # Handle empty vectors
  if (length(.x) == 0) return("")

  # Convert to character if needed
  if (!is.character(.x)) {
    .x <- as.character(.x)
  }

  # Apply item transformers
  x_transformed <- .transformer_item(.x)

  # Process each item with names if available
  has_names <- !is.null(names(.x)) && any(names(.x) != "")

  if (has_names) {
    # Transform names
    names_transformed <- .transformer_name(names(.x))

    # Apply item template to each item with its name
    formatted_items <- vapply(seq_along(.x), function(i) {
      item_env <- new.env(parent = .envir)
      item_env$.item <- x_transformed[i]
      item_env$.name <- names_transformed[i]
      glue::glue(.item, .envir = item_env, ...)
    }, character(1))
  } else {
    # Apply item template to each item without names
    formatted_items <- vapply(seq_along(.x), function(i) {
      item_env <- new.env(parent = .envir)
      item_env$.item <- x_transformed[i]
      item_env$.name <- ""
      glue::glue(.item, .envir = item_env, ...)
    }, character(1))
  }

  # Collapse items with separators
  collapsed <- glue::glue_collapse(formatted_items, sep = .sep, last = .last, width = .width)

  # Apply vector template
  vector_env <- new.env(parent = .envir)
  vector_env$.vec <- collapsed
  glue::glue(.vec, .envir = vector_env, ...)
}
