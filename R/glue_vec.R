#' Format vectors for markdown output
#'
#' @description
#' Formats a vector into markdown text with various styling options.
#'
#' @param x Vector to format
#' @param format Format type: "-" (unordered list), "1" (ordered list),
#'   "=" (definition list), ":" (YAML), "[" (task list), or NULL (plain text)
#' @param sep Separator between items when format is NULL
#' @param last Separator before the last item when format is NULL
#' @param pre Text to insert before the entire vector
#' @param post Text to insert after the entire vector
#' @param name_pre Text to insert before each name (for named vectors)
#' @param name_post Text to insert after each name (for named vectors)
#' @param item_pre Text to insert before each item
#' @param item_post Text to insert after each item
#' @param name_fn Function to apply to each name
#' @param item_fn Function to apply to each item
#'
#' @return Formatted markdown text
#' @export
#'
#' @examples
#' # Simple list formatting
#' items <- c("apples", "bananas", "oranges")
#' glue_vec(items, format = "-")  # Unordered list
#' glue_vec(items, format = "1")  # Ordered list
#'
#' # Named vector for definition list
#' terms <- c(R = "A language for statistical computing",
#'   Python = "A general-purpose programming language")
#' glue_vec(terms, format = "=")
#'
#' # Custom separators for inline text
#' glue_vec(items, sep = "; ", last = "; and ")
#'
#' # Using functions to transform items
#' glue_vec(items, item_fn = toupper)
glue_vec <- function(x,
                     format = NULL,
                     sep = ", ",
                     last = ", and ",
                     pre = "",
                     post = "",
                     name_pre = "",
                     name_post = "",
                     item_pre = "",
                     item_post = "",
                     name_fn = identity,
                     item_fn = identity) {
  # Convert to character if needed
  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Handle empty vectors
  if (length(x) == 0) return("")

  # Get names if available
  x_names <- names(x)
  has_names <- !is.null(x_names) && any(x_names != "")

  # Apply processing functions
  x <- vapply(x, item_fn, character(1))
  if (has_names) {
    x_names <- vapply(x_names, name_fn, character(1))
  }

  # Format based on type
  if (!is.null(format)) {
    result <- switch(format,
      "-" = format_unordered_list(x, item_pre, item_post),
      "1" = format_ordered_list(x, item_pre, item_post),
      "=" = format_definition_list(x, x_names, name_pre, name_post, item_pre, item_post),
      ":" = format_yaml(x, x_names),
      "[" = format_task_list(x, x_names),
      # Default to plain text if format not recognized
      format_plain(x, sep, last, item_pre, item_post)
    )
  } else {
    result <- format_plain(x, sep, last, item_pre, item_post)
  }

  paste0(pre, result, post)
}

#' Format vector as an unordered list
#' @noRd
format_unordered_list <- function(x, item_pre, item_post) {
  items <- paste0("- ", item_pre, x, item_post)
  paste(items, collapse = "\n")
}

#' Format vector as an ordered list
#' @noRd
format_ordered_list <- function(x, item_pre, item_post) {
  items <- paste0("1. ", item_pre, x, item_post)
  paste(items, collapse = "\n")
}

#' Format vector as a definition list
#' @noRd
format_definition_list <- function(x, x_names, name_pre, name_post, item_pre, item_post) {
  if (is.null(x_names) || length(x_names) != length(x)) {
    stop("Definition lists require named vectors")
  }

  terms <- paste0(name_pre, x_names, name_post)
  definitions <- paste0(":    ", item_pre, x, item_post)

  result <- character(length(x) * 2)
  for (i in seq_along(x)) {
    idx <- (i - 1) * 2 + 1
    result[idx] <- terms[i]
    result[idx + 1] <- definitions[i]
  }

  paste(result, collapse = "\n")
}

#' Format vector as YAML
#' @noRd
format_yaml <- function(x, x_names) {
  if (is.null(x_names) || length(x_names) != length(x)) {
    stop("YAML formatting requires named vectors")
  }

  yaml_lines <- paste0(x_names, ": ", x)
  paste0("---\n", paste(yaml_lines, collapse = "\n"), "\n---")
}

#' Format vector as a task list
#' @noRd
format_task_list <- function(x, x_names) {
  # For task lists, names are used to determine if task is complete
  if (is.null(x_names)) {
    # If no names, all tasks are incomplete
    items <- paste0("- [ ] ", x)
  } else {
    # Tasks with non-empty names are considered complete
    checkboxes <- ifelse(x_names != "", "- [x] ", "- [ ] ")
    items <- paste0(checkboxes, x)
  }

  paste(items, collapse = "\n")
}

#' Format vector as plain text
#' @noRd
format_plain <- function(x, sep, last, item_pre, item_post) {
  # Format items
  items <- paste0(item_pre, x, item_post)

  # Handle simple cases
  if (length(items) == 1) return(items)
  if (length(items) == 2) return(paste0(items[1], last, items[2]))

  # For longer vectors
  paste0(
    paste(items[-length(items)], collapse = sep),
    last,
    items[length(items)]
  )
}
