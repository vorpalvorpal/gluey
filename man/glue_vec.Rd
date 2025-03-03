% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glue_vec.R
\name{glue_vec}
\alias{glue_vec}
\title{Format vectors for markdown output}
\usage{
glue_vec(
  .x,
  .item = "{.item}",
  .vec = "{.vec}",
  .sep = ", ",
  .last = ", and ",
  .width = Inf,
  .na = "",
  .transformer_name = identity,
  .transformer_item = identity,
  .envir = parent.frame(),
  .trim = TRUE,
  ...
)
}
\arguments{
\item{.x}{Vector to format}

\item{.item}{Template for each item using \code{{.item}} and \code{{.name}} for interpolation}

\item{.vec}{Template for the entire vector using \code{{.vec}} for interpolation}

\item{.sep}{Separator between items (default: ", ")}

\item{.last}{Separator before the last item (default: ", and ")}

\item{.width}{Maximum line width for collapsed output}

\item{.na}{Character vector to replace NAs in \code{.x} with. (default = "")}

\item{.transformer_name}{Function to transform each name before interpolation}

\item{.transformer_item}{Function to transform each item before interpolation}

\item{.envir}{Environment for evaluating glue expressions}

\item{.trim}{Whether to trim the input template with \code{trim()} or not (default: TRUE)}

\item{...}{Additional arguments passed to \code{\link[glue:glue]{glue::glue()}}}
}
\value{
Formatted markdown text
}
\description{
Formats a vector into markdown text with styling options. Uses glue for interpolation
and follows a functional approach for vector processing.
}
\examples{
# Simple vector formatting
fruits <- c("apples", "bananas", "oranges")
glue_vec(fruits)  # "apples, bananas, and oranges"

# With custom separators
glue_vec(fruits, .sep = "; ", .last = " or ")  # "apples; bananas or oranges"

# Unordered list
glue_vec(fruits, .item = "- {.item}", .sep = "\n")  # "- apples\n- bananas\n- oranges"

# Ordered list
names(fruits) <- 1:3
glue_vec(fruits, .item = "{.name}. {.item}", .sep = "\n")

# Definition list
terms <- c(R = "A language for statistical computing",
  Python = "A general-purpose programming language")
glue_vec(terms, .item = "{.name}\n:    {.item}", .sep = "\n")

# Using item transformations
glue_vec(fruits, .transformer_item = toupper)  # "APPLES, BANANAS, and ORANGES"
}
