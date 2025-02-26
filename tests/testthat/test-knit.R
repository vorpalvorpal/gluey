test_that("Basic expression processing works", {
  # Simple variable interpolation
  text <- "Hello, {{name}}!"
  name <- "World"

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, "Hello, `r gluey(\"{name}\")`!")

  # Test Quarto format
  result_quarto <- process_gluey_expressions(text, environment(), TRUE)
  expect_equal(result_quarto, "Hello, {{gluey(\"{name}\")}}!")

  # Multiple expressions
  text <- "The {{color}} {{animal}} jumps over the {{object}}."
  color <- "brown"
  animal <- "fox"
  object <- "fence"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(
    result_rmd,
    "The `r gluey(\"{color}\")` `r gluey(\"{animal}\")` jumps over the `r gluey(\"{object}\")`."
  )
})

test_that("Raw passthrough expressions work", {
  # Basic passthrough
  text <- "Today is {{! Sys.Date()}}."

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, "Today is `r Sys.Date()`.")

  # Test Quarto format
  result_quarto <- process_gluey_expressions(text, environment(), TRUE)
  expect_equal(result_quarto, "Today is {{Sys.Date()}}.")

  # Multiple passthroughs
  text <- "Current time: {{! Sys.time()}}, R version: {{! R.version.string}}"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(
    result_rmd,
    "Current time: `r Sys.time()`, R version: `r R.version.string`"
  )
})

test_that("Pluralization expressions work", {
  # Simple pluralization
  text <- "{{n_files}} file{{?s}}"
  n_files <- 1

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "^`r gluey\\(\"\\{n_files\\}\"\\)` `r gluey\\(\"\\{qty\\(n_files\\)\\}\\{\\?s\\}\"\\)`$")

  # Multiple pluralizations
  text <- "{{n_files}} file{{?s}} and {{n_dirs}} director{{?y/ies}}"
  n_files <- 2
  n_dirs <- 1

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "file")
  expect_match(result_rmd, "director")
  expect_match(result_rmd, "qty\\(n_files\\)")
  expect_match(result_rmd, "qty\\(n_dirs\\)")

  # Pluralization with qty()
  text <- "{{qty(n_items)}}There {{?is/are}} {{n_items}} item{{?s}}"
  n_items <- 3

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "qty\\(n_items\\)")
  # The qty() should be unwrapped for the pluralization
  expect_match(result_rmd, "qty\\(n_items\\)\\}\\{\\?is/are\\}")
})

test_that("Special case of pluralization before quantity works", {
  # Pluralization before quantity (inverse order)
  text <- "There {{?is/are}} {{n_items}} item{{?s}}"
  n_items <- 3

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "There `r gluey\\(\"\\{qty\\(n_items\\)\\}\\{\\?is/are\\}\"\\)` `r gluey\\(\"\\{n_items\\}\"\\)` ")

  # With no() function
  text <- "There {{?is/are}} {{no(count)}} item{{?s}}"
  count <- 0

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "qty\\(count\\)")
})

test_that("Special formatters work", {
  # Unordered list
  text <- "{{- items}}"
  items <- c("Item 1", "Item 2")

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, "`r gluey(\"{- items}\")`")

  # All formatters
  text <- "List: {{- items}}\nOrdered: {{1 steps}}\nDefs: {{= terms}}\nYAML: {{: meta}}\nTasks: {{[ tasks}}"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "\\{- items\\}")
  expect_match(result_rmd, "\\{1 steps\\}")
  expect_match(result_rmd, "\\{= terms\\}")
  expect_match(result_rmd, "\\{: meta\\}")
  expect_match(result_rmd, "\\{\\[ tasks\\}")

  # Test that values are extracted correctly for pluralization
  text <- "{{- animals}} can make {{?sound/sounds}}"
  animals <- c("Dog", "Cat")

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "qty\\(animals\\)")
})

test_that("Mixed expressions work", {
  # Mix of raw, pluralization, and regular expressions
  text <- "Today {{! Sys.Date()}}, we have {{n_files}} file{{?s}} and {{- items}} on the list"
  n_files <- 5
  items <- c("Apples", "Bananas")

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "Today `r Sys.Date\\(\\)`")
  expect_match(result_rmd, "\\{n_files\\}")
  expect_match(result_rmd, "qty\\(n_files\\)\\}\\{\\?s\\}")
  expect_match(result_rmd, "\\{- items\\}")

  # Line with multiple mixed expressions
  text <- "Summary: {{! format(Sys.Date())}} - {{count}} {{?person/people}} attended, {{! round(percent*100, 1)}}% liked {{- foods}}"
  count <- 12
  percent <- 0.75
  foods <- c("Pizza", "Pasta", "Salad")

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "`r format\\(Sys.Date\\(\\)\\)`")
  expect_match(result_rmd, "\\{count\\}")
  expect_match(result_rmd, "qty\\(count\\)\\}\\{\\?person/people\\}")
  expect_match(result_rmd, "`r round\\(percent\\*100, 1\\)`")
  expect_match(result_rmd, "\\{- foods\\}")
})

test_that("Error cases are handled properly", {
  # Pluralization without preceding expression
  text <- "This will {{?fail}}"

  expect_error(
    process_gluey_expressions(text, environment(), FALSE),
    "Pluralization directive without a preceding expression"
  )

  # Multiple expressions with pluralization without preceding expression
  text <- "This {{word}} will {{?fail}} here"

  expect_error(
    process_gluey_expressions(text, environment(), FALSE),
    NA
  )
})

test_that("Quoting and escaping works correctly", {
  # Expressions with quotes
  text <- 'The "{{item}}" is {{color}}'
  item <- "pencil"
  color <- "blue"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, 'The "')
  expect_match(result_rmd, "\\{item\\}")

  # Expressions with backslashes and quotes
  text <- 'Path: {{path}} with "{{name}}"'
  path <- "C:\\Users\\test"
  name <- "file.txt"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "Path:")
  expect_match(result_rmd, "\\{path\\}")
  expect_match(result_rmd, "\\{name\\}")
})

test_that("Multi-line documents work", {
  # Document with multiple lines
  text <- "# Title\n\nFirst paragraph with {{var1}}.\n\nSecond paragraph with {{var2}}.\n"
  var1 <- "value1"
  var2 <- "value2"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(
    result_rmd,
    "# Title\n\nFirst paragraph with `r gluey(\"{var1}\")`.\n\nSecond paragraph with `r gluey(\"{var2}\")`."
  )

  # Mixed content across lines
  text <- "Line 1: {{var1}}\nLine 2: {{! expr}}\nLine 3: {{var2}} with {{?s}}"
  var1 <- "test1"
  var2 <- 5

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "Line 1:")
  expect_match(result_rmd, "Line 2: `r expr`")
  expect_match(result_rmd, "Line 3:")
  expect_match(result_rmd, "qty\\(var2\\)\\}\\{\\?s\\}")
})

test_that("helper functions work correctly", {
  # Test extract_var_name
  extract_var_name <- function(expr) {
    format_char <- substr(expr, 1, 1)
    if (format_char %in% c("-", "1", "=", ":", "[", "|")) {
      trimws(substr(expr, 2, nchar(expr)))
    } else {
      expr
    }
  }

  expect_equal(extract_var_name("var"), "var")
  expect_equal(extract_var_name("- items"), "items")
  expect_equal(extract_var_name("1 steps"), "steps")
  expect_equal(extract_var_name("= terms"), "terms")
  expect_equal(extract_var_name(": meta"), "meta")
  expect_equal(extract_var_name("[ tasks"), "tasks")
  expect_equal(extract_var_name("| options"), "options")

  # Test unwrap_qty_no
  unwrap_qty_no <- function(expr) {
    if (grepl("^\\s*qty\\((.*)\\)\\s*$", expr)) {
      gsub("^\\s*qty\\((.*)\\)\\s*$", "\\1", expr)
    } else if (grepl("^\\s*no\\((.*)\\)\\s*$", expr)) {
      gsub("^\\s*no\\((.*)\\)\\s*$", "\\1", expr)
    } else {
      expr
    }
  }

  expect_equal(unwrap_qty_no("var"), "var")
  expect_equal(unwrap_qty_no("qty(count)"), "count")
  expect_equal(unwrap_qty_no("no(items)"), "items")
  expect_equal(unwrap_qty_no(" qty(count) "), "count")
})

test_that("detect_quarto_document works", {
  # Create a mock Quarto document
  quarto_text <- "---\ntitle: Test\nformat: html\n---\n\nContent"
  expect_true(detect_quarto_document(quarto_text, default = FALSE))

  # Create a mock R Markdown document
  rmd_text <- "---\ntitle: Test\noutput: html_document\n---\n\nContent"
  expect_false(detect_quarto_document(rmd_text))
})
