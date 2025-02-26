test_that("Basic expression processing works", {
  # Simple variable interpolation
  text <- "Hello, {{name}}!"
  name <- "World"

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, "`r gluey(\"Hello, {name}!\")`")

  # Test Quarto format
  result_quarto <- process_gluey_expressions(text, environment(), TRUE)
  expect_equal(result_quarto, "{{gluey(\"Hello, {name}!\")}}")

  # Multiple expressions in one line
  text <- "The {{color}} {{animal}} jumps over the {{object}}."
  color <- "brown"
  animal <- "fox"
  object <- "fence"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, "`r gluey(\"The {color} {animal} jumps over the {object}.\")`")
})

test_that("Pluralization expressions work", {
  # Simple pluralization
  text <- "{{n_files}} file{{?s}}"
  n_files <- 1

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, "`r gluey(\"{n_files} file{?s}\")`")

  # Pluralization with quantity
  text <- "There {{?is/are}} {{n_items}} item{{?s}}"
  n_items <- 3

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, "`r gluey(\"There {?is/are} {n_items} item{?s}\")`")

  # Complex pluralization patterns
  text <- "You have {{n_files}} file{{?s}} and {{n_folders}} folder{{?s}}."
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(
    result_rmd,
    "`r gluey(\"You have {n_files} file{?s} and {n_folders} folder{?s}.\")`"
  )
})

test_that("Raw passthrough expressions work", {
  # Basic passthrough
  text <- "Today is {{! Sys.Date()}}."

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, "`r gluey(\"Today is {! Sys.Date()}.\")`")

  # Multiple passthroughs
  text <- "Current time: {{! Sys.time()}}, R version: {{! R.version.string}}"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(
    result_rmd,
    "`r gluey(\"Current time: {! Sys.time()}, R version: {! R.version.string}\")`"
  )
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
  expected <- paste(
    "`r gluey(\"List: {- items}\")`",
    "`r gluey(\"Ordered: {1 steps}\")`",
    "`r gluey(\"Defs: {= terms}\")`",
    "`r gluey(\"YAML: {: meta}\")`",
    "`r gluey(\"Tasks: {[ tasks}\")`",
    sep = "\n"
  )
  expect_equal(result_rmd, expected)
})

test_that("Mixed expressions work", {
  # Mix of raw, pluralization, and regular expressions
  text <- "Today {{! Sys.Date()}}, we have {{n_files}} file{{?s}} and {{- items}} on the list"

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(
    result_rmd,
    "`r gluey(\"Today {! Sys.Date()}, we have {n_files} file{?s} and {- items} on the list\")`"
  )

  # Line with multiple mixed expressions
  text <- "Summary: {{! format(Sys.Date())}} - {{count}} {{?person/people}} attended, {{! round(percent*100, 1)}}% liked {{- foods}}"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(
    result_rmd,
    "`r gluey(\"Summary: {! format(Sys.Date())} - {count} {?person/people} attended, {! round(percent*100, 1)}% liked {- foods}\")`"
  )
})

test_that("Multi-line documents work", {
  # Document with multiple lines
  text <- "# Title\n\nFirst paragraph with {{var1}}.\n\nSecond paragraph with {{var2}}.\n"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expected <- paste(
    "# Title\n",
    "\n",
    "`r gluey(\"First paragraph with {var1}.\")`\n",
    "\n",
    "`r gluey(\"Second paragraph with {var2}.\")`\n",
    sep = ""
  )
  expect_equal(result_rmd, expected)
})

test_that("Quoting and escaping works correctly", {
  # Expressions with quotes
  text <- 'The "{{item}}" is {{color}}'

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, '`r gluey("The \\"{item}\\" is {color}")`')

  # Expressions with backslashes and quotes
  text <- 'Path: {{path}} with "{{name}}"'

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_equal(result_rmd, '`r gluey("Path: {path} with \\"{name}\\"")`')
})

test_that("detect_quarto_document works", {
  # Create a mock Quarto document
  quarto_text <- "---\ntitle: Test\nformat: html\n---\n\nContent"
  expect_true(detect_quarto_document(quarto_text, default = FALSE))

  # Create a mock R Markdown document
  rmd_text <- "---\ntitle: Test\noutput: html_document\n---\n\nContent"
  expect_false(detect_quarto_document(rmd_text, default = TRUE))
})

test_that("Full document processing works", {
  # Create a mock document with various gluey expressions
  text <- paste(
    "---",
    "title: Test Document",
    "author: Test Author",
    "---",
    "",
    "# Introduction",
    "",
    "Hello, {{name}}! Today is {{! Sys.Date()}}.",
    "",
    "## Items",
    "",
    "We have {{n_items}} item{{?s}} in our inventory:",
    "",
    "{{- items}}",
    "",
    "Thank you for {{?your/your}} attention!",
    sep = "\n"
  )

  # Set variables
  name <- "World"
  n_items <- 3
  items <- c("Item A", "Item B", "Item C")

  # Process as R Markdown
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)

  # Check key parts
  expect_match(result_rmd, "Hello.*gluey.*name", all = TRUE)
  expect_match(result_rmd, "Today.*Sys.Date", all = TRUE)
  expect_match(result_rmd, "{n_items}.*item.*\\?s", all = TRUE)
  expect_match(result_rmd, "{- items}", all = TRUE)

  # Process as Quarto
  result_quarto <- process_gluey_expressions(text, environment(), TRUE)

  # Check key parts for Quarto format
  expect_match(result_quarto, "Hello.*{{gluey.*name", all = TRUE)
  expect_match(result_quarto, "Today.*Sys.Date", all = TRUE)
  expect_match(result_quarto, "{n_items}.*item.*\\?s", all = TRUE)
  expect_match(result_quarto, "{- items}", all = TRUE)
})
