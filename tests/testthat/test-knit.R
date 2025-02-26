test_that("Basic expression processing works", {
  # Simple variable interpolation
  text <- "Hello, {{name}}!"
  name <- "World"

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "\\`r gluey_stateful\\(\"\\{name\\}\"", all = TRUE)
  expect_match(result_rmd, "new_env = TRUE", all = TRUE)

  # Test Quarto format
  result_quarto <- process_gluey_expressions(text, environment(), TRUE)
  expect_match(result_quarto, "\\{\\{gluey_stateful\\(\"\\{name\\}\"", all = TRUE)
  expect_match(result_quarto, "new_env = TRUE", all = TRUE)

  # Multiple expressions
  text <- "The {{color}} {{animal}} jumps over the {{object}}."
  color <- "brown"
  animal <- "fox"
  object <- "fence"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "The `r gluey_stateful\\(\"\\{color\\}\"", all = TRUE)
  expect_match(result_rmd, "jumps over the `r gluey_stateful\\(\"\\{object\\}\"", all = TRUE)
  expect_match(result_rmd, "new_env = TRUE", all = FALSE)
  expect_match(result_rmd, "new_env = FALSE", all = FALSE)
})

test_that("Raw passthrough expressions work", {
  # Basic passthrough
  text <- "Today is {{! Sys.Date()}}."

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "gluey_stateful\\(\"\\{! Sys.Date\\(\\)\\}\"", all = TRUE)
  expect_match(result_rmd, "new_env = TRUE", all = TRUE)

  # Test Quarto format
  result_quarto <- process_gluey_expressions(text, environment(), TRUE)
  expect_match(result_quarto, "\\{\\{gluey_stateful\\(\"\\{! Sys.Date\\(\\)\\}\"", all = TRUE)
  expect_match(result_quarto, "new_env = TRUE", all = TRUE)

  # Multiple passthroughs
  text <- "Current time: {{! Sys.time()}}, R version: {{! R.version.string}}"

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "gluey_stateful\\(\"\\{! Sys.time\\(\\)\\}\"", all = TRUE)
  expect_match(result_rmd, "gluey_stateful\\(\"\\{! R.version.string\\}\"", all = TRUE)
  expect_match(result_rmd, "new_env = TRUE", all = FALSE)
  expect_match(result_rmd, "new_env = FALSE", all = FALSE)
})

test_that("Pluralization expressions work", {
  # Simple pluralization
  text <- "{{n_files}} file{{?s}}"
  n_files <- 1

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "gluey_stateful\\(\"\\{n_files\\}\".*new_env = TRUE", all = TRUE)
  expect_match(result_rmd, "gluey_stateful\\(\"\\{\\?s\\}\".*new_env = FALSE", all = TRUE)

  # Multiple pluralizations
  text <- "{{n_files}} file{{?s}} and {{n_dirs}} director{{?y/ies}}"
  n_files <- 2
  n_dirs <- 1

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "file")
  expect_match(result_rmd, "director")
  expect_match(result_rmd, "new_env = TRUE", all = FALSE)
  expect_match(result_rmd, "new_env = FALSE", all = FALSE)

  # Pluralization with qty()
  text <- "{{qty(n_items)}}There {{?is/are}} {{n_items}} item{{?s}}"
  n_items <- 3

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "qty\\(n_items\\)")
  expect_match(result_rmd, "\\{\\?is/are\\}")
})

test_that("Special case of pluralization before quantity works", {
  # Pluralization before quantity (inverse order)
  text <- "There {{?is/are}} {{n_items}} item{{?s}}"
  n_items <- 3

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "gluey_stateful\\(\"\\{\\?is/are\\}\"", all = TRUE)
  expect_match(result_rmd, "gluey_stateful\\(\"\\{n_items\\}\"", all = TRUE)

  # With no() function
  text <- "There {{?is/are}} {{no(count)}} item{{?s}}"
  count <- 0

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "no\\(count\\)")
})

test_that("Special formatters work", {
  # Unordered list
  text <- "{{- items}}"
  items <- c("Item 1", "Item 2")

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "gluey_stateful\\(\"\\{- items\\}\"", all = TRUE)
  expect_match(result_rmd, "new_env = TRUE", all = TRUE)

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
  expect_match(result_rmd, "animals")
  expect_match(result_rmd, "\\?sound/sounds")
})

test_that("Mixed expressions work", {
  # Mix of raw, pluralization, and regular expressions
  text <- "Today {{! Sys.Date()}}, we have {{n_files}} file{{?s}} and {{- items}} on the list"
  n_files <- 5
  items <- c("Apples", "Bananas")

  # Test R Markdown format
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "Today")
  expect_match(result_rmd, "Sys.Date\\(\\)")
  expect_match(result_rmd, "\\{n_files\\}")
  expect_match(result_rmd, "\\{\\?s\\}")
  expect_match(result_rmd, "\\{- items\\}")

  # Line with multiple mixed expressions
  text <- "Summary: {{! format(Sys.Date())}} - {{count}} {{?person/people}} attended, {{! round(percent*100, 1)}}% liked {{- foods}}"
  count <- 12
  percent <- 0.75
  foods <- c("Pizza", "Pasta", "Salad")

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "format\\(Sys.Date\\(\\)\\)")
  expect_match(result_rmd, "\\{count\\}")
  expect_match(result_rmd, "\\{\\?person/people\\}")
  expect_match(result_rmd, "round\\(percent\\*100, 1\\)")
  expect_match(result_rmd, "\\{- foods\\}")
})

test_that("Error cases are handled properly", {
  # Skipping test for pluralization without preceding expression,
  # as this is now handled differently with the stateful approach

  # Multiple expressions with mixed content
  text <- "This {{word}} {{!expr}} here"

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
  expect_match(result_rmd, "# Title")
  expect_match(result_rmd, "First paragraph with")
  expect_match(result_rmd, "gluey_stateful\\(\"\\{var1\\}\"")
  expect_match(result_rmd, "Second paragraph with")
  expect_match(result_rmd, "gluey_stateful\\(\"\\{var2\\}\"")

  # Mixed content across lines
  text <- "Line 1: {{var1}}\nLine 2: {{! expr}}\nLine 3: {{var2}} with {{?s}}"
  var1 <- "test1"
  var2 <- 5

  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "Line 1:")
  expect_match(result_rmd, "Line 2:")
  expect_match(result_rmd, "expr")
  expect_match(result_rmd, "Line 3:")
  expect_match(result_rmd, "\\{\\?s\\}")

  # Each line should have its own new_env = TRUE
  expect_equal(length(gregexpr("new_env = TRUE", result_rmd)[[1]]), 3)
})

test_that("Helper functions work correctly", {
  # Environment management tests
  env <- knitr::knit_global()

  # Clean up any existing state
  if (exists("gluey_state", envir = env)) {
    rm("gluey_state", envir = env)
  }

  # Test getting a fresh state
  state1 <- get_gluey_state(TRUE)
  expect_true(exists("gluey_state", envir = env))
  expect_true(is.environment(state1))

  # Test reusing existing state
  state2 <- get_gluey_state(FALSE)
  expect_identical(state1, state2)

  # Test creating a new state
  state3 <- get_gluey_state(TRUE)
  expect_true(is.environment(state3))
  expect_false(identical(state1, state3))

  # Test cleanup
  cleanup_gluey_state()
  expect_false(exists("gluey_state", envir = env))
})

test_that("detect_quarto_document works", {
  # Create a mock Quarto document
  quarto_text <- "---\ntitle: Test\nformat: html\n---\n\nContent"
  expect_true(detect_quarto_document(quarto_text, default = FALSE))

  # Create a mock R Markdown document
  rmd_text <- "---\ntitle: Test\noutput: html_document\n---\n\nContent"
  expect_false(detect_quarto_document(rmd_text, default = TRUE))
})

# New tests for gluey_stateful function
test_that("gluey_stateful maintains state between calls", {
  # Test with basic plurals
  expect_equal(
    paste0(
      gluey_stateful("{2} file", new_env = TRUE),
      gluey_stateful("{?s}")
    ),
    "2 files"
  )

  # Test with is/are forms
  expect_equal(
    paste0(
      gluey_stateful("There ", new_env = TRUE),
      gluey_stateful("{?is/are} "),
      gluey_stateful("{5} "),
      gluey_stateful("item"),
      gluey_stateful("{?s}")
    ),
    "There are 5 items"
  )

  # Test with zero/one/many forms
  expect_equal(
    paste0(
      gluey_stateful("{0} ", new_env = TRUE),
      gluey_stateful("{?no/one/many} "),
      gluey_stateful("cat"),
      gluey_stateful("{?s}")
    ),
    "0 no cats"
  )

  # Test with qty() function
  expect_equal(
    paste0(
      gluey_stateful("{qty(3)}", new_env = TRUE),
      gluey_stateful("There {?is/are} {3} file{?s}")
    ),
    "There are 3 files"
  )

  # Test that new_env resets the state
  result1 <- gluey_stateful("{1} file", new_env = TRUE)
  result2 <- gluey_stateful("{?s}")
  result3 <- gluey_stateful("{2} file", new_env = TRUE)
  result4 <- gluey_stateful("{?s}")

  expect_equal(paste0(result1, result2), "1 file")
  expect_equal(paste0(result3, result4), "2 files")
})

test_that("! format works in gluey_stateful", {
  # Test direct passthrough with ! format
  expect_equal(
    gluey_stateful("{! 2 + 3}"),
    "5"
  )

  # Test with expressions
  current_date <- Sys.Date()
  expect_equal(
    gluey_stateful("Today is {! format(Sys.Date())}"),
    paste0("Today is ", format(current_date))
  )

  # Test combination of ! format with plurals
  expect_equal(
    paste0(
      gluey_stateful("Found {! 2 + 3} ", new_env = TRUE),
      gluey_stateful("item{?s}")
    ),
    "Found 5 items"
  )
})

test_that("! format works in regular gluey", {
  # Test direct passthrough with ! format
  expect_equal(
    gluey("{! 2 + 3}"),
    "5"
  )

  # Test with expressions
  current_date <- Sys.Date()
  expect_equal(
    gluey("Today is {! format(Sys.Date())}"),
    paste0("Today is ", format(current_date))
  )
})

test_that("process_gluey_expressions generates correct code", {
  # Skip if knitr is not available
  skip_if_not_installed("knitr")

  # Mock text with gluey expressions
  text <- "Hello, {{name}}! You have {{n_files}} file{{?s}}."

  # Process for R Markdown
  result_rmd <- process_gluey_expressions(text, environment(), FALSE)
  expect_match(result_rmd, "gluey_stateful", all = TRUE)
  expect_match(result_rmd, "new_env = TRUE", all = FALSE)
  expect_match(result_rmd, "new_env = FALSE", all = FALSE)

  # Process for Quarto
  result_quarto <- process_gluey_expressions(text, environment(), TRUE)
  expect_match(result_quarto, "gluey_stateful", all = TRUE)
  expect_match(result_quarto, "new_env = TRUE", all = FALSE)
  expect_match(result_quarto, "new_env = FALSE", all = FALSE)

  # Check for proper environment reset between lines
  text_multi_line <- "Line 1: {{1}} apple{{?s}}\nLine 2: {{2}} orange{{?s}}"
  result_multi <- process_gluey_expressions(text_multi_line, environment(), FALSE)
  expect_match(result_multi, "new_env = TRUE", all = FALSE)

  # Count occurrences of new_env = TRUE (should be once per line with expressions)
  expect_equal(length(gregexpr("new_env = TRUE", result_multi)[[1]]), 2)
})
