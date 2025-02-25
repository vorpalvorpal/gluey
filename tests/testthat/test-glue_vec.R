# Test suite for glue_vec function

testthat::test_that("glue_vec handles basic string formatting", {
  # Basic string joining
  items <- c("apple", "banana", "cherry")
  testthat::expect_equal(
    glue_vec(items),
    "apple, banana, and cherry"
  )

  # Custom separators
  testthat::expect_equal(
    glue_vec(items, .sep = "; ", .last = " or "),
    "apple; banana or cherry"
  )

  # Custom vector template
  testthat::expect_equal(
    glue_vec(items, .vec = "Items: {.vec}!"),
    "Items: apple, banana, and cherry!"
  )

  # Item template with formatting
  testthat::expect_equal(
    glue_vec(items, .item = "*{.item}*"),
    "*apple*, *banana*, and *cherry*"
  )

  # Apply item transformation
  testthat::expect_equal(
    glue_vec(items, .transformer_item = toupper),
    "APPLE, BANANA, and CHERRY"
  )
})

testthat::test_that("glue_vec handles named vectors", {
  # Named vector with name in template
  named_items <- c(fruit1 = "apple", fruit2 = "banana", fruit3 = "cherry")

  testthat::expect_equal(
    glue_vec(named_items, .item = "[{.name}]: {.item}"),
    "[fruit1]: apple, [fruit2]: banana, and [fruit3]: cherry"
  )

  # Name transformation
  testthat::expect_equal(
    glue_vec(named_items, .item = "{.name}: {.item}", .transformer_name = toupper),
    "FRUIT1: apple, FRUIT2: banana, and FRUIT3: cherry"
  )

  # Both name and item transformations
  testthat::expect_equal(
    glue_vec(named_items, .item = "{.name}: {.item}",
      .transformer_name = toupper, .transformer_item = toupper),
    "FRUIT1: APPLE, FRUIT2: BANANA, and FRUIT3: CHERRY"
  )
})

testthat::test_that("glue_vec creates common markdown formats", {
  items <- c("apple", "banana", "cherry")

  # Unordered list
  testthat::expect_equal(
    glue_vec(items, .item = "- {.item}", .sep = "\n", .last = "\n"),
    "- apple\n- banana\n- cherry"
  )

  # Ordered list
  ordered_items <- c("First item", "Second item", "Third item")
  names(ordered_items) <- 1:3
  testthat::expect_equal(
    glue_vec(ordered_items, .item = "{.name}. {.item}", .sep = "\n", .last = "\n"),
    "1. First item\n2. Second item\n3. Third item"
  )

  # Definition list
  terms <- c(R = "A language for statistical computing",
    Python = "A general-purpose programming language")
  testthat::expect_equal(
    glue_vec(terms, .item = "{.name}\n:    {.item}", .sep = "\n", .last = "\n"),
    "R\n:    A language for statistical computing\nPython\n:    A general-purpose programming language"
  )

  # YAML block
  metadata <- c(title = "Document", author = "Jane Doe")
  testthat::expect_equal(
    glue_vec(metadata, .item = "{.name}: {.item}", .sep = "\n", .last = "\n",
      .vec = "---\n{.vec}\n---"),
    "---\ntitle: Document\nauthor: Jane Doe\n---"
  )

  # Task list
  tasks <- c(x = "Task 1", "Task 2", x = "Task 3")
  testthat::expect_equal(
    glue_vec(tasks, .sep = "\n", .last = "\n",
      .item = "- [{if(.name == '') ' ' else .name}] {.item}"),
    "- [x] Task 1\n- [ ] Task 2\n- [x] Task 3"
  )
})

testthat::test_that("glue_vec handles edge cases", {
  # Empty vector
  testthat::expect_equal(glue_vec(character(0)), "")

  # Single element
  testthat::expect_equal(glue_vec("apple"), "apple")

  # Two elements
  testthat::expect_equal(
    glue_vec(c("apple", "banana")),
    "apple, and banana"
  )

  # Non-character vector
  testthat::expect_equal(
    glue_vec(1:3),
    "1, 2, and 3"
  )

  # Vector with some empty names
  mixed_names <- c("a", b = "b", "c", d = "d")
  testthat::expect_equal(
    glue_vec(mixed_names),
    "a, b, c, and d"
  )
})

testthat::test_that("glue_vec handles width parameter", {
  long_items <- c(
    "This is a very long item that should wrap at some point",
    "Another lengthy item that goes beyond the width",
    "A third item to demonstrate width handling"
  )

  # With width parameter
  result_with_width <- glue_vec(long_items, .width = 40)
  testthat::expect_true(nchar(result_with_width) > 0)

  # Lines shouldn't exceed width + length of last separator
  lines <- strsplit(result_with_width, "\n")[[1]]
  for (i in seq_along(lines)) {
    if (i < length(lines)) {
      testthat::expect_lte(nchar(lines[i]), 40)
    }
  }
})

testthat::test_that("glue_vec passes arguments to glue", {
  # Using .open and .close parameters
  items <- c("apple", "banana", "cherry")
  testthat::expect_equal(
    glue_vec(items, .item = "<.item>", .vec = "<.vec>", .open = "<", .close = ">"),
    "apple, banana, and cherry"
  )

  # Using .trim parameter
  testthat::expect_equal(
    glue_vec(items, .item = "{    .item     }", .vec = "{    .vec     }", .trim = TRUE),
    "apple, banana, and cherry"
  )
})

testthat::test_that("glue_vec validates inputs correctly", {
  # Expect cli errors with appropriate messages
  testthat::expect_error(
    glue_vec(environment()),
    "must be a vector"
  )

  testthat::expect_error(
    glue_vec(c("a", "b"), .sep = 1),
    "must be a string"
  )

  testthat::expect_error(
    glue_vec(c("a", "b"), .transformer_item = "not a function"),
    "must be a function"
  )

  testthat::expect_error(
    glue_vec(c("a", "b"), .width = "not an integer"),
    "must be an integer"
  )
})
