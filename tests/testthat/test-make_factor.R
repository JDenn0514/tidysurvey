# Labelled values ---------------------------------------------------------

#### NUMERIC VECTORS
testthat::test_that("all labels are preserved if drop_levels is FALSE", {
  s1 <- haven::labelled(
    rep(1, 3),
    c("A" = 1, "B" = 2, "C" = 3),
    label = "Variable label"
  )
  exp <- factor(rep("A", 3), levels = c("A", "B", "C")) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Variable label"
    )
  testthat::expect_equal(make_factor(s1, drop_levels = FALSE), exp)
})

testthat::test_that("Only labels in data are preserved if drop_levels is TRUE", {
  s1 <- haven::labelled(
    rep(1, 3),
    c("A" = 1, "B" = 2, "C" = 3),
    label = "Variable label"
  )
  exp <- factor(rep("A", 3), levels = c("A")) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Variable label"
    )
  testthat::expect_equal(make_factor(s1, drop_levels = TRUE), exp)
})


testthat::test_that("all labels are preserved when using a non-haven_labelled numeric vector", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- structure(c(0, 1, 0, 1), labels = stats::setNames(c(0, 1), c("A", "B")))
  exp <- factor(c("A", "B", "A", "B"), levels = c("A", "B")) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("all labels are preserved when using a non-haven_labelled numeric vector", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- c(0, 1, 0, 1)
  attr(s1, "labels") <- c(`0` = "A", `1` = "B")
  # s1 <- structure(c(0, 1, 0, 1), labels = stats::setNames(c(0, 1), c("A", "B")))
  exp <- factor(c("A", "B", "A", "B"), levels = c("A", "B")) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("Convert NAs using labels", {
  s1 <- haven::labelled(
    c(0, 1, haven::tagged_na("c")),
    c("A" = 0, "B" = 1, "C" = haven::tagged_na("c"))
  )
  exp <- factor(c("A", "B", "C"), levels = c("A", "B", "C")) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("Keep NAs as NAs", {
  s1 <- haven::labelled(
    c(0, 1, haven::tagged_na("c")),
    c("A" = 0, "B" = 1, "C" = haven::tagged_na("c"))
  )
  exp <- factor(c("A", "B", NA), levels = c("A", "B", NA)) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  testthat::expect_equal(make_factor(s1, na.rm = TRUE), exp)
})

#### CHARACTER VECTORS WITH VALUE LABELS ------------------------
testthat::test_that("character labelled converts to factor preserves all attributes", {
  s1 <- haven::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"), "Gender")
  exp <- factor(c("Male", "Male", "Female"), levels = c("Male", "Female")) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Gender"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("character labelled converts to factor and uses name as label attribute", {
  s1 <- haven::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
  exp <- factor(c("Male", "Male", "Female"), levels = c("Male", "Female")) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("all labels are preserved when using a non-haven_labelled numeric vector and uses name as label attribute", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- structure(
    c("A", "B", "A", "B"),
    labels = stats::setNames(c("A", "B"), c("Letter A", "Letter B"))
  )
  exp <- factor(
    c("Letter A", "Letter B", "Letter A", "Letter B"),
    levels = c("Letter A", "Letter B")
  ) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("all labels are preserved when using a non-haven_labelled numeric vector and uses name as label attribute", {
  #values <- stats::setNames(c(0, 1), c("A", "B"))
  s1 <- structure(
    c("A", "B", "A", "B"),
    labels = stats::setNames(c("A", "B"), c("Letter A", "Letter B")),
    label = "Alphabet"
  )
  exp <- factor(
    c("Letter A", "Letter B", "Letter A", "Letter B"),
    levels = c("Letter A", "Letter B")
  ) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "Alphabet"
    )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("converts tagged NAs", {
  s1 <- haven::labelled(
    c(1:2, haven::tagged_na("a")),
    c("Orange" = 1, "Blue" = 2, "Apple" = haven::tagged_na("a")),
  )
  exp <- factor(
    c("Orange", "Blue", "Apple"),
    levels = c("Orange", "Blue", "Apple")
  ) |>
    structure(
      transformation = "Converted 's1' into a factor based on its value labels",
      label = "s1"
    )
  expect_equal(make_factor(s1), exp)
})

# Without labels --------------------------------------------------------------

#### CHARACTER VECTORS WITHOUT VALUE LABELS

testthat::test_that("convert to factor, add transformation and label attributes", {
  s1 <- letters
  exp <- structure(
    factor(letters),
    transformation = "Updated 's1' from a character vector to a factor",
    label = "s1"
  )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("convert to factor, add transformation and keep label attributes", {
  s1 <- letters |> structure(label = "Alphabet")
  exp <- structure(
    factor(letters),
    transformation = "Updated 's1' from a character vector to a factor",
    label = "Alphabet"
  )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("should force s1 to a factor", {
  s1 <- c(0, 1, 2)
  testthat::expect_warning(make_factor(s1))

  exp <- as.factor(c(0, 1, 2)) |>
    structure(
      label = "s1",
      transformation = "Converted 's1' from a numeric vector to a factor"
    )
  testthat::expect_equal(suppressWarnings(make_factor(s1)), exp)
})


testthat::test_that("should force s1 to a factor", {
  s1 <- c(0, 1, 2)
  attr(s1, "label") <- "Label"

  exp <- as.factor(c(0, 1, 2)) |>
    structure(
      label = "Label",
      transformation = "Converted 's1' from a numeric vector to a factor"
    )
  testthat::expect_equal(suppressWarnings(make_factor(s1)), exp)
})

#### FACTOR VECTORS

testthat::test_that("add label attribute to factor without one", {
  s1 <- factor(letters, levels = letters)
  exp <- structure(
    factor(letters, levels = letters),
    label = "s1"
  )
  testthat::expect_equal(make_factor(s1), exp)
})

testthat::test_that("Check a vector from a data set", {
  x <- c(1, 2, 2, 1) |>
    structure(labels = c("Yes" = 1, "No" = 2))
  y <- factor(c("Yes", "No", "No", "Yes"), levels = c("Yes", "No")) |>
    structure(
      transformation = "Converted 'df[[\"x\"]]' into a factor based on its value labels",
      label = 'df[[\"x\"]]'
    )
  df <- data.frame(x, y)

  testthat::expect_equal(make_factor(df[["x"]], drop_levels = TRUE), df$y)
})


# Check errors ----------------------------------------

# works
testthat::test_that("error when some values are missing labels - numeric vector", {
  s1 <- haven::labelled(c(1, 4), c("Agree" = 1))
  # New main message:
  # "Each value in `x` must have value labels."
  # Plus bullets for unlabeled values and domain summary.
  testthat::expect_error(
    make_factor(s1),
    regexp = "Each value in `s1` must have value labels\\."
  )
  # Optionally also check the bullet about unlabeled values:
  testthat::expect_error(
    make_factor(s1),
    regexp = "Unlabeled values detected:"
  )
  # And the info bullet about known domains:
  testthat::expect_error(
    make_factor(s1),
    regexp = "Known codes: \\[|labels: \\["
  )
})

testthat::test_that("error when some values are missing labels - character vector", {
  s1 <- haven::labelled(c("A", "B"), c("Agree" = "A"))
  testthat::expect_error(
    make_factor(s1),
    regexp = "Each value in `s1` must have value labels\\."
  )
  testthat::expect_error(
    make_factor(s1),
    regexp = "Unlabeled values detected:"
  )
  testthat::expect_error(
    make_factor(s1),
    regexp = "Known codes: \\[|labels: \\["
  )
})

testthat::test_that("error when numeric and force is FALSE", {
  s1 <- c(1, 2, 3, 4)
  # Exact message from cli_abort call:
  # "The vector provided in `x` does not have value labels."
  testthat::expect_error(
    make_factor(s1, force = FALSE),
    regexp = "The vector provided in `s1` does not have value labels\\."
  )
})
