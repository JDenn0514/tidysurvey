testthat::test_that("check an individual vector", {
  df <- make_basic_df()
  testthat::expect_equal(
    attr_var_label(df$x1),
    attr(df$x1, "label", exact = TRUE)
  )
})

testthat::test_that("check an a dataset, unlist it", {
  df <- make_basic_df()
  exp <- purrr::map(
    colnames(df),
    \(x) attr(df[[x]], "label")
  ) |>
    stats::setNames(colnames(df)) |>
    unlist()

  testthat::expect_equal(
    attr_var_label(df),
    exp
  )
})

testthat::test_that("check an a dataset, don't unlist it", {
  df <- make_basic_df()
  exp <- purrr::map(
    colnames(df),
    \(x) attr(df[[x]], "label")
  ) |>
    stats::setNames(colnames(df))

  testthat::expect_equal(
    attr_var_label(df, unlist = FALSE),
    exp
  )
})

testthat::test_that("check a vector without a label and use NA", {
  top <- sample(c(1:2), 5, replace = TRUE)

  testthat::expect_equal(
    attr_var_label(top, if_null = "NA"),
    NA
  )
})

testthat::test_that("check a vector without a label and use the anme", {
  top <- sample(c(1:2), 5, replace = TRUE)

  testthat::expect_equal(
    attr_var_label(top, if_null = "name"),
    `top`
  )
})

testthat::test_that("check a dataset where some variables have labels and some don't", {
  x <- sample(c(1:2), 5, replace = TRUE)
  attr(x, "label") <- "Variable label X"
  y <- sample(c(1:2), 5, replace = TRUE)
  z <- sample(c(1:2), 5, replace = TRUE)
  attr(z, "label") <- "Variable label Z"

  df <- data.frame(x, y, z)

  # check with NAs
  testthat::expect_equal(
    attr_var_label(df, if_null = "NA"),
    c(x = "Variable label X", y = NA, z = "Variable label Z")
  )

  # check with names
  testthat::expect_equal(
    attr_var_label(df, if_null = "name"),
    c(x = "Variable label X", y = "y", z = "Variable label Z")
  )

  # don't unlist the data and use NAs
  testthat::expect_equal(
    attr_var_label(df, unlist = FALSE, if_null = "NA"),
    list(x = "Variable label X", y = NA, z = "Variable label Z")
  )

  # don't unlist the data and use names
  testthat::expect_equal(
    attr_var_label(df, unlist = FALSE, if_null = "name"),
    list(x = "Variable label X", y = "y", z = "Variable label Z")
  )
})
