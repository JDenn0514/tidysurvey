testthat::test_that("check an individual vector", {
  testthat::expect_equal(
    attr_val_labels(test_data$top),
    attr(test_data$top, "labels")
  )
})

testthat::test_that("check an individual vector, with data and x separate", {
  testthat::expect_equal(
    attr_val_labels("top", test_data),
    attr(test_data$top, "labels")
  )
})

testthat::test_that("check an individual vector, with data and x separate", {
  # make a haven labelled vector
  out <- structure(
    c(1:4),
    labels = stats::setNames(
      c(1:4),
      c(
        "Strongly agree",
        "Somewhat agree",
        "Somewhat disagree",
        "Strongly disagree"
      )
    ),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )
  exp <- c(
    "Strongly agree" = 1,
    "Somewhat agree" = 2,
    "Somewhat disagree" = 3,
    "Strongly disagree" = 4
  )

  testthat::expect_equal(attr_val_labels(out), exp)
})

testthat::test_that("check an individual vector, with data and x separate", {
  # make a normal labelled vector
  out <- structure(
    c(1:4),
    labels = stats::setNames(
      c(1:4),
      c(
        "Strongly agree",
        "Somewhat agree",
        "Somewhat disagree",
        "Strongly disagree"
      )
    )
  )
  exp <- c(
    "Strongly agree" = 1,
    "Somewhat agree" = 2,
    "Somewhat disagree" = 3,
    "Strongly disagree" = 4
  )

  testthat::expect_equal(attr_val_labels(out), exp)
})

testthat::test_that("check a dataset", {
  exp <- purrr::map(
    colnames(test_data),
    ~ attr_val_labels({{ .x }}, test_data)
  ) |>
    stats::setNames(colnames(test_data))

  testthat::expect_equal(
    attr_val_labels(test_data),
    exp
  )
})
