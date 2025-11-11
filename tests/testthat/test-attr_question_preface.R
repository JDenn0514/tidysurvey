testthat::test_that("attr_question_preface.default works with a vector", {
  df <- make_basic_df() |> label_vars()

  # Extract one variable
  x1 <- df$x1

  # Expect correct preface
  testthat::expect_equal(
    attr_question_preface(x1),
    "Please indicate which of the following colors you like. Select all that apply:"
  )
})

testthat::test_that("attr_question_preface.default works with data + variable name", {
  df <- make_basic_df() |> label_vars()

  # Provide variable name + data
  testthat::expect_equal(
    attr_question_preface("x2", data = df),
    "Please indicate which of the following colors you like. Select all that apply:"
  )
})

testthat::test_that("attr_question_preface.data.frame returns named list of prefaces", {
  df <- make_basic_df() |> label_vars()

  result <- attr_question_preface(df)

  # Only x1 and x2 have prefaces
  testthat::expect_type(result, "list")
  testthat::expect_named(result, names(df))

  testthat::expect_equal(
    result$x1,
    "Please indicate which of the following colors you like. Select all that apply:"
  )
  testthat::expect_equal(
    result$x2,
    "Please indicate which of the following colors you like. Select all that apply:"
  )

  # Variables without prefaces should return NULL
  testthat::expect_null(result$id)
  testthat::expect_null(result$grp)
  testthat::expect_null(result$wts)
})
