testthat::test_that("gets the names", {
  df <- label_vars(make_basic_df())

  names <- get_col_names(df)
  testthat::expect_equal(
    names,
    c("id", "grp", "x1", "x2", "wts")
  )
})
