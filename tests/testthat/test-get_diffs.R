# tests/testthat/test-get_diffs.R

testthat::test_that("get_diffs.default computes differences without groups", {
  df <- make_expanded_df() |> label_vars()
  out <- get_diffs(
    data = df,
    x = satisfaction_price,
    treats = grp,
    group = x2,
    wt = wts,
    ref_level = "A",
    show_means = FALSE
  )

  testthat::expect_s3_class(out, "tidysurvey_mean_diffs")
  testthat::expect_true("diffs" %in% names(out))
  testthat::expect_true("conf_low" %in% names(out))
  testthat::expect_true("conf_high" %in% names(out))
  testthat::expect_true(all(out$grp %in% c("B", "C")))
  testthat::expect_equal(
    attr(out$diffs, "label"),
    "Difference in means relative to A"
  )
})

testthat::test_that("get_diffs.default computes differences without groups", {
  df <- make_expanded_df()

  out <- get_diffs(
    data = df,
    x = satisfaction_price,
    treats = grp,
    group = x2,
    wt = wts,
    ref_level = "A",
    show_means = FALSE
  )

  testthat::expect_s3_class(out, "tidysurvey_mean_diffs")
  testthat::expect_true("diffs" %in% names(out))
  testthat::expect_true("conf_low" %in% names(out))
  testthat::expect_true("conf_high" %in% names(out))
  testthat::expect_true(all(out$grp %in% c("B", "C")))
  testthat::expect_equal(
    attr(out$diffs, "label"),
    "Difference in means relative to A"
  )
})

testthat::test_that("get_diffs.default works with show_means and show_pct_change", {
  df <- make_expanded_df()
  out <- get_diffs(
    data = df,
    x = income,
    treats = grp,
    wt = wts,
    ref_level = "A",
    show_means = TRUE,
    show_pct_change = TRUE
  )

  testthat::expect_true("mean" %in% names(out))
  testthat::expect_true("pct_change" %in% names(out))
  testthat::expect_type(out$mean, "double")
  testthat::expect_type(out$pct_change, "double")
})

testthat::test_that("get_diffs.default works without show_means but with show_pct_change", {
  df <- make_expanded_df()
  out <- get_diffs(
    data = df,
    x = income,
    treats = grp,
    wt = wts,
    ref_level = "A",
    show_means = FALSE,
    show_pct_change = TRUE
  )

  testthat::expect_false("mean" %in% names(out))
  testthat::expect_true("pct_change" %in% names(out))
  testthat::expect_type(out$pct_change, "double")
})

testthat::test_that("get_diffs.default respects grouping variables", {
  df <- make_expanded_df()
  out <- df |>
    dplyr::group_by(x1) |>
    get_diffs(
      x = income,
      treats = grp,
      wt = wts,
    )

  testthat::expect_true("grp" %in% names(out))
  testthat::expect_true(all(out$grp %in% c("A", "B", "C")))
  testthat::expect_equal(attr(out, "group_names"), "x1")
})

testthat::test_that("get_diffs.default works when resetting the reference level", {
  df <- make_expanded_df()
  out <- df |>
    dplyr::group_by(x1) |>
    get_diffs(
      x = income,
      treats = grp,
      wt = wts,
      ref_level = "B"
    )

  ref_level <- attr(out, "ref_level")

  testthat::expect_equal(ref_level, "Reference level is B")
  testthat::expect_true("grp" %in% names(out))
  testthat::expect_true(all(out$grp %in% c("B", "A", "C")))
  testthat::expect_equal(attr(out, "group_names"), "x1")
})

testthat::test_that("get_diffs.default errors on non-numeric outcome", {
  df <- make_expanded_df()
  testthat::expect_error(
    get_diffs(
      data = df,
      x = grp, # not numeric
      treats = strata,
      wt = wts,
      ref_level = "1"
    ),
    "must be of class `numeric`"
  )
})

testthat::test_that("get_diffs.survey.design computes differences without groups", {
  df <- make_expanded_df()
  des <- survey::svydesign(
    id = ~psu,
    strata = ~strata,
    weights = ~wts,
    fpc = ~fpc_psu,
    data = df,
    nest = TRUE
  )

  out <- get_diffs(
    data = des,
    x = income,
    treats = grp,
    ref_level = "A"
  )

  testthat::expect_s3_class(out, "tidysurvey_mean_diffs")
  testthat::expect_true("diffs" %in% names(out))
  testthat::expect_true(all(out$grp %in% c("A", "B", "C")))
})

testthat::test_that("get_diffs.survey.design computes differences with groups", {
  df <- make_expanded_df()
  des <- survey::svydesign(
    id = ~psu,
    strata = ~strata,
    weights = ~wts,
    fpc = ~fpc_psu,
    data = df,
    nest = TRUE
  )

  out <- get_diffs(
    data = des,
    x = income,
    treats = grp,
    group = strata2,
    ref_level = "A"
  )

  testthat::expect_true("strata2" %in% names(out))
  testthat::expect_equal(attr(out, "group_names"), "strata2")
})

testthat::test_that("get_diffs.survey.design supports show_means and pct_change", {
  df <- make_expanded_df()
  des <- survey::svydesign(
    id = ~psu,
    strata = ~strata,
    weights = ~wts,
    fpc = ~fpc_psu,
    data = df,
    nest = TRUE
  )

  out <- get_diffs(
    data = des,
    x = income,
    treats = grp,
    ref_level = "A",
    show_means = TRUE,
    show_pct_change = TRUE
  )

  testthat::expect_true("mean" %in% names(out))
  testthat::expect_true("pct_change" %in% names(out))
  testthat::expect_type(out$mean, "double")
})

testthat::test_that("get_diffs.survey.design works when resetting the reference level", {
  df <- make_expanded_df()
  des <- survey::svydesign(
    id = ~psu,
    strata = ~strata,
    weights = ~wts,
    fpc = ~fpc_psu,
    data = df,
    nest = TRUE
  )

  out <- get_diffs(
    data = des,
    x = income,
    treats = grp,
    ref_level = "B"
  )

  ref_level <- attr(out, "ref_level")

  testthat::expect_equal(ref_level, "Reference level is B")
  testthat::expect_true("grp" %in% names(out))
  testthat::expect_true(all(out$grp %in% c("B", "A", "C")))
})

testthat::test_that("get_diffs.svyrep.design computes differences without groups", {
  df <- make_expanded_df() |> label_vars()
  rep_weights <- make_rep_weights(n = nrow(df), nrep = 5)
  df <- dplyr::bind_cols(df, as.data.frame(rep_weights))

  des <- srvyr::as_survey_rep(
    df,
    repweights = dplyr::starts_with("rep_"),
    type = "BRR",
    weights = wts
  )

  out <- get_diffs(
    data = des,
    x = income,
    treats = grp,
    ref_level = "A"
  )

  testthat::expect_s3_class(out, "tidysurvey_mean_diffs")
  testthat::expect_true("diffs" %in% names(out))
  testthat::expect_true(all(out$grp %in% c("A", "B", "C")))
})

testthat::test_that("get_diffs.svyrep.design computes differences with groups", {
  df <- make_expanded_df() |> label_vars()
  rep_weights <- make_rep_weights(n = nrow(df), nrep = 5)
  df <- dplyr::bind_cols(df, as.data.frame(rep_weights))

  des <- srvyr::as_survey_rep(
    df,
    repweights = dplyr::starts_with("rep_"),
    type = "BRR",
    weights = wts
  )

  out <- get_diffs(
    data = des,
    x = income,
    treats = grp,
    group = strata2,
    ref_level = "A"
  )

  testthat::expect_true("strata2" %in% names(out))
  testthat::expect_equal(attr(out, "group_names"), "strata2")
})

testthat::test_that("get_diffs.svyrep.design supports show_means and pct_change", {
  df <- make_expanded_df() |> label_vars()
  rep_weights <- make_rep_weights(n = nrow(df), nrep = 5)
  df <- dplyr::bind_cols(df, as.data.frame(rep_weights))
  df <- label_vars(df)

  des <- srvyr::as_survey_rep(
    df,
    repweights = dplyr::starts_with("rep_"),
    type = "BRR",
    weights = wts
  )

  out <- get_diffs(
    data = des,
    x = income,
    treats = grp,
    ref_level = "A",
    show_means = TRUE,
    show_pct_change = TRUE
  )

  testthat::expect_true("mean" %in% names(out))
  testthat::expect_true("pct_change" %in% names(out))
  testthat::expect_type(out$mean, "double")
})
