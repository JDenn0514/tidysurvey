# -----------------------------
# data.frame tests
# -----------------------------
testthat::test_that("make_nested() works for data.frame", {
  df <- make_expanded_df() |> label_vars()

  out <- make_nested(df, c("grp", "strata"))

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true("data" %in% names(out))
  testthat::expect_true("name" %in% names(out))

  expected_n_groups <- dplyr::n_distinct(df$grp, df$strata)
  testthat::expect_equal(nrow(out), expected_n_groups)

  inner_cols <- names(out$data[[1]])
  expected_inner_cols <- setdiff(names(df), c("grp", "strata"))
  testthat::expect_equal(sort(inner_cols), sort(expected_inner_cols))

  first_name <- out$name[1]
  first_levels <- out[1, c("grp", "strata")]
  testthat::expect_equal(first_name, paste(first_levels, collapse = "_"))
})

# -----------------------------
# survey.design tests
# -----------------------------
testthat::test_that("make_nested() works for survey.design", {
  df <- make_expanded_df() |> label_vars()
  design <- survey::svydesign(
    id = ~id,
    strata = ~strata,
    weights = ~wts,
    data = df,
    nest = TRUE
  )

  svy <- srvyr::as_survey_design(
    id = id,
    strata = strata,
    weights = wts,
    .data = df,
    nest = TRUE
  )

  out <- make_nested(svy, c("grp"))

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true("data" %in% names(out))
  testthat::expect_true("name" %in% names(out))
  testthat::expect_true(inherits(out$data[[1]], "survey.design"))

  inner_design <- out$data[[1]]

  # Compare weights to original wts column
  testthat::expect_equal(
    unname(1 / inner_design$prob),
    inner_design$variables$wts
  )
})

testthat::test_that("make_nested() correctly nests survey objects", {
  df <- make_expanded_df() |> label_vars()

  # Create a srvyr survey design
  svy <- srvyr::as_survey_design(
    .data = df,
    id = id,
    strata = strata,
    weights = wts,
    nest = TRUE
  )

  # Nest by single grouping variable
  out <- make_nested(svy, c("grp"))

  # Basic structure
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("data", "name") %in% names(out)))

  # All nested elements are survey.design objects
  testthat::expect_true(all(vapply(
    out$data,
    inherits,
    logical(1),
    "survey.design"
  )))

  # Check group names match unique groups
  testthat::expect_equal(sort(out$name), sort(unique(df$grp)))

  # Check row counts within each nested survey match original data
  for (i in seq_len(nrow(out))) {
    inner <- out$data[[i]]
    grp_name <- out$name[i]

    # Filter original df to this group
    df_grp <- df[df$grp == grp_name, ]

    testthat::expect_equal(nrow(inner$variables), nrow(df_grp))

    # Check weights are correct
    testthat::expect_equal(inner$variables$wts, df_grp$wts)

    # Check strata are preserved
    if (!is.null(inner$strata)) {
      testthat::expect_equal(inner$variables$strata, df_grp$strata)
    }

    # Check ids match
    if (!is.null(inner$cluster)) {
      testthat::expect_equal(inner$variables$id, df_grp$id)
    }
  }

  # Test with multiple grouping variables
  out2 <- make_nested(svy, c("grp", "strata2"))
  testthat::expect_s3_class(out2, "tbl_df")
  testthat::expect_true(all(vapply(
    out2$data,
    inherits,
    logical(1),
    "survey.design"
  )))

  # Names should be combinations of groups
  expected_names <- with(df, paste(grp, strata2, sep = "_"))
  testthat::expect_equal(sort(out2$name), sort(unique(expected_names)))
})

testthat::test_that("make_nested() correctly nests survey objects with nesting", {
  df <- make_expanded_df() |> label_vars()

  # Create a srvyr survey design
  svy <- srvyr::as_survey_design(
    .data = df,
    id = id,
    strata = strata,
    weights = wts,
    nest = TRUE
  )

  # Nest by single grouping variable
  out <- make_nested(svy, c("grp"))

  # Basic structure
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("data", "name") %in% names(out)))

  # All nested elements are survey.design objects
  testthat::expect_true(all(vapply(
    out$data,
    inherits,
    logical(1),
    "survey.design"
  )))

  # Check group names match unique groups
  testthat::expect_equal(sort(out$name), sort(unique(df$grp)))

  # Check row counts, weights, strata, and ids within each nested survey
  for (i in seq_len(nrow(out))) {
    inner <- out$data[[i]]
    df_grp <- df[df$grp == out$name[i], ]

    # row counts
    testthat::expect_equal(nrow(inner$variables), nrow(df_grp))

    # weights
    testthat::expect_equal(inner$variables$wts, df_grp$wts)

    # strata
    if (!is.null(inner$strata)) {
      testthat::expect_equal(inner$variables$strata, df_grp$strata)
    }

    # clusters
    if (!is.null(inner$cluster)) {
      testthat::expect_equal(inner$variables$id, df_grp$id)

      cluster_counts <- table(inner$variables$strata, inner$variables$id)
      # No cluster appears twice in same strata if nest=TRUE
      testthat::expect_true(all(cluster_counts <= 1))
    }
  }

  # check that a summary statistic works without error (exactly what I'll be doing)
  for (i in seq_len(nrow(out))) {
    inner <- out$data[[i]]

    # Example: mean of x3 should run
    testthat::expect_silent(survey::svymean(~x3, design = inner, na.rm = TRUE))

    # Example: proportion of x1
    testthat::expect_silent(survey::svymean(~x1, design = inner, na.rm = TRUE))
  }

  # Test with multiple grouping variables
  out2 <- make_nested(svy, c("grp", "strata2"))
  testthat::expect_s3_class(out2, "tbl_df")
  testthat::expect_true(all(vapply(
    out2$data,
    inherits,
    logical(1),
    "survey.design"
  )))

  # Names should be combinations of groups
  expected_names <- with(df, paste(grp, strata2, sep = "_"))
  testthat::expect_equal(sort(out2$name), sort(unique(expected_names)))

  # Check row counts
  for (i in seq_len(nrow(out2))) {
    inner <- out2$data[[i]]
    grp_str <- strsplit(out2$name[i], "_")[[1]]
    df_grp <- df[df$grp == grp_str[1] & df$strata2 == grp_str[2], ]

    testthat::expect_equal(nrow(inner$variables), nrow(df_grp))
  }
})


# -----------------------------
# svyrep.design tests
# -----------------------------
testthat::test_that("make_nested() works for svyrep.design", {
  df <- make_expanded_df() |> label_vars()
  rep_weights <- make_rep_weights(n = nrow(df), nrep = 5)
  df <- dplyr::bind_cols(df, as.data.frame(rep_weights))

  svy_rep <- srvyr::as_survey_rep(
    df,
    repweights = dplyr::starts_with("rep_"),
    type = "BRR",
    weights = wts
  )

  out <- make_nested(svy_rep, c("grp", "strata"))

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true("data" %in% names(out))
  testthat::expect_true("name" %in% names(out))
  testthat::expect_true(inherits(out$data[[1]], "svyrep.design"))

  inner_rw <- out$data[[1]]$repweights
  testthat::expect_equal(ncol(inner_rw), ncol(rep_weights))

  # Check number of rows in each inner survey matches original grouping
  first_group_name <- out$name[1]
  first_inner <- out$data[[1]]$variables
  expected_rows <- nrow(dplyr::filter(
    df,
    paste(grp, strata, sep = "_") == first_group_name
  ))
  testthat::expect_equal(nrow(first_inner), expected_rows)

  # Check weights preserved
  # testthat::expect_equal(1 / out$data[[1]]$prob, out$data[[1]]$variables$wts)
  testthat::expect_equal(
    out$data[[1]]$variables$wts,
    dplyr::filter(df, paste(grp, strata, sep = "_") == first_group_name)$wts
  )
})


# -----------------------------
# Edge cases: NA handling
# -----------------------------
testthat::test_that("na.rm = FALSE preserves NA rows in data.frame", {
  df <- make_expanded_df()
  df$grp[1:3] <- NA # inject NAs

  out <- make_nested(df, c("grp"), na.rm = FALSE)
  testthat::expect_true(any(is.na(out$grp)))
})

testthat::test_that("na.rm = TRUE removes rows with NA in data.frame", {
  df <- make_expanded_df()
  df$grp[1:3] <- NA # inject NAs

  out <- make_nested(df, c("grp"), na.rm = TRUE)
  testthat::expect_false(any(is.na(out$grp)))
})

# -----------------------------
# Edge case: no grouping variable
# -----------------------------
testthat::test_that("make_nested() errors if no grouping variable supplied", {
  df <- make_expanded_df()
  testthat::expect_error(
    make_nested(df, c()),
    "No grouping variables were provided"
  )
})

# -----------------------------
# Edge case: sorting check
# -----------------------------
testthat::test_that("make_nested() output is sorted by grouping variables", {
  df <- make_expanded_df() |> label_vars()

  # Shuffle rows to ensure original order is random
  df <- dplyr::slice_sample(df, n = nrow(df))

  out <- make_nested(df, c("grp", "strata"))

  group_cols <- dplyr::select(out, grp, strata)
  expected_order <- df %>%
    dplyr::arrange(grp, strata) %>%
    dplyr::distinct(grp, strata)

  testthat::expect_equal(group_cols, expected_order)
})
