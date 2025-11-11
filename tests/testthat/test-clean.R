testthat::test_that("clean() works for simple lm models", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ age + grp, data = df)

  res <- clean(model)

  # Basic structure
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(all(
    c("term", "estimate", "conf_low", "conf_high", "n") %in% names(res)
  ))

  # Terms should include intercept, age, and grp levels
  testthat::expect_true("(Intercept)" %in% res$term)
  testthat::expect_true(any(grepl("^grp", res$term)))

  # Confidence intervals should make sense
  testthat::expect_true(all(res$conf_low <= res$conf_high, na.rm = TRUE))

  # n should be non-missing and positive
  testthat::expect_true(all(res$n > 0, na.rm = TRUE))
})


testthat::test_that("clean() includes reference levels correctly", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp + x1, data = df)

  res <- clean(model, include_reference = TRUE)

  # Reference levels should appear in results
  ref_terms <- grep("A$|yes$", res$term, value = TRUE)
  testthat::expect_true(length(ref_terms) > 0)

  # Reference levels should have estimate = 0 and NA for SE
  ref_rows <- dplyr::filter(res, estimate == 0)
  testthat::expect_true(all(is.na(ref_rows$conf_high)))

  # Reference levels should appear before other levels of the same variable
  grp_rows <- grep("^grp", res$term, value = TRUE)
  testthat::expect_true(ref_rows$term[1] == grp_rows[1])
})


testthat::test_that("clean() produces correct sample sizes per term", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ x1 + x3, data = df)

  res <- clean(model)

  # The intercept should have total n
  testthat::expect_equal(unname(res$n[res$term == "(Intercept)"]), nrow(df))

  # Binary factors should have n equal to sum of '1's
  term_name <- grep("^x1", res$term, value = TRUE)[1]
  mm <- stats::model.matrix(model)
  testthat::expect_equal(
    unname(res$n[res$term == term_name]),
    sum(mm[, term_name])
  )
})


testthat::test_that("clean() handles glm models", {
  df <- make_expanded_df()
  df$outcome <- as.integer(df$x4) # convert to numeric binary
  model <- stats::glm(
    outcome ~ age + grp,
    data = df,
    family = stats::binomial()
  )

  res <- clean(model, conf_method = "wald")
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(all(
    c("term", "estimate", "conf_low", "conf_high", "n") %in% names(res)
  ))
  testthat::expect_true(all(is.finite(res$estimate)))
})


testthat::test_that("clean() handles svyglm models", {
  testthat::skip_if_not_installed("survey")

  df <- make_expanded_df()
  design <- survey::svydesign(
    ids = ~1,
    strata = ~strata,
    weights = ~wts,
    # fpc = ~ fpc_psu + fpc_ssu,
    data = df,
    nest = TRUE
  )

  model <- survey::svyglm(rating_overall ~ age + grp, design = design)
  res <- clean(model)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(all(
    c("term", "estimate", "conf_low", "conf_high", "n") %in% names(res)
  ))
  testthat::expect_true("(Intercept)" %in% res$term)
})


testthat::test_that("clean() reorders terms correctly when reference levels included", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp + x1, data = df)

  res <- clean(model, include_reference = TRUE)
  grp_terms <- grep("^grp", res$term, value = TRUE)

  # Reference (grpA) should appear before grpB, grpC
  testthat::expect_match(grp_terms[1], "grpA")
})


testthat::test_that("clean() handles missing data gracefully", {
  df <- make_expanded_df()
  df$x1[sample(1:nrow(df), 10)] <- NA
  model <- stats::lm(rating_overall ~ x1 + age, data = df)

  res <- clean(model)
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(!any(is.nan(res$estimate)))
})


testthat::test_that("clean() orders terms correctly with reference levels", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp + x1, data = df)

  result <- clean(model, include_reference = TRUE)

  # Check order: Intercept, then grpA, grpB, grpC, then x1no, x1yes
  testthat::expect_equal(result$term[1], "(Intercept)")
  testthat::expect_equal(result$term[2], "grpA")
  testthat::expect_equal(result$term[3], "grpB")
  testthat::expect_equal(result$term[4], "grpC")
  testthat::expect_equal(result$term[5], "x1no")
  testthat::expect_equal(result$term[6], "x1yes")
})

testthat::test_that("clean() handles categorical × categorical interactions", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp * x1, data = df)

  result <- clean(model, include_reference = TRUE)

  # Should include main effects and interactions
  testthat::expect_true("grpA" %in% result$term)
  testthat::expect_true("grpB" %in% result$term)
  testthat::expect_true("x1no" %in% result$term)
  testthat::expect_true("x1yes" %in% result$term)

  # Check for interaction terms (reference combinations should be included)
  interaction_terms <- result$term[grepl(":", result$term)]
  testthat::expect_true(length(interaction_terms) > 0)

  # Reference interaction should have estimate of 0
  ref_interaction <- result$term[grepl("grpA:x1no", result$term)]
  if (length(ref_interaction) > 0) {
    testthat::expect_equal(result$estimate[result$term == ref_interaction], 0)
  }
})

testthat::test_that("clean() handles numeric × categorical interactions", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ age * grp, data = df)

  result <- clean(model, include_reference = TRUE)

  # Should include main effects
  testthat::expect_true("age" %in% result$term)
  testthat::expect_true("grpA" %in% result$term)
  testthat::expect_true("grpB" %in% result$term)
  testthat::expect_true("grpC" %in% result$term)

  # Should include interaction terms
  testthat::expect_true(any(
    grepl("age.*grp", result$term) | grepl("grp.*age", result$term)
  ))
})

testthat::test_that("clean() handles three-way categorical interactions", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp * x1 * x2, data = df)

  suppressWarnings(result <- clean(model, include_reference = TRUE))

  # Should include all main effects with references
  testthat::expect_true("grpA" %in% result$term)
  testthat::expect_true("x1no" %in% result$term)
  testthat::expect_true("x2no" %in% result$term)

  # Should include three-way interactions
  three_way <- result$term[sapply(strsplit(result$term, ":"), length) == 3]
  testthat::expect_true(length(three_way) > 0)
})

testthat::test_that("clean() confidence intervals are calculated correctly", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ age, data = df)

  result <- clean(model, conf_level = 0.95)

  # Check that CIs contain the estimate
  testthat::expect_true(all(result$conf_low <= result$estimate, na.rm = TRUE))
  testthat::expect_true(all(result$conf_high >= result$estimate, na.rm = TRUE))

  # Check CI width is approximately correct
  age_row <- result[result$term == "age", ]
  expected_width <- 2 *
    stats::qt(0.975, df = model$df.residual) *
    attr(age_row, "se")[2]
  actual_width <- age_row$conf_high - age_row$conf_low
  testthat::expect_equal(actual_width, expected_width, tolerance = 1e-6)
})

testthat::test_that("clean() sample sizes are correct", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp, data = df)

  result <- clean(model, include_reference = TRUE)

  # Count actual observations in data
  grp_counts <- table(df$grp)

  testthat::expect_equal(
    unname(result$n[result$term == "grpA"]),
    as.integer(grp_counts["A"])
  )
  testthat::expect_equal(
    unname(result$n[result$term == "grpB"]),
    as.integer(grp_counts["B"])
  )
  testthat::expect_equal(
    unname(result$n[result$term == "grpC"]),
    as.integer(grp_counts["C"])
  )
})

# Tests for get_reference_levels()
testthat::test_that("get_reference_levels() identifies reference levels correctly", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp + x1, data = df)

  result <- get_reference_levels(model, df)

  testthat::expect_true("grpA" %in% result$terms)
  testthat::expect_true("x1no" %in% result$terms)
  testthat::expect_true(all(result$n_values > 0))
})

testthat::test_that("get_reference_levels() handles interactions", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp * x1, data = df)

  result <- get_reference_levels(model, df)

  # Should include reference interaction
  testthat::expect_true(any(
    grepl("grpA.*x1no", result$terms) | grepl("x1no.*grpA", result$terms)
  ))
})

testthat::test_that("get_reference_levels() validates against coefficients", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp, data = df)

  result <- get_reference_levels(model, df)

  testthat::expect_true("validation" %in% names(result))
  testthat::expect_type(result$validation, "list")

  # Reference level should be valid
  testthat::expect_true(result$validation$grpA$is_valid)
})

# Tests for get_term_n()
testthat::test_that("get_term_n() returns correct sample sizes", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp + age, data = df)

  result <- get_term_n(model, include_reference = FALSE)

  testthat::expect_type(result, "double")
  testthat::expect_true(all(names(result) %in% names(stats::coef(model))))
  testthat::expect_equal(as.double(result["(Intercept)"]), nrow(df))
})

testthat::test_that("get_term_n() includes reference levels when requested", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp, data = df)

  result <- get_term_n(model, include_reference = TRUE)

  testthat::expect_true("grpA" %in% names(result))
})

# Tests for get_factor_reference()
testthat::test_that("get_factor_reference() identifies treatment contrast reference", {
  df <- make_expanded_df()
  df$grp_fct <- factor(df$grp)
  model <- stats::lm(rating_overall ~ grp_fct, data = df)

  mf <- stats::model.frame(model)
  result <- get_factor_reference(mf$grp_fct, "grp_fct", model)

  testthat::expect_equal(result, "A")
})

testthat::test_that("get_factor_reference() handles ordered factors", {
  df <- make_expanded_df()
  df$x1_ord <- ordered(df$x1)
  model <- stats::lm(rating_overall ~ x1_ord, data = df)

  mf <- stats::model.frame(model)
  result <- get_factor_reference(mf$x1_ord, "x1_ord", model)

  testthat::expect_true(result %in% levels(mf$x1_ord))
})

# Tests for model_reorder()
testthat::test_that("model_reorder() places intercept first", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp + age, data = df)

  coefs <- summary(model)$coefficients |>
    tibble::as_tibble(rownames = "term")

  result <- model_reorder(model, coefs)

  testthat::expect_equal(result$term[1], "(Intercept)")
})

testthat::test_that("model_reorder() orders categorical levels correctly", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp, data = df)

  # Create shuffled coefs
  coefs <- tibble::tibble(
    term = c("grpC", "(Intercept)", "grpB", "grpA"),
    estimate = c(1.5, 5.0, 2.0, 0)
  )

  result <- model_reorder(model, coefs)

  testthat::expect_equal(result$term[1], "(Intercept)")
  testthat::expect_equal(result$term[2], "grpA")
  testthat::expect_equal(result$term[3], "grpB")
  testthat::expect_equal(result$term[4], "grpC")
})

testthat::test_that("model_reorder() handles interaction terms correctly", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp * x1, data = df)

  result <- clean(model, include_reference = TRUE)
  result <- model_reorder(model, result)

  # Main effects should come before interactions
  grp_indices <- which(result$term %in% c("grpA", "grpB", "grpC"))
  x1_indices <- which(result$term %in% c("x1no", "x1yes"))
  interaction_indices <- which(grepl(":", result$term))

  if (length(interaction_indices) > 0) {
    testthat::expect_true(max(grp_indices) < min(interaction_indices))
    testthat::expect_true(max(x1_indices) < min(interaction_indices))
  }
})

# Tests for glm models
testthat::test_that("clean() works with glm models", {
  df <- make_expanded_df()
  df$binary_outcome <- ifelse(df$rating_overall > 7, 1, 0)
  model <- stats::glm(
    binary_outcome ~ grp + age,
    data = df,
    family = stats::binomial()
  )

  result <- clean(model, include_reference = TRUE, conf_method = "wald")

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true("grpA" %in% result$term)
  testthat::expect_true(attr(result, "test_type") == "z")
})

# Edge cases
testthat::test_that("clean() handles models with only numeric predictors", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ age + income, data = df)

  result <- clean(model, include_reference = TRUE)

  testthat::expect_equal(nrow(result), 3) # Intercept + age + income
})

testthat::test_that("clean() handles models with missing data", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp + x1, data = df)

  result <- clean(model, include_reference = TRUE)

  # Should still work despite NAs in x1
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(all(!is.na(result$term)))
})

# Tests for polynomial terms
testthat::test_that("clean() handles polynomial terms", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ poly(age, 2) + grp, data = df)

  result <- clean(model, include_reference = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(any(grepl("poly", result$term)))
  testthat::expect_true("grpA" %in% result$term)

  # All polynomial terms should have full sample size
  poly_terms <- result$term[grepl("poly", result$term)]
  testthat::expect_true(all(
    result$n[result$term %in% poly_terms] == nrow(stats::model.frame(model))
  ))
})

testthat::test_that("clean() handles raw polynomial terms", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ poly(age, 3, raw = TRUE), data = df)

  result <- clean(model, include_reference = FALSE)

  testthat::expect_equal(nrow(result), 4) # Intercept + 3 polynomial terms
  testthat::expect_true(all(grepl("poly|Intercept", result$term)))
})

testthat::test_that("clean() handles I() transformed polynomials", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ age + I(age^2) + grp, data = df)

  result <- clean(model, include_reference = TRUE)

  testthat::expect_true(any(grepl("I(age^2)", result$term, fixed = TRUE)))
  testthat::expect_true("grpA" %in% result$term)
})

testthat::test_that("clean() handles polynomial × categorical interactions", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ poly(age, 2) * grp, data = df)

  result <- clean(model, include_reference = TRUE)

  # Should include main effects and interactions
  testthat::expect_true("grpA" %in% result$term)
  testthat::expect_true(any(grepl("poly.*grp|grp.*poly", result$term)))
})

# Tests for splines (requires splines package)
testthat::test_that("clean() handles natural splines (ns)", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ splines::ns(age, df = 3) + grp, data = df)

  result <- clean(model, include_reference = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(any(grepl("ns", result$term)))
  testthat::expect_true("grpA" %in% result$term)

  # All spline terms should have full sample size
  spline_terms <- result$term[grepl("ns", result$term)]
  testthat::expect_true(all(
    result$n[result$term %in% spline_terms] == nrow(stats::model.frame(model))
  ))
})

testthat::test_that("clean() handles B-splines (bs)", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ splines::bs(age, df = 4), data = df)

  result <- clean(model, include_reference = FALSE)

  testthat::expect_true(any(grepl("bs", result$term)))
  testthat::expect_equal(nrow(result), 5) # Intercept + 4 spline terms
})

testthat::test_that("clean() handles spline × categorical interactions", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ splines::ns(age, df = 2) * grp, data = df)

  result <- clean(model, include_reference = TRUE)

  # Should include reference level for categorical
  testthat::expect_true("grpA" %in% result$term)

  # Should include interaction terms
  testthat::expect_true(any(grepl("ns.*grp|grp.*ns", result$term)))
})

testthat::test_that("clean() handles multiple spline terms", {
  df <- make_expanded_df()
  model <- stats::lm(
    rating_overall ~ splines::ns(age, df = 2) + splines::ns(income, df = 2),
    data = df
  )

  result <- clean(model, include_reference = FALSE)

  age_splines <- sum(grepl("age", result$term))
  income_splines <- sum(grepl("income", result$term))

  testthat::expect_equal(age_splines, 2)
  testthat::expect_equal(income_splines, 2)
})

testthat::test_that("model_reorder() preserves spline term order", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ grp + splines::ns(age, df = 3), data = df)

  result <- clean(model, include_reference = TRUE)

  # Check that intercept is first
  testthat::expect_equal(result$term[1], "(Intercept)")

  # Check that grp terms come together
  grp_indices <- which(result$term %in% c("grpA", "grpB", "grpC"))
  testthat::expect_equal(length(grp_indices), 3)
  testthat::expect_equal(diff(grp_indices), c(1, 1))
})


# Edge cases with transformations
testthat::test_that("clean() handles log transformations", {
  df <- make_expanded_df()
  df$income_pos <- df$income + abs(min(df$income)) + 1 # Ensure positive
  model <- stats::lm(rating_overall ~ log(income_pos) + grp, data = df)

  result <- clean(model, include_reference = TRUE)

  testthat::expect_true(any(grepl("log", result$term)))
  testthat::expect_true("grpA" %in% result$term)
})

testthat::test_that("clean() handles sqrt transformations", {
  df <- make_expanded_df()
  df$age_pos <- abs(df$age)
  model <- stats::lm(rating_overall ~ sqrt(age_pos) + grp, data = df)

  result <- clean(model, include_reference = TRUE)

  testthat::expect_true(any(grepl("sqrt", result$term)))
  testthat::expect_true("grpA" %in% result$term)
})

testthat::test_that("clean() handles complex formulas with multiple transformations", {
  df <- make_expanded_df()
  df$income_pos <- df$income + abs(min(df$income)) + 1
  model <- stats::lm(
    rating_overall ~ poly(age, 2) + splines::ns(income_pos, df = 2) + grp * x1,
    data = df
  )

  result <- clean(model, include_reference = TRUE)

  # Should handle all components
  testthat::expect_true(any(grepl("poly", result$term)))
  testthat::expect_true(any(grepl("ns", result$term)))
  testthat::expect_true("grpA" %in% result$term)
  testthat::expect_true("x1no" %in% result$term)
  testthat::expect_true(any(grepl(":", result$term))) # Interactions
})

# Test that n values are correct for transformed variables
testthat::test_that("get_term_n() correctly counts for polynomial terms", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ poly(age, 2) + grp, data = df)

  result <- get_term_n(model, include_reference = TRUE)

  poly_n <- result[grepl("poly", names(result))]
  testthat::expect_true(all(poly_n == nrow(stats::model.frame(model))))
})

testthat::test_that("get_term_n() correctly counts for spline terms", {
  df <- make_expanded_df()
  model <- stats::lm(rating_overall ~ splines::ns(age, df = 3), data = df)

  result <- get_term_n(model, include_reference = FALSE)

  spline_n <- result[grepl("ns", names(result))]
  testthat::expect_true(all(spline_n == nrow(stats::model.frame(model))))
})
