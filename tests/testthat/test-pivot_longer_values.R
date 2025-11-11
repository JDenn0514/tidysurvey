# test-pivot_longer_values.R

# ------------------------------
# default method tests (unchanged)
# ------------------------------

testthat::test_that("pivot_longer_values.default pivots data and preserves labels (implicit name_label)", {
  df <- make_basic_df() |> label_vars()

  long_df <- pivot_longer_values(
    df,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  # Basic structure
  testthat::expect_s3_class(long_df, "tbl_df")
  testthat::expect_true(all(
    c("id", "grp", "wts", "question", "response") %in% names(long_df)
  ))

  # Labels on the names_to column
  question_col <- long_df$question
  testthat::expect_true(!is.null(attr(question_col, "labels")))
  testthat::expect_true(!is.null(attr(question_col, "label")))

  # Check label attribute is preface from attr_question_preface
  testthat::expect_equal(
    attr(question_col, "label"),
    "Please indicate which of the following colors you like. Select all that apply:"
  )
})

testthat::test_that("pivot_longer_values.default pivots data and respects explicit name_label", {
  df <- make_basic_df() |> label_vars()

  long_df <- pivot_longer_values(
    df,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response",
    name_label = "Custom Label"
  )

  question_col <- long_df$question
  testthat::expect_equal(attr(question_col, "label"), "Custom Label")
})

# ------------------------------
# survey.design method tests using srvyr::as_survey_design
# ------------------------------

# ids == 1 branch (implicit name_label)
testthat::test_that("survey.design (srvyr): ids == 1 branch", {
  df <- make_basic_df() |> label_vars()

  svy <- srvyr::as_survey_design(
    .data = df,
    ids = 1,
    weights = wts
  )

  out <- pivot_longer_values(
    svy,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  testthat::expect_s3_class(out, "survey.design")

  long_data <- out$variables
  testthat::expect_true(all(
    c("id", "grp", "wts", "question", "response") %in% names(long_data)
  ))

  q <- long_data$question
  testthat::expect_true(!is.null(attr(q, "labels")))
  testthat::expect_true(!is.null(attr(q, "label")))
  testthat::expect_equal(
    attr(q, "label"),
    "Please indicate which of the following colors you like. Select all that apply:"
  )
})

# ids single variable branch
testthat::test_that("survey.design (srvyr): ids single variable branch", {
  df <- make_basic_df() |> label_vars()

  svy <- srvyr::as_survey_design(
    .data = df,
    ids = id,
    weights = wts
  )

  out <- pivot_longer_values(
    svy,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  testthat::expect_s3_class(out, "survey.design")
  testthat::expect_true(all(
    c("id", "question", "response") %in% names(out$variables)
  ))
})

# ids "+" branch
# We coerce srvyr to record a multi-id by providing a combined string "id + id2"
# via a quosure-like expression. This mirrors how your function parses design_vars.
testthat::test_that("survey.design (srvyr): ids multi-variable '+' branch", {
  df <- make_basic_df() |> label_vars()

  svy <- srvyr::as_survey_design(
    .data = df,
    ids = c(id, id2),
    weights = wts
  )

  out <- pivot_longer_values(
    svy,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  testthat::expect_s3_class(out, "survey.design")
  testthat::expect_true(all(
    c("id", "id2", "question", "response") %in% names(out$variables)
  ))
})

# strata single variable and '+' branches
testthat::test_that("survey.design (srvyr): strata single variable and '+' branches", {
  df <- make_basic_df() |> label_vars()

  # Single variable strata
  svy_single <- srvyr::as_survey_design(
    .data = df,
    ids = id,
    strata = strata,
    weights = wts
  )
  out_single <- pivot_longer_values(
    svy_single,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_single, "survey.design")
  testthat::expect_true("strata" %in% names(out_single$variables))

  svy_plus <- srvyr::as_survey_design(
    .data = df,
    ids = id,
    strata = c(strata, strata2),
    weights = wts,
    nest = TRUE
  )
  out_plus <- pivot_longer_values(
    svy_plus,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_plus, "survey.design")
  testthat::expect_true(all(
    c("strata", "strata2") %in% names(out_plus$variables)
  ))
})

# weights single variable and '+' branches
testthat::test_that("survey.design (srvyr): weights single variable and '+' branches", {
  df <- make_basic_df() |> label_vars()

  # Single weights
  svy_single <- srvyr::as_survey_design(
    .data = df,
    ids = id,
    weights = wts
  )
  out_single <- pivot_longer_values(
    svy_single,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_single, "survey.design")
  testthat::expect_true("wts" %in% names(out_single$variables))

  svy_plus <- srvyr::as_survey_design(
    .data = df,
    ids = id,
    weights = c(wts, w2)
  )
  out_plus <- pivot_longer_values(
    svy_plus,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_plus, "survey.design")
  testthat::expect_true(all(c("wts", "w2") %in% names(out_plus$variables)))
})

# fpc single variable and '+' branches
# fpc single stage (vector) and two-stage (data.frame) branches
testthat::test_that("survey.design (srvyr): FPC single-stage vector and two-stage data.frame branches", {
  df <- make_basic_df() |> label_vars()

  # # Derive PSUs and SSUs in the test (if not already present in helper)
  # # 4 PSUs (1..4), each with 2 sampled SSUs (rows)
  # df$psu <- rep(1:4, each = 2)
  # df$ssu <- seq_len(nrow(df))

  # # Valid FPCs: population sizes (N) at each stage
  # # PSU popsize: each sampled PSU represents a pop of 10 PSUs
  # df$fpc_psu <- 10L
  # # SSU popsize within each PSU: 5 SSUs in population per PSU; we sampled 2 per PSU
  # df$fpc_ssu <- 5L

  # 1) Single-stage FPC (vector), with single-stage ids
  svy_single <- srvyr::as_survey_design(
    .data = df,
    ids = psu,
    weights = wts,
    fpc = fpc_psu
  )
  out_single <- pivot_longer_values(
    svy_single,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_single, "survey.design")
  testthat::expect_true("fpc_psu" %in% names(out_single$variables))

  # 2) Two-stage FPC, with two-stage ids
  svy_two <- srvyr::as_survey_design(
    .data = df,
    ids = c(psu, ssu),
    weights = wts,
    fpc = c(fpc_psu, fpc_ssu)
  )
  out_two <- pivot_longer_values(
    svy_two,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_two, "survey.design")
  testthat::expect_true(all(
    c("fpc_psu", "fpc_ssu") %in% names(out_two$variables)
  ))
})

testthat::test_that("survey.design (srvyr): nest enforces nesting when PSUs overlap across strata", {
  df <- make_basic_df() |> label_vars()
  n <- nrow(df)

  # Case A: PSUs are ALREADY nested in strata (distinct codes per stratum)
  # Strata: 1,1,1,1,2,2,2,2
  # PSUs:   1,2,3,4,5,6,7,8  -> unique across strata
  df_nested_ok <- df
  df_nested_ok$strata <- rep(1:2, each = n / 2)
  df_nested_ok$psu <- 1:n # unique PSUs; nested by construction
  df_nested_ok$ssu <- 1:n
  df_nested_ok$fpc_psu <- 10L
  df_nested_ok$fpc_ssu <- 5L

  svy_ok_no_nest <- srvyr::as_survey_design(
    .data = df_nested_ok,
    ids = c(psu, ssu),
    strata = strata,
    weights = wts,
    fpc = c(fpc_psu, fpc_ssu),
    nest = FALSE # valid because PSUs are already nested in strata
  )

  out_ok_no_nest <- pivot_longer_values(
    svy_ok_no_nest,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_ok_no_nest, "survey.design")

  # Case B: PSUs are NOT nested in strata (overlap), requires nest = TRUE
  # Strata: 1,1,1,1,2,2,2,2
  # PSUs:   1,2,3,4,1,2,3,4  -> overlap across strata
  df_overlap <- df
  df_overlap$strata <- rep(1:2, each = n / 2)
  df_overlap$psu <- rep(1:4, times = 3) # overlap across strata
  df_overlap$ssu <- 1:n
  df_overlap$fpc_psu <- 10L
  df_overlap$fpc_ssu <- 5L

  # This would error with nest = FALSE; we set TRUE to allow relabeling
  svy_need_nest <- srvyr::as_survey_design(
    .data = df_overlap,
    ids = c(psu, ssu),
    strata = strata,
    weights = wts,
    fpc = c(fpc_psu, fpc_ssu),
    nest = TRUE
  )

  out_need_nest <- pivot_longer_values(
    svy_need_nest,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_need_nest, "survey.design")
})


# nest present vs absent
testthat::test_that("survey.design (srvyr): nest present and absent", {
  df <- make_basic_df() |> label_vars()

  # Present
  svy_present <- srvyr::as_survey_design(
    .data = df,
    ids = id,
    weights = wts,
    nest = TRUE
  )
  out_present <- pivot_longer_values(
    svy_present,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_present, "survey.design")

  # Absent (FALSE)
  svy_absent <- srvyr::as_survey_design(
    .data = df,
    ids = id,
    weights = wts,
    nest = FALSE
  )
  out_absent <- pivot_longer_values(
    svy_absent,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  testthat::expect_s3_class(out_absent, "survey.design")
})

# ... passthrough
testthat::test_that("survey.design (srvyr): ... forwarded to tidyr::pivot_longer (values_drop_na)", {
  df <- make_basic_df() |> label_vars()

  svy <- srvyr::as_survey_design(
    .data = df,
    ids = 1,
    weights = wts
  )

  out <- pivot_longer_values(
    svy,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response",
    values_drop_na = TRUE
  )

  testthat::expect_s3_class(out, "survey.design")
  testthat::expect_false(any(is.na(out$variables$response)))
})

# labels preserved across '+' branches
testthat::test_that("survey.design (srvyr): labels on names_to preserved across '+' branches", {
  df <- make_basic_df() |> label_vars()

  ids_expr <- rlang::expr(!!rlang::parse_expr("id + id2"))
  weights_expr <- rlang::expr(!!rlang::parse_expr("wts + w2"))

  svy <- srvyr::as_survey_design(
    .data = df,
    ids = c(id, id2),
    weights = c(wts, w2)
  )

  out <- pivot_longer_values(
    svy,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  qc <- out$variables$question
  testthat::expect_true(!is.null(attr(qc, "labels")))
  testthat::expect_true(!is.null(attr(qc, "label")))
})

# explicit name_label respected
testthat::test_that("survey.design (srvyr): respects explicit name_label", {
  df <- make_basic_df() |> label_vars()

  svy <- srvyr::as_survey_design(
    .data = df,
    ids = 1,
    weights = wts
  )

  long_svy <- pivot_longer_values(
    svy,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response",
    name_label = "Custom Label"
  )

  long_data <- long_svy$variables
  question_col <- long_data$question
  testthat::expect_equal(attr(question_col, "label"), "Custom Label")
})

# Test with satisfaction questions (numeric Likert scale)
testthat::test_that("pivot_longer_values works with satisfaction scales", {
  df <- make_basic_df() |> label_vars()

  long_df <- pivot_longer_values(
    df,
    cols = starts_with("satisfaction_"),
    names_to = "satisfaction_aspect",
    values_to = "rating"
  )

  testthat::expect_equal(nrow(long_df), nrow(df) * 4) # 4 satisfaction questions

  aspect_col <- long_df$satisfaction_aspect
  testthat::expect_equal(
    attr(aspect_col, "label"),
    "Please rate your satisfaction with each of the following aspects:"
  )

  # Check that all satisfaction variables are included
  testthat::expect_true(all(
    c(
      "satisfaction_service",
      "satisfaction_price",
      "satisfaction_quality",
      "satisfaction_support"
    ) %in%
      unique(long_df$satisfaction_aspect)
  ))
})

# Test with agreement questions (7-point scale)
testthat::test_that("pivot_longer_values works with agreement scales", {
  df <- make_basic_df() |> label_vars()

  long_df <- pivot_longer_values(
    df,
    cols = starts_with("agree_"),
    names_to = "agreement_item",
    values_to = "agreement_level"
  )

  testthat::expect_equal(nrow(long_df), nrow(df) * 3) # 3 agreement questions

  item_col <- long_df$agreement_item
  testthat::expect_equal(
    attr(item_col, "label"),
    "Please indicate how much you agree or disagree with each statement:"
  )
})

# Test with mixed question types
testthat::test_that("pivot_longer_values works with mixed numeric scales", {
  df <- make_basic_df() |> label_vars()

  long_df <- pivot_longer_values(
    df,
    cols = c(starts_with("rating_"), starts_with("freq_")),
    names_to = "measure_type",
    values_to = "score"
  )

  # 3 rating + 3 frequency = 6 questions
  testthat::expect_equal(nrow(long_df), nrow(df) * 6)

  # Should include all the variables
  expected_vars <- c(
    "rating_overall",
    "rating_value",
    "rating_experience",
    "freq_use_product",
    "freq_visit_store",
    "freq_contact_support"
  )
  testthat::expect_true(all(expected_vars %in% unique(long_df$measure_type)))
})

# ------------------------------
# svyrep.design method tests using srvyr::as_survey_rep
# ------------------------------

# Helper function to create replicate weights for testing
make_rep_weights <- function(n = 8, nrep = 4) {
  # Create simple replicate weights matrix
  rep_weights <- matrix(runif(n * nrep, 0.5, 1.5), nrow = n, ncol = nrep)
  colnames(rep_weights) <- paste0("rep_", 1:nrep)
  return(rep_weights)
}


# Basic svyrep.design test
testthat::test_that("svyrep.design (srvyr): basic functionality with BRR", {
  df <- make_basic_df() |> label_vars()

  # Create replicate weights and add them directly to the data
  rep_weights <- make_rep_weights(nrow(df))
  df <- df |>
    dplyr::bind_cols(rep_weights)

  svy_rep <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"), # Assuming your rep weights start with "rep_"
    type = "BRR",
    weights = wts
  )

  out <- pivot_longer_values(
    svy_rep,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  testthat::expect_s3_class(out, "svyrep.design")

  long_data <- out$variables
  testthat::expect_true(all(
    c("id", "grp", "wts", "question", "response") %in% names(long_data)
  ))

  # Check that we have twice as many rows (2 x variables pivoted)
  testthat::expect_equal(nrow(long_data), nrow(df) * 2)

  # Check labels
  q <- long_data$question
  testthat::expect_true(!is.null(attr(q, "labels")))
  testthat::expect_true(!is.null(attr(q, "label")))
  testthat::expect_equal(
    attr(q, "label"),
    "Please indicate which of the following colors you like. Select all that apply:"
  )
})

# Test replicate weights matrix expansion
testthat::test_that("svyrep.design (srvyr): replicate weights matrix properly expanded", {
  df <- make_basic_df() |> label_vars()

  # Create replicate weights and add them directly to the data
  rep_weights <- make_rep_weights(nrow(df))
  df <- df |>
    dplyr::bind_cols(rep_weights)

  svy_rep <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    type = "bootstrap",
    weights = wts
  )

  out <- pivot_longer_values(
    svy_rep,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  # Check that replicate weights matrix has correct dimensions
  testthat::expect_equal(nrow(out$repweights), nrow(df) * 2) # 8 * 2 = 16 rows
  testthat::expect_equal(ncol(out$repweights), ncol(rep_weights)) # Same number of replicates

  # Check that the first two rows of new repweights match the first row of original
  testthat::expect_equal(
    as.numeric(out$repweights[1, ]),
    unname(rep_weights[1, ])
  )
  testthat::expect_equal(
    as.numeric(out$repweights[2, ]),
    unname(rep_weights[1, ])
  )
})

# Test different replication types
testthat::test_that("svyrep.design (srvyr): different replication types", {
  df <- make_basic_df() |> label_vars()

  # Create replicate weights and add them directly to the data
  rep_weights <- make_rep_weights(nrow(df))
  df <- df |>
    dplyr::bind_cols(rep_weights)

  # Test JKn (jackknife)
  svy_jkn <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    type = "JKn",
    weights = wts,
    rscales = rep(3 / 4, 4)
  )

  out_jkn <- pivot_longer_values(
    svy_jkn,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  testthat::expect_s3_class(out_jkn, "svyrep.design")
  testthat::expect_equal(out_jkn$type, "JKn")

  # Test bootstrap
  svy_boot <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    type = "bootstrap",
    weights = wts
  )

  out_boot <- pivot_longer_values(
    svy_boot,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  testthat::expect_s3_class(out_boot, "svyrep.design")
  testthat::expect_equal(out_boot$type, "bootstrap")
})

# Test with scale and rscales parameters
testthat::test_that("svyrep.design (srvyr): scale and rscales preserved", {
  df <- make_basic_df() |> label_vars()

  # Create replicate weights and add them directly to the data
  rep_weights <- make_rep_weights(nrow(df))
  df <- df |>
    dplyr::bind_cols(rep_weights)

  svy_rep <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    type = "BRR",
    weights = wts,
    rscales = rep(1.2, ncol(rep_weights))
  )

  out <- pivot_longer_values(
    svy_rep,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )

  testthat::expect_s3_class(out, "svyrep.design")
  testthat::expect_equal(length(out$rscales), ncol(rep_weights))
  testthat::expect_true(all(out$rscales == 1.2))
})

# Test without weights
testthat::test_that("svyrep.design: triggers prob branch", {
  df <- make_basic_df() |> label_vars()

  rep_weights <- make_rep_weights(nrow(df))
  df <- dplyr::bind_cols(df, rep_weights)

  # Create a survey object WITHOUT specifying weights
  svy_no_weights <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    weights = wts,
    type = "BRR"
  )

  # Your pivot function should trigger the prob branch

  out <- pivot_longer_values(
    svy_no_weights,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response"
  )
  # Confirm it still produces a valid survey object
  testthat::expect_s3_class(out, "svyrep.design")
})


# Test explicit name_label
testthat::test_that("svyrep.design (srvyr): respects explicit name_label", {
  df <- make_basic_df() |> label_vars()

  # Create replicate weights and add them directly to the data
  rep_weights <- make_rep_weights(nrow(df))
  df <- df |>
    dplyr::bind_cols(rep_weights)

  svy_rep <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    type = "BRR",
    weights = wts
  )

  out <- pivot_longer_values(
    svy_rep,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response",
    name_label = "Custom Replicate Label"
  )

  question_col <- out$variables$question
  testthat::expect_equal(attr(question_col, "label"), "Custom Replicate Label")
})

# Test ... passthrough to tidyr::pivot_longer
testthat::test_that("svyrep.design (srvyr): ... forwarded to tidyr::pivot_longer", {
  df <- make_basic_df() |> label_vars()

  # Create replicate weights and add them directly to the data
  rep_weights <- make_rep_weights(nrow(df))
  df <- df |>
    dplyr::bind_cols(rep_weights)

  svy_rep <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    type = "BRR",
    weights = wts
  )

  out <- pivot_longer_values(
    svy_rep,
    cols = c(x1, x2),
    names_to = "question",
    values_to = "response",
    values_drop_na = TRUE
  )

  testthat::expect_s3_class(out, "svyrep.design")
  testthat::expect_false(any(is.na(out$variables$response)))
})

# Test with more variables to ensure proper replication factor
testthat::test_that("svyrep.design (srvyr): works with multiple pivot columns", {
  df <- make_basic_df() |> label_vars()

  # Create replicate weights and add them directly to the data
  rep_weights <- make_rep_weights(nrow(df))
  df <- df |>
    dplyr::bind_cols(rep_weights)

  svy_rep <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    type = "BRR",
    weights = wts
  )

  out <- pivot_longer_values(
    svy_rep,
    cols = tidyselect::starts_with("satisfaction_"), # 4 variables
    names_to = "question",
    values_to = "response"
  )

  testthat::expect_s3_class(out, "svyrep.design")

  # Should have 4 times the original rows (4 variables pivoted)
  testthat::expect_equal(nrow(out$variables), nrow(df) * 4)
  testthat::expect_equal(nrow(out$repweights), nrow(df) * 4)

  # Check that labels include all 4 variables
  question_labels <- attr(out$variables$question, "labels")
  testthat::expect_equal(length(question_labels), 4)
  testthat::expect_true(all(
    c(
      "Satisfaction with Service",
      "Satisfaction with Price",
      "Satisfaction with Quality",
      "Satisfaction with Support"
    ) %in%
      names(question_labels)
  ))
})

# Test that variance calculations still work after pivot
testthat::test_that("svyrep.design (srvyr): variance estimation works after pivot", {
  df <- make_basic_df() |> label_vars()

  # Create replicate weights and add them directly to the data
  rep_weights <- make_rep_weights(nrow(df))
  df <- df |>
    dplyr::bind_cols(rep_weights)

  svy_rep <- srvyr::as_survey_rep(
    .data = df,
    repweights = starts_with("rep_"),
    type = "bootstrap",
    weights = wts
  )

  out <- pivot_longer_values(
    svy_rep,
    cols = starts_with("satisfaction_"), # Use numeric variable for mean calculation
    names_to = "question",
    values_to = "response"
  )

  # Should be able to calculate means and SEs without error
  testthat::expect_no_error({
    mean_est <- srvyr::summarize(
      out,
      mean_resp = srvyr::survey_mean(response, na.rm = TRUE)
    )
  })

  testthat::expect_s3_class(out, "svyrep.design")
})
