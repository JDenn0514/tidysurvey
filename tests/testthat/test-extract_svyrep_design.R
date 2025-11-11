# testthat::test_that("extract_svyrep_design() works for survey::svyrep.design", {
#   df <- make_expanded_df() |> label_vars()

#   # Create a simple svyrep.design (BRR)
#   svyrep <- survey::svrepdesign(
#     weights = ~wts,
#     repweights = "w2",
#     data = df,
#     type = "BRR",
#     combined.weights = TRUE,
#     rho = 0.5,
#     mse = FALSE
#   )

#   out <- extract_svyrep_design(svyrep)

#   # Basic checks
#   testthat::expect_true(is.list(out))
#   testthat::expect_equal(out$type, "BRR")
#   testthat::expect_equal(out$weights, "wts")
#   testthat::expect_equal(out$repweights, "w2")
#   testthat::expect_equal(out$combined_weights, TRUE)
#   testthat::expect_equal(out$rho, 0.5)
#   testthat::expect_equal(out$mse, FALSE)

#   # scale should be NULL for BRR
#   testthat::expect_null(out$scale)
# })

# testthat::test_that("extract_svyrep_design() works for srvyr::as_survey_rep", {
#   df <- make_expanded_df() |> label_vars()

#   # Create srvyr object
#   svy <- srvyr::as_survey_rep(
#     .data = df,
#     weights = wts,
#     repweights = w2,
#     type = "JK1",
#     combined_weights = TRUE,
#     rho = 0.25,
#     scale = 2,
#     mse = TRUE
#   )

#   out <- extract_svyrep_design(svy)

#   # Basic checks
#   testthat::expect_true(is.list(out))
#   testthat::expect_equal(out$type, "JK1")
#   testthat::expect_equal(out$weights, "wts")
#   testthat::expect_equal(out$repweights, "w2")
#   testthat::expect_equal(out$combined_weights, TRUE)
#   testthat::expect_equal(out$rho, 0.25)
#   testthat::expect_equal(out$scale, 2)
#   testthat::expect_equal(out$mse, TRUE)
# })

# testthat::test_that("extract_svyrep_design() handles missing optional arguments", {
#   df <- make_expanded_df() |> label_vars()

#   svy <- srvyr::as_survey_rep(
#     .data = df,
#     weights = wts,
#     repweights = w2,
#     type = "JK1"
#     # combined_weights, rho, scale, mse omitted
#   )

#   out <- extract_svyrep_design(svy)

#   testthat::expect_true(is.list(out))
#   testthat::expect_equal(out$type, "JK1")
#   testthat::expect_equal(out$weights, "wts")
#   testthat::expect_equal(out$repweights, "w2")
#   testthat::expect_null(out$combined_weights)
#   testthat::expect_null(out$rho)
#   testthat::expect_null(out$scale)
#   testthat::expect_null(out$mse)
# })

# testthat::test_that("extract_svyrep_design() handles NULL weights", {
#   df <- make_expanded_df() |> label_vars()

#   svy <- srvyr::as_survey_rep(
#     .data = df,
#     repweights = w2,
#     type = "JK1"
#   )

#   out <- extract_svyrep_design(svy)

#   testthat::expect_null(out$weights)
#   testthat::expect_equal(out$repweights, "w2")
#   testthat::expect_equal(out$type, "JK1")
# })

# Generate a sample dataset and replicate weights for testing
df <- make_expanded_df() |> label_vars()
n <- nrow(df)
reps <- make_rep_weights(n = n, nrep = 5)
df <- cbind(df, reps)
rep_names <- colnames(reps)

# -------------------------------
# 1. Test that extract_svyrep_design() returns correct list structure
# -------------------------------
testthat::test_that("extract_svyrep_design() returns expected list elements", {
  original <- survey::svrepdesign(
    weights = ~wts,
    repweights = df[, rep_names],
    data = df,
    type = "Fay",
    combined.weights = TRUE,
    rho = 0.5,
    mse = FALSE
  )

  info <- extract_svyrep_design(original)

  expected_elements <- c(
    "repweights",
    "weights",
    "type",
    "combined_weights",
    "rho",
    "scale",
    "mse"
  )
  testthat::expect_true(all(expected_elements %in% names(info)))
})

# -------------------------------
# 2. Test recreation from svyrep.design works
# -------------------------------
testthat::test_that("Recreation from svrepdesign produces identical survey object with type Fay", {
  original <- survey::svrepdesign(
    weights = ~wts,
    repweights = df[, rep_names],
    data = df,
    type = "Fay",
    combined.weights = TRUE,
    rho = 0.5,
    mse = FALSE
  )

  info <- extract_svyrep_design(original)
  recreated <- do.call(
    srvyr::as_survey_rep,
    c(list(.data = df), info[lengths(info) != 0])
  )

  # Class
  testthat::expect_s3_class(recreated, c("tbl_svy", "svyrep.design"))

  # Key fields
  testthat::expect_equal(
    names(recreated$repweights),
    names(original$repweights)
  )
  testthat::expect_equal(recreated$type, original$type)
  testthat::expect_equal(recreated$weights, original$weights)
  testthat::expect_equal(recreated$combined.weights, original$combined.weights)
  testthat::expect_equal(recreated$rho, original$rho)
  testthat::expect_equal(recreated$mse, original$mse)

  # Simple calculation
  testthat::expect_equal(
    as.numeric(survey::svymean(~x3, design = original, na.rm = TRUE)),
    as.numeric(survey::svymean(~x3, design = recreated, na.rm = TRUE))
  )
})

testthat::test_that("Recreation from svrepdesign produces identical survey object with type BRR", {
  original <- survey::svrepdesign(
    weights = ~wts,
    repweights = df[, rep_names],
    data = df,
    type = "BRR",
    combined.weights = TRUE,
    mse = FALSE
  )

  info <- extract_svyrep_design(original)
  recreated <- do.call(
    srvyr::as_survey_rep,
    c(list(.data = df), info[lengths(info) != 0])
  )

  # Class
  testthat::expect_s3_class(recreated, c("tbl_svy", "svyrep.design"))

  # Key fields
  testthat::expect_equal(
    names(recreated$repweights),
    names(original$repweights)
  )
  testthat::expect_equal(recreated$type, original$type)
  testthat::expect_equal(recreated$weights, original$weights)
  testthat::expect_equal(recreated$combined.weights, original$combined.weights)
  testthat::expect_equal(recreated$rho, original$rho)
  testthat::expect_equal(recreated$mse, original$mse)

  # Simple calculation
  testthat::expect_equal(
    as.numeric(survey::svymean(~x3, design = original, na.rm = TRUE)),
    as.numeric(survey::svymean(~x3, design = recreated, na.rm = TRUE))
  )
})

# -------------------------------
# 3. Test recreation from srvyr::as_survey_rep works
# -------------------------------
testthat::test_that("Recreation from srvyr::as_survey_rep produces identical survey object", {
  original_srvyr <- srvyr::as_survey_rep(
    .data = df,
    weights = wts,
    repweights = tidyselect::all_of(c(rep_names)),
    type = "Fay",
    combined.weights = TRUE,
    rho = 0.5,
    mse = FALSE
  )

  info_srvyr <- extract_svyrep_design(original_srvyr)

  recreated_srvyr <- do.call(
    srvyr::as_survey_rep,
    c(list(.data = df), info_srvyr[lengths(info_srvyr) != 0])
  )

  # Class
  testthat::expect_s3_class(recreated_srvyr, c("tbl_svy", "svyrep.design"))

  # Key fields
  testthat::expect_equal(recreated_srvyr$type, original_srvyr$type)
  testthat::expect_equal(recreated_srvyr$weights, original_srvyr$weights)
  testthat::expect_equal(
    names(recreated_srvyr$repweights),
    names(original_srvyr$repweights)
  )
  testthat::expect_equal(
    recreated_srvyr$combined.weights,
    original_srvyr$combined.weights
  )
  testthat::expect_equal(recreated_srvyr$rho, original_srvyr$rho)
  testthat::expect_equal(recreated_srvyr$scale, original_srvyr$scale)
  testthat::expect_equal(recreated_srvyr$mse, original_srvyr$mse)

  # Simple calculation
  testthat::expect_equal(
    as.numeric(survey::svymean(~x3, design = original_srvyr, na.rm = TRUE)),
    as.numeric(survey::svymean(~x3, design = recreated_srvyr, na.rm = TRUE))
  )
})


testthat::test_that("extract_svyrep_design() handles missing optional arguments", {
  # Create svyrep.design without rho, scale, or mse
  original <- survey::svrepdesign(
    weights = ~wts,
    repweights = df[, rep_names],
    data = df,
    type = "JK1",
    scale = 1 / 5,
    combined.weights = FALSE
  )

  info <- extract_svyrep_design(original)
  recreated <- do.call(
    srvyr::as_survey_rep,
    c(list(.data = df), info[lengths(info) != 0])
  )

  testthat::expect_s3_class(recreated, c("tbl_svy", "svyrep.design"))
  testthat::expect_equal(recreated$type, original$type)
  testthat::expect_equal(recreated$combined.weights, original$combined.weights)
  testthat::expect_equal(recreated$scale, original$scale)
  testthat::expect_equal(recreated$mse, original$mse)
  testthat::expect_null(recreated$rho)
})

testthat::test_that("extract_svyrep_design() handles scale and rscales with type JK2", {
  # Create svyrep.design with scale and rscales
  suppressWarnings(
    original <- survey::svrepdesign(
      weights = ~wts,
      repweights = df[, rep_names],
      data = df,
      type = "ACS",
      scale = 1 / 5,
      rscales = rep(1, 5),
      combined.weights = TRUE
    )
  )

  info <- extract_svyrep_design(original)
  recreated <- do.call(
    srvyr::as_survey_rep,
    c(list(.data = df), info[lengths(info) != 0])
  )

  testthat::expect_s3_class(recreated, c("tbl_svy", "svyrep.design"))
  testthat::expect_equal(recreated$type, original$type)
  testthat::expect_equal(recreated$combined.weights, original$combined.weights)
  testthat::expect_equal(recreated$scale, original$scale)
  testthat::expect_equal(recreated$rscales, original$rscales)
  testthat::expect_equal(recreated$mse, original$mse)
  testthat::expect_null(recreated$rho)
})

testthat::test_that("extract_svyrep_design() handles rscales with type JKN", {
  # Create svyrep.design without rho, scale, or mse
  original <- survey::svrepdesign(
    weights = ~wts,
    repweights = df[, rep_names],
    data = df,
    type = "JKn",
    scale = 1 / 5,
    rscales = rep(1, 5),
    combined.weights = TRUE
  )

  info <- extract_svyrep_design(original)
  recreated <- do.call(
    srvyr::as_survey_rep,
    c(list(.data = df), info[lengths(info) != 0])
  )

  testthat::expect_s3_class(recreated, c("tbl_svy", "svyrep.design"))
  testthat::expect_equal(recreated$type, original$type)
  testthat::expect_equal(recreated$combined.weights, original$combined.weights)
  testthat::expect_equal(recreated$scale, original$scale)
  testthat::expect_equal(recreated$rscales, original$rscales)
  testthat::expect_equal(recreated$mse, original$mse)
  testthat::expect_null(recreated$rho)
})

testthat::test_that("extract_svyrep_design works with custom rscales vector", {
  custom_rscales <- runif(ncol(reps), 0.8, 1.2)

  original <- survey::svrepdesign(
    weights = ~wts,
    repweights = df[, rep_names],
    data = df,
    type = "other",
    combined.weights = TRUE,
    scale = 1 / 5,
    rscales = custom_rscales
  )

  info <- extract_svyrep_design(original)
  recreated <- do.call(
    srvyr::as_survey_rep,
    c(list(.data = df), info[lengths(info) != 0])
  )

  # Key checks
  testthat::expect_s3_class(recreated, c("tbl_svy", "svyrep.design"))
  attr(recreated$repweights, "terms") <- NULL
  testthat::expect_equal(recreated$repweights, original$repweights)
  testthat::expect_equal(recreated$rscales, custom_rscales)
})
