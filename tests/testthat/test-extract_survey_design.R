testthat::test_that("extract_survey_design works for simple survey design", {
  df <- make_basic_df()

  # Basic 1-stage design
  dsgn <- survey::svydesign(
    id = ~psu,
    strata = ~strata,
    weights = ~wts,
    data = df,
    nest = TRUE
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$ids, "psu")
  testthat::expect_equal(out$strata, "strata")
  testthat::expect_equal(out$weights, "wts")
  testthat::expect_true(out$nest) # explicitly set
})

testthat::test_that("extract_survey_design detects nest=TRUE when PSUs do overlap strata", {
  df <- make_basic_df()

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = psu,
    strata = strata,
    weights = wts,
    nest = FALSE
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$ids, "psu")
  testthat::expect_equal(out$strata, "strata")
  testthat::expect_equal(out$weights, "wts")
  testthat::expect_false(out$nest)
})

testthat::test_that("extract_survey_design detects nest=TRUE when PSUs overlap strata", {
  df <- make_basic_df()

  # Force overlap by using id2 as PSU across strata2
  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = id2,
    strata = strata2,
    weights = wts
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$ids, "id2")
  testthat::expect_equal(out$strata, "strata2")
  testthat::expect_equal(out$weights, "wts")
  testthat::expect_false(out$nest) # should be inferred by overlap check
})

testthat::test_that("extract_survey_design works with multiple weights", {
  df <- make_basic_df()

  dsgn <- survey::svydesign(
    id = ~psu,
    strata = ~strata,
    weights = ~w2,
    data = df,
    nest = FALSE
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$weights, "w2")
  testthat::expect_false(out$nest)
})

testthat::test_that("extract_survey_design works with srvyr tbl_svy objects", {
  df <- make_basic_df()

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = psu,
    strata = strata,
    weights = wts,
    nest = TRUE
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$ids, "psu")
  testthat::expect_equal(out$strata, "strata")
  testthat::expect_equal(out$weights, "wts")
  testthat::expect_false(out$nest)
})


testthat::test_that("extract_survey_design returns nest=FALSE when PSUs do not overlap strata", {
  # Non-overlapping dataset
  df <- tibble::tibble(
    psu = 1:12,
    strata = rep(1:3, each = 4),
    wts = runif(12)
  )

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = psu,
    strata = strata,
    weights = wts
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$ids, "psu")
  testthat::expect_equal(out$strata, "strata")
  testthat::expect_false(out$nest)
})


testthat::test_that("extract_survey_design works with multiple variables for ids or strata", {
  df <- tibble::tibble(
    psu1 = rep(1:3, each = 4),
    psu2 = rep(4:6, each = 4),
    strata1 = rep(1:2, each = 6),
    strata2 = rep(3:4, each = 6),
    wts = runif(12)
  )

  # Multiple ids
  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = c(psu1, psu2),
    strata = c(strata1, strata2),
    weights = wts,
    nest = TRUE
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$ids, c("psu1", "psu2"))
  testthat::expect_equal(out$strata, c("strata1", "strata2"))
})

testthat::test_that("extract_survey_design works with survey::svydesign objects", {
  df <- tibble::tibble(
    psu = 1:6,
    strata = rep(1:3, each = 2),
    wts = runif(6)
  )

  dsgn <- survey::svydesign(
    id = ~psu,
    strata = ~strata,
    weights = ~wts,
    data = df,
    nest = TRUE
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$ids, "psu")
  testthat::expect_equal(out$strata, "strata")
  testthat::expect_true(out$nest)
})

testthat::test_that("extract_survey_design correctly handles multiple weights", {
  df <- tibble::tibble(
    psu = 1:6,
    strata = rep(1:3, each = 2),
    wts1 = runif(6),
    wts2 = runif(6)
  )

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = psu,
    strata = strata,
    weights = c(wts1, wts2)
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$weights, c("wts1", "wts2"))
})


testthat::test_that("extract_survey_design returns FPC variables correctly", {
  df <- make_basic_df()

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = c(ssu, psu),
    strata = strata,
    weights = wts,
    fpc = c(fpc_ssu, fpc_psu),
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$fpc, c("fpc_ssu", "fpc_psu"))
})

testthat::test_that("extract_survey_design handles missing weights or clusters", {
  df <- tibble::tibble(
    psu = 1:6,
    strata = rep(1:3, each = 2),
    wts = runif(6)
  )

  # Missing weights
  dsgn1 <- srvyr::as_survey_design(
    .data = df,
    ids = psu,
    strata = strata
  )
  out1 <- extract_survey_design(dsgn1)
  testthat::expect_null(out1$weights)

  # Missing cluster
  dsgn2 <- srvyr::as_survey_design(
    .data = df,
    strata = strata,
    weights = wts
  )
  out2 <- extract_survey_design(dsgn2)
  testthat::expect_null(out2$ids)
})

testthat::test_that("extract_survey_design handles unstratified designs", {
  df <- tibble::tibble(
    psu = 1:6,
    wts = runif(6)
  )

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = psu,
    weights = wts
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_null(out$strata)
  testthat::expect_equal(out$ids, "psu")
  testthat::expect_equal(out$weights, "wts")
})


testthat::test_that("extract_survey_design works with character inputs", {
  df <- tibble::tibble(
    psu = 1:6,
    strata = rep(1:3, each = 2),
    wts = runif(6)
  )

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = "psu",
    strata = "strata",
    weights = "wts"
  )

  out <- extract_survey_design(dsgn)

  testthat::expect_equal(out$ids, "psu")
  testthat::expect_equal(out$strata, "strata")
  testthat::expect_equal(out$weights, "wts")
})

testthat::test_that("extract_survey_design returns NULL for ids = 1 in svydesign", {
  df <- make_basic_df()

  dsgn <- survey::svydesign(
    ids = ~1,
    strata = ~strata,
    weights = ~wts,
    data = df
  )

  out <- extract_survey_design(dsgn)
  testthat::expect_null(out$ids)
})

testthat::test_that("extract_survey_design returns NULL for ids = 0 in svydesign", {
  df <- make_basic_df()

  dsgn <- survey::svydesign(
    ids = ~0,
    strata = ~strata,
    weights = ~wts,
    data = df
  )

  out <- extract_survey_design(dsgn)
  testthat::expect_null(out$ids)
})

testthat::test_that("extract_survey_design returns NULL for ids = 1 in as_survey_design", {
  df <- make_basic_df()

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = 1,
    strata = strata,
    weights = wts
  )

  out <- extract_survey_design(dsgn)
  testthat::expect_null(out$ids)
})

testthat::test_that("extract_survey_design returns NULL for ids = 0 in as_survey_design", {
  df <- make_basic_df()

  dsgn <- srvyr::as_survey_design(
    .data = df,
    ids = 0,
    strata = strata,
    weights = wts
  )

  out <- extract_survey_design(dsgn)
  testthat::expect_null(out$ids)
})
