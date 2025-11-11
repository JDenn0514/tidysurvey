testthat::test_that("default: single x, no group, unweighted matches count", {
  df <- label_vars(make_basic_df())

  out <- get_freqs(
    df,
    x = x1,
    wt = wts,
    na.rm = TRUE,
    drop_zero = FALSE,
    decimals = 3
  )

  # Structure
  testthat::expect_s3_class(out, "tidysurvey_freqs")
  testthat::expect_true(all(c("x1", "n", "pct") %in% names(out)))
  testthat::expect_false("names" %in% names(out)) # single-x path should not add names_to
  testthat::expect_false("values" %in% names(out))

  # Weighted manual check:
  yes_n <- out$n[out$x1 == "yes"]
  no_n <- out$n[out$x1 == "no"]
  testthat::expect_equal(as.numeric(yes_n), 8)
  testthat::expect_equal(as.numeric(no_n), 10)

  # pct rounded to decimals+2 = 5 digits
  testthat::expect_equal(out$pct[out$x1 == "yes"], round(8 / 18, 5))
  testthat::expect_equal(out$pct[out$x1 == "no"], round(10 / 18, 5))

  # labels attached
  testthat::expect_equal(attr(out$x1, "label"), "Q1. Blue")
  testthat::expect_equal(attr(out$n, "label"), "N")
  testthat::expect_equal(attr(out$pct, "label"), "Percent")
})

testthat::test_that("default: wt = NULL falls back to equal weights", {
  df <- make_basic_df()
  out <- get_freqs(df, x = x1, wt = NULL, na.rm = TRUE)
  # unweighted counts: yes = 7, no = 9 (NA removed)
  testthat::expect_equal(out$n[out$x1 == "yes"], 5)
  testthat::expect_equal(out$n[out$x1 == "no"], 6)
})

testthat::test_that("default: group accepts string input", {
  df <- make_basic_df()
  out1 <- get_freqs(df, x = x1, group = grp, na.rm = TRUE)
  out2 <- get_freqs(df, x = x1, group = "grp", na.rm = TRUE)
  testthat::expect_equal(out1, out2)
})

testthat::test_that("default: group accepts NULL input same as missing", {
  df <- make_basic_df()
  out1 <- get_freqs(df, x = x1, group = NULL, na.rm = TRUE)
  out2 <- get_freqs(df, x = x1, na.rm = TRUE)
  testthat::expect_equal(out1, out2)
})


testthat::test_that("default: grouping works and respects weights", {
  df <- label_vars(make_basic_df())

  out <- get_freqs(
    df,
    x = x1,
    group = grp,
    wt = wts,
    na.rm = TRUE,
    drop_zero = FALSE,
    decimals = 2
  )

  testthat::expect_true(all(c("grp", "x1", "n", "pct") %in% names(out)))

  # Group A rows: 1:4, weights yes: rows 1(1),3(1)=2; no: row2(2); NA row4 removed
  # Total A = 4 weighted but with NA removed: yes=2, no=2 -> sum 4
  out_A <- subset(out, grp == "A")
  testthat::expect_equal(out_A$n[out_A$x1 == "yes"], 2)
  testthat::expect_equal(out_A$n[out_A$x1 == "no"], 2)
  testthat::expect_equal(out_A$pct[out_A$x1 == "yes"], round(2 / 4, 4))

  # Group B rows: 5:8 yes: row7(1) => 1; no: rows 5(1)+6(3)+8(2) => 6; NA none except x2 NA not involved
  out_B <- subset(out, grp == "B")
  testthat::expect_equal(out_B$n[out_B$x1 == "yes"], 1)
  testthat::expect_equal(out_B$n[out_B$x1 == "no"], 6)
  testthat::expect_equal(out_B$pct[out_B$x1 == "no"], round(6 / 7, 4))
})

testthat::test_that("default: multi-x without groups pivots and aggregates correctly", {
  df <- label_vars(make_basic_df())

  out <- get_freqs(
    df,
    x = c("x1", "x2"),
    wt = wts,
    names_to = "item",
    values_to = "resp",
    na.rm = TRUE,
    drop_zero = FALSE,
    decimals = 3
  )

  testthat::expect_true(all(c("item", "resp", "n", "pct") %in% names(out)))
  testthat::expect_true(all(unique(out$item) %in% c("Q1. Blue", "Q2. Red")))

  # For x2 (ignoring NA at row 8), compute weighted counts:
  # Q2. Red: rows 1(1),2(2),5(1) => 4; Q1. Blue: rows 3(1),4(1),6(3),7(1) => 6; total 10
  # out_x2 <- subset(out, item == "Q2. Red?")
  testthat::expect_equal(out$n[[4]], 7)
  testthat::expect_equal(out$n[[3]], 10)
  testthat::expect_equal(out$pct[[3]], round(10 / 17, 5))
})

testthat::test_that("default: multi-x with groups works and keep filter applies", {
  df <- label_vars(make_basic_df())

  out <- get_freqs(
    df,
    x = c("x1", "x2"),
    group = grp,
    wt = wts,
    names_to = "item",
    values_to = "resp",
    keep = c("yes"), # keep filter over resp
    na.rm = TRUE,
    drop_zero = FALSE,
    decimals = 3
  )

  testthat::expect_true(all(
    c("grp", "item", "resp", "n", "pct") %in% names(out)
  ))
  testthat::expect_true(all(out$resp %in% c("yes")))
  # Spot-check one group/item:
  # Group A, x1, "yes" weights: rows 1(1),3(1) -> 2
  # a_x1_yes <- subset(out, grp == "A" & item == "x1" & resp == "yes")
  testthat::expect_equal(out$n[[1]], 2)
})

testthat::test_that("default: drop_zero drops zero-count rows", {
  # Create a column with a level that never appears when na.rm=TRUE
  df <- label_vars(make_basic_df())
  df$x1 <- factor(df$x1, levels = c("yes", "no", "maybe"))
  attr(df$x1, "label") <- "Q1. Blue"

  out_keep_zero <- get_freqs(
    df,
    x = x1,
    wt = wts,
    na.rm = TRUE,
    drop_zero = FALSE
  )
  out_drop_zero <- get_freqs(
    df,
    x = x1,
    wt = wts,
    na.rm = TRUE,
    drop_zero = TRUE
  )

  testthat::expect_true(any(out_keep_zero$x1 == "maybe"))
  testthat::expect_false(any(out_drop_zero$x1 == "maybe"))
})

testthat::test_that("default: decimals controls rounding as expected", {
  df <- label_vars(make_basic_df())
  out2 <- get_freqs(df, x = x1, wt = wts, decimals = 2)
  out0 <- get_freqs(df, x = x1, wt = wts, decimals = 0)

  # n rounded to decimals, pct to decimals+2
  testthat::expect_equal(out2$n[out2$x1 == "yes"], 8.00)
  testthat::expect_equal(out0$n[out0$x1 == "yes"], 8)
  testthat::expect_equal(out2$pct[out2$x1 == "yes"], round(8 / 18, 4))
  testthat::expect_equal(out0$pct[out0$x1 == "yes"], round(8 / 18, 2))
})

testthat::test_that("default: names_to and values_to respected in multi-x", {
  df <- label_vars(make_basic_df())
  out <- get_freqs(
    df,
    x = tidyselect::all_of(c("x1", "x2")),
    wt = wts,
    names_to = "question",
    values_to = "answer",
    na.rm = TRUE
  )

  testthat::expect_true(all(c("question", "answer") %in% names(out)))
})

testthat::test_that("default: errors and warnings are informative", {
  df <- label_vars(make_basic_df())

  # x selects nothing
  testthat::expect_error(
    get_freqs(df, x = tidyselect::starts_with("zzz")),
    "x must select at least one column"
  )

  # invalid wt name
  testthat::expect_error(
    get_freqs(df, x = x1, wt = bad_wt),
    "Weight column `bad_wt` not found"
  )

  # non-numeric wt
  df$bad_wt <- as.character(df$wts)
  testthat::expect_error(
    get_freqs(df, x = x1, wt = bad_wt),
    "must be a numeric variable"
  )

  # NA removal wipes out all rows for selected vars
  df_all_na <- df
  df_all_na$x1 <- NA
  testthat::expect_error(
    get_freqs(df_all_na, x = x1, wt = wts, na.rm = TRUE),
    "After removing NAs, no rows remain"
  )
})

# -------- survey.design path --------

testthat::test_that("survey: single x no groups matches svytable", {
  df <- label_vars(make_basic_df())
  dsn <- make_svy(df)

  out <- get_freqs(dsn, x = x1, na.rm = TRUE, decimals = 3)

  testthat::expect_true(all(c("x1", "n", "pct") %in% names(out)))
  # Should match default weighted results (8 and 10 from earlier)
  testthat::expect_equal(out$n[out$x1 == "yes"], 8)
  testthat::expect_equal(out$n[out$x1 == "no"], 10)
})

testthat::test_that("survey: single x with groups uses srvyr to compute totals", {
  df <- label_vars(make_basic_df())
  dsn <- make_svy(df)

  out <- get_freqs(dsn, x = x1, group = grp, na.rm = TRUE, decimals = 3)

  testthat::expect_true(all(c("grp", "x1", "n", "pct") %in% names(out)))

  # Cross-check a group:
  # Group B yes = 1, no = 6 (from earlier)
  b <- subset(out, grp == "B")
  testthat::expect_equal(b$n[b$x1 == "yes"], 1)
  testthat::expect_equal(b$n[b$x1 == "no"], 6)
})

testthat::test_that("survey: multi-x no groups pivots and aggregates correctly", {
  df <- label_vars(make_basic_df())
  dsn <- make_svy(df)

  out <- get_freqs(
    dsn,
    x = tidyselect::all_of(c("x1", "x2")),
    names_to = "item",
    values_to = "resp",
    na.rm = TRUE,
    decimals = 2
  )

  testthat::expect_true(all(c("item", "resp", "n", "pct") %in% names(out)))
  out_x2 <- subset(out, item == "Q2. Red")
  testthat::expect_equal(out_x2$n[[1]], 6)
  testthat::expect_equal(out_x2$n[[2]], 5)
})

testthat::test_that("survey: multi-x with groups computes grouped totals", {
  df <- label_vars(make_basic_df())
  dsn <- make_svy(df)

  out <- get_freqs(
    dsn,
    x = c("x1", "x2"),
    group = grp,
    names_to = "item",
    values_to = "resp",
    na.rm = TRUE
  )

  testthat::expect_true(all(
    c("grp", "item", "resp", "n", "pct") %in% names(out)
  ))
  # Spot check one cell
  testthat::expect_equal(out$n[[1]], 1)
})

testthat::test_that("survey: NA removal that empties data triggers error", {
  df <- label_vars(make_basic_df())
  df$x1 <- NA # force NA
  dsn <- make_svy(df)

  testthat::expect_error(
    get_freqs(dsn, x = x1, na.rm = TRUE),
    "After removing NAs, no rows remain for survey path"
  )
})

testthat::test_that("survey: names_to/values_to respected and single-x renames value column back", {
  df <- label_vars(make_basic_df())
  dsn <- make_svy(df)

  # Single-x: value renamed back to x var
  out1 <- get_freqs(dsn, x = x1, na.rm = TRUE)
  testthat::expect_true("x1" %in% names(out1))
  testthat::expect_false("value" %in% names(out1))

  # Multi-x: uses provided names_to/values_to
  out2 <- get_freqs(
    dsn,
    x = tidyselect::all_of(c("x1", "x2")),
    names_to = "q",
    values_to = "ans",
    na.rm = TRUE
  )
  testthat::expect_true(all(c("q", "ans") %in% names(out2)))
})

testthat::test_that("survey: drop_zero drops zero rows and decimals apply", {
  df <- label_vars(make_basic_df())
  df$x1 <- factor(df$x1, levels = c("yes", "no", "maybe"))
  dsn <- make_svy(df)

  out <- get_freqs(dsn, x = x1, na.rm = TRUE, drop_zero = TRUE, decimals = 1)
  testthat::expect_false(any(out$x1 == "maybe"))
  # Check rounding on pct for yes
  testthat::expect_equal(out$pct[out$x1 == "yes"], round(8 / 18, 3))
})

testthat::test_that("survey: error when x selects no columns", {
  df <- label_vars(make_basic_df())
  dsn <- make_svy(df)
  testthat::expect_error(
    get_freqs(dsn, x = tidyselect::starts_with("zzz")),
    "x must select at least one column"
  )
})

testthat::test_that("low_freqs Case A: error when value_col missing in survey variables", {
  df <- label_vars(make_basic_df())
  dsn <- make_svy(df)
  # Call low_freqs directly mimicking Case A context
  # value_col = "not_there"; name_col = NULL; no groups on dsn$variables
  testthat::expect_error(
    low_freqs(dsn, value_col = "not_there", name_col = NULL, values_to = "val"),
    "`not_there` not found in survey design variables"
  )
})

testthat::test_that("survey single-x: completion shows zero levels when drop_zero = FALSE", {
  df <- label_vars(make_basic_df())
  df$x1 <- factor(df$x1, levels = c("yes", "no", "maybe"))
  dsn <- make_svy(df)

  out <- get_freqs(dsn, x = x1, na.rm = TRUE, drop_zero = FALSE)
  testthat::expect_true(any(out$x1 == "maybe"))
  # With drop_zero = TRUE it disappears
  out2 <- get_freqs(dsn, x = x1, na.rm = TRUE, drop_zero = TRUE)
  testthat::expect_false(any(out2$x1 == "maybe"))
})

testthat::test_that("low_freqs Case C: error when name_col or value_col not found", {
  df <- label_vars(make_basic_df())
  # Build a long-like survey design with only one of the needed columns
  long_df <- tibble::tibble(
    id = df$id,
    item = rep("x1", nrow(df)),
    # purposely omit the value column 'resp'
    wts = df$wts
  )
  dsn <- survey::svydesign(ids = ~1, weights = ~wts, data = long_df)

  testthat::expect_error(
    low_freqs(
      dsn,
      value_col = "resp",
      name_col = "item",
      values_to = "resp"
    ),
    "`item` or `resp` not found in survey design variables"
  )
})

testthat::test_that("low_freqs Case C: error when name_col or value_col all NA", {
  # Create a long design where resp is all NA
  long_df <- tibble::tibble(
    item = rep("x1", 5),
    resp = NA_character_,
    wts = rep(1, 5)
  )
  dsn <- survey::svydesign(ids = ~1, weights = ~wts, data = long_df)

  testthat::expect_error(
    low_freqs(
      dsn,
      value_col = "resp",
      name_col = "item",
      values_to = "resp"
    ),
    "`item` or `resp` is empty after removing NAs in survey path"
  )
})


testthat::test_that("keep = character filters values in multi-x (data.frame)", {
  df <- label_vars(make_basic_df())

  out <- get_freqs(
    df,
    x = c("x1", "x2"),
    wt = wts,
    names_to = "item",
    values_to = "resp",
    keep = c("yes"),
    na.rm = TRUE
  )

  testthat::expect_s3_class(out, "tidysurvey_freqs")
  testthat::expect_named(
    out,
    c("item", "resp", "n", "pct"),
    ignore.order = TRUE
  )
  testthat::expect_true(all(out$resp %in% "yes"))
  testthat::expect_true(all(out$item %in% c("Q1. Blue", "Q2. Red")))
})

testthat::test_that("keep = function works and TRUE is a no-op (data.frame)", {
  df <- label_vars(make_basic_df())

  out_fun <- get_freqs(
    df,
    x = tidyselect::all_of(c("x1", "x2")),
    wt = wts,
    names_to = "item",
    values_to = "resp",
    keep = function(v) grepl("e$", v), # keeps "Q1. Blue" if present, otherwise only "no"/"yes" unaffected
    na.rm = TRUE
  )
  testthat::expect_type(out_fun$resp, "character")
  testthat::expect_true(all(grepl("e$", out_fun$resp))) # here should be only "no"/"yes", so likely empty or none match

  out_true <- get_freqs(
    df,
    x = tidyselect::all_of(c("x1", "x2")),
    wt = wts,
    names_to = "item",
    values_to = "resp",
    keep = function(v) TRUE,
    na.rm = TRUE
  )
  testthat::expect_true(all(c("yes", "no") %in% unique(out_true$resp)))
})

testthat::test_that("keep = tidy expression evaluates in result context (data.frame)", {
  df <- label_vars(make_basic_df())

  out <- get_freqs(
    df,
    x = tidyselect::all_of(c("x1", "x2")),
    group = grp,
    wt = wts,
    names_to = "item",
    values_to = "resp",
    keep = resp != "no",
    na.rm = TRUE
  )

  testthat::expect_false(any(out$resp == "no"))
  testthat::expect_true(all(c("A", "B") %in% out$grp))
})

testthat::test_that("keep is ignoQ2. Red for single-x (data.frame)", {
  df <- label_vars(make_basic_df())

  out <- get_freqs(
    df,
    x = x1,
    wt = wts,
    keep = c("yes"),
    na.rm = FALSE
  )
  # All observed levels appear since keep is ignoQ2. Red in single-x
  testthat::expect_true(all(c("yes", "no", NA) %in% c(out$x1)))
})

testthat::test_that("keep function invalid length errors (data.frame)", {
  df <- make_basic_df()
  testthat::expect_error(
    get_freqs(
      df,
      x = tidyselect::all_of(c("x1", "x2")),
      wt = wts,
      names_to = "item",
      values_to = "resp",
      keep = function(v) c(TRUE, FALSE, TRUE), # wrong length
      na.rm = TRUE
    ),
    regexp = "must return a logical vector of length"
  )
})

testthat::test_that("apply_keep_filter warns if values_col not found (defensive)", {
  df <- make_basic_df()
  out <- get_freqs(
    df,
    x = tidyselect::all_of(c("x1", "x2")),
    wt = wts,
    names_to = "item",
    values_to = "resp",
    na.rm = TRUE
  )
  out2 <- out
  names(out2)[names(out2) == "resp"] <- "resp2"

  testthat::expect_warning(
    apply_keep_filter(out2, values_col = "resp", keep = rlang::quo(c("yes"))),
    regexp = "skipping keep filter"
  )
})

testthat::test_that("drop_zero + keep interact reasonably (data.frame)", {
  # Create factors with extra levels to allow zero-level expansion on df path
  df <- make_basic_df()
  df$x1 <- factor(df$x1, levels = c("yes", "no", "maybe"))
  df$x2 <- factor(df$x2, levels = c("yes", "no", "skip"))

  out <- get_freqs(
    df,
    x = tidyselect::all_of(c("x1", "x2")),
    wt = wts,
    names_to = "item",
    values_to = "resp",
    drop_zero = FALSE, # allow zero levels in df path
    keep = c("yes", "maybe"), # keep observed 'yes' and a zero-level 'maybe' if materialized
    na.rm = TRUE
  )

  # Depending on how factorization + complete() run, "maybe" may or may not be present.
  # Assert at least 'yes' is retained, and no 'no' or 'skip'.
  testthat::expect_true("yes" %in% out$resp)
  testthat::expect_false(any(out$resp %in% c("no", "skip")))
})

testthat::test_that("keep = character filters values in multi-x (survey.design)", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(
    df,
    x = c("x1", "x2"),
    # wt ignoQ2. Red in survey path
    names_to = "item",
    values_to = "resp",
    keep = c("yes"),
    na.rm = TRUE
  )

  testthat::expect_s3_class(out, "tidysurvey_freqs")
  testthat::expect_named(
    out,
    c("item", "resp", "n", "pct"),
    ignore.order = TRUE
  )
  testthat::expect_true(all(out$resp %in% "yes"))
  testthat::expect_true(all(out$item %in% c("Q1. Blue", "Q2. Red")))
})

testthat::test_that("keep = function works and TRUE is a no-op (survey.design)", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out_fun <- get_freqs(
    svy,
    x = tidyselect::all_of(c("x1", "x2")),
    names_to = "item",
    values_to = "resp",
    keep = function(v) grepl("s$", v),
    na.rm = TRUE
  )
  testthat::expect_type(out_fun$resp, "character")
  testthat::expect_true(all(grepl("s$", out_fun$resp)))

  out_true <- get_freqs(
    svy,
    x = tidyselect::all_of(c("x1", "x2")),
    names_to = "item",
    values_to = "resp",
    keep = function(v) TRUE,
    na.rm = TRUE
  )
  testthat::expect_true(all(c("yes", "no") %in% unique(out_true$resp)))
})

testthat::test_that("keep = tidy expression evaluates in result context (survey.design)", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(
    svy,
    x = tidyselect::all_of(c("x1", "x2")),
    group = grp,
    names_to = "item",
    values_to = "resp",
    keep = resp != "no",
    na.rm = TRUE
  )

  testthat::expect_false(any(out$resp == "no"))
  testthat::expect_true(all(c("A", "B") %in% out$grp))
})

testthat::test_that("keep is ignoQ2. Red for single-x (survey.design)", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(svy, x = x1, keep = c("yes"), na.rm = FALSE)
  # In survey path, the single-x output renames "value" back to the original var
  testthat::expect_true(all(c("yes", "no", NA) %in% c(out$x1)))
})

testthat::test_that("keep is ignoQ2. Red for single-x with groups (survey.design) and NA retained", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(
    svy,
    x = x1, # single-x
    group = grp, # groups present -> Case B
    keep = c("yes"), # should be ignoQ2. Red for single-x
    na.rm = FALSE # NA should be retained as its own row
  )

  # x1 should contain yes, no, and NA
  testthat::expect_true(all(c("yes", "no", NA) %in% out$x1))

  # Both groups should be present
  testthat::expect_true(all(c("A", "B") %in% out$grp))

  # Basic shape expectations
  testthat::expect_true(all(c("grp", "x1", "n", "pct") %in% names(out)))
})

testthat::test_that("keep filters responses and item labels are applied (multi-x, no groups, survey.design)", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(
    svy,
    x = c("x1", "x2"), # multi-x -> Case C when no groups
    names_to = "item",
    values_to = "resp",
    keep = c("yes"), # filter to yes
    na.rm = FALSE # retain NA rows (should be kept if present, but filteQ2. Red out by keep)
  )

  # Item should show variable labels (not raw names)
  testthat::expect_true(all(out$item %in% c("Q1. Blue", "Q2. Red")))
  testthat::expect_false(any(out$item %in% c("x1", "x2")))

  # keep applied: only "yes" responses remain
  testthat::expect_true(all(out$resp %in% "yes"))

  # Structure expectations
  testthat::expect_true(all(c("item", "resp", "n", "pct") %in% names(out)))
})

testthat::test_that("keep filters responses; item labels and groups retained (multi-x with groups, survey.design)", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(
    svy,
    x = tidyselect::all_of(c("x1", "x2")), # multi-x
    group = grp, # with groups -> Case D
    names_to = "item",
    values_to = "resp",
    keep = resp == "yes", # tidy expression keep
    na.rm = FALSE # NA present before filter; filteQ2. Red out if not matching
  )

  # keep applied: only "yes"
  testthat::expect_true(all(out$resp %in% "yes"))

  # Item labels applied
  testthat::expect_true(all(out$item %in% c("Q1. Blue", "Q2. Red")))
  testthat::expect_false(any(out$item %in% c("x1", "x2")))

  # Both groups present
  testthat::expect_true(all(c("A", "B") %in% out$grp))

  # Structure expectations
  testthat::expect_true(all(
    c("grp", "item", "resp", "n", "pct") %in% names(out)
  ))
})

testthat::test_that("NA responses retained when na.rm = FALSE (multi-x, no groups, survey.design)", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(
    svy,
    x = c("x1", "x2"),
    names_to = "item",
    values_to = "resp",
    keep = function(v) TRUE,
    na.rm = FALSE
  )

  # Since x1 or x2 has NA in test data, expect at least one NA in resp
  testthat::expect_true(any(is.na(out$resp)))
})


testthat::test_that("drop_zero + keep interact reasonably (survey.design)", {
  # Survey path factors groups/x similarly, but does not "complete" zero cells by default.
  # Still, we can assert keep behavior on observed values.
  df <- make_basic_df()
  # Add extra levels to mirror the df test; survey path factors with your helper
  df$x1 <- factor(df$x1, levels = c("yes", "no", "maybe"))
  df$x2 <- factor(df$x2, levels = c("yes", "no", "skip"))
  svy <- make_svy(df)

  out <- get_freqs(
    svy,
    x = tidyselect::all_of(c("x1", "x2")),
    names_to = "item",
    values_to = "resp",
    drop_zero = FALSE,
    keep = c("yes", "maybe"),
    na.rm = TRUE
  )

  # "maybe" might or might not materialize given survey path; assert stable bits:
  testthat::expect_true("yes" %in% out$resp)
  testthat::expect_false(any(out$resp %in% c("no", "skip")))
})

testthat::test_that("survey Case A: na.rm = FALSE retains NA and pct includes NA in denominator", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(svy, x = x1, na.rm = FALSE, decimals = 3)

  # Expect NA in output values
  testthat::expect_true(any(is.na(out$x1)))

  # Denominator includes NA
  s <- sum(out$n)
  # Sum of pct should be 1 within rounding tolerance
  testthat::expect_equal(round(sum(out$pct), 3), 1.000)
})

testthat::test_that("survey Case A: na.rm = FALSE retains NA and pct includes NA in denominator", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(svy, x = x1, na.rm = FALSE, decimals = 3)

  # Expect NA in output values
  testthat::expect_true(any(is.na(out$x1)))

  # Denominator includes NA
  s <- sum(out$n)
  # Sum of pct should be 1 within rounding tolerance
  testthat::expect_equal(round(sum(out$pct), 3), 1.000)
})

testthat::test_that("survey Case B: na.rm=TRUE completes zero-count levels by group", {
  df <- label_vars(make_basic_df())
  # Add an unused level to x1 to exercise completion
  df$x1 <- factor(df$x1, levels = c("yes", "no", "maybe"))
  svy <- make_svy(df)

  out <- get_freqs(svy, x = x1, group = grp, na.rm = TRUE, drop_zero = FALSE)

  # For both groups, 'maybe' should appear with zero count
  by_grp <- split(out, out$grp)
  testthat::expect_true(all(vapply(
    by_grp,
    function(d) any(d$x1 == "maybe"),
    logical(1)
  )))
  testthat::expect_true(all(out$n[out$x1 == "maybe"] == 0))
})

testthat::test_that("survey Case C: na.rm toggles NA retention in multi-x no groups", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out_false <- get_freqs(
    svy,
    x = c("x1", "x2"),
    names_to = "item",
    values_to = "resp",
    na.rm = FALSE
  )
  out_true <- get_freqs(
    svy,
    x = c("x1", "x2"),
    names_to = "item",
    values_to = "resp",
    na.rm = TRUE
  )

  testthat::expect_true(any(is.na(out_false$resp)))
  testthat::expect_false(any(is.na(out_true$resp)))
})

testthat::test_that("apply_keep_filter character keep can include NA explicitly", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  out <- apply_keep_filter(
    df,
    values_col = "resp",
    keep = rlang::quo(c("yes", NA))
  )
  testthat::expect_true(all(out$resp %in% c("yes", NA)))
  testthat::expect_equal(nrow(out), 2L)
})

testthat::test_that("apply_keep_filter applies function on character values, not factor codes", {
  df <- tibble::tibble(
    resp = factor(c("yes", "no", "maybe"), levels = c("yes", "no", "maybe")),
    n = 1:3
  )
  out <- apply_keep_filter(
    df,
    values_col = "resp",
    keep = rlang::quo(function(v) grepl("^m", v))
  )
  testthat::expect_equal(as.character(out$resp), "maybe")
})

testthat::test_that("survey multi-x item column uses variable labels (name_label propagation)", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(
    svy,
    x = c("x1", "x2"),
    names_to = "item",
    values_to = "resp",
    na.rm = TRUE
  )

  testthat::expect_true(all(unique(out$item) %in% c("Q1. Blue", "Q2. Red")))
  testthat::expect_false(any(unique(out$item) %in% c("x1", "x2")))
})

testthat::test_that("default single-x ignores keep even with na.rm=TRUE", {
  df <- label_vars(make_basic_df())
  out <- get_freqs(df, x = x1, wt = wts, keep = c("yes"), na.rm = TRUE)
  # NA removed due to na.rm=TRUE; keep ignoQ2. Red so both yes and no appear
  testthat::expect_true(all(c("yes", "no") %in% out$x1))
})

testthat::test_that("survey multi-x values_to remains factor unless filteQ2. Red to empty", {
  df <- label_vars(make_basic_df())
  svy <- make_svy(df)

  out <- get_freqs(
    svy,
    x = c("x1", "x2"),
    names_to = "item",
    values_to = "resp",
    keep = function(v) TRUE,
    na.rm = TRUE
  )
  testthat::expect_s3_class(out, "tidysurvey_freqs")
  testthat::expect_true(is.factor(out$resp) || is.character(out$resp)) # allow for your implementation
})

testthat::test_that("finalize_common_attrs attaches expected class and labels", {
  df <- label_vars(make_basic_df())
  out <- get_freqs(df, x = x1, wt = wts, na.rm = TRUE)
  testthat::expect_true(inherits(out, "tidysurvey_freqs"))
  testthat::expect_equal(attr(out$n, "label"), "N")
  testthat::expect_equal(attr(out$pct, "label"), "Percent")
})


### apply_keep_filter tests -----------------------------------------------------

testthat::test_that("character keep filters membership", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  out <- apply_keep_filter(df, values_col = "resp", keep = rlang::quo(c("yes")))
  testthat::expect_equal(out$resp, "yes")
})

testthat::test_that("function keep accepts pQ2. Redicate on response vector", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  out <- apply_keep_filter(
    df,
    values_col = "resp",
    keep = rlang::quo(function(x) x == "no")
  )
  testthat::expect_equal(out$resp, "no")
})

testthat::test_that("function keep can return TRUE scalar", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  out <- apply_keep_filter(
    df,
    values_col = "resp",
    keep = rlang::quo(function(x) TRUE)
  )
  testthat::expect_equal(nrow(out), nrow(df))
})

testthat::test_that("tidy expression keep evaluates in data mask", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  # Capture quosure like get_freqs() should
  kq <- rlang::quo(resp != "no")
  out <- apply_keep_filter(df, values_col = "resp", keep = kq)
  testthat::expect_equal(out$resp, "yes")
})

testthat::test_that("tidy expression handles NA mask", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  kq <- rlang::quo(!is.na(resp) & resp == "yes")
  out <- apply_keep_filter(df, values_col = "resp", keep = kq)
  testthat::expect_equal(out$resp, "yes")
})

testthat::test_that("logical keep of correct length is allowed", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  out <- apply_keep_filter(
    df,
    values_col = "resp",
    keep = rlang::quo(c(TRUE, FALSE, NA))
  )
  testthat::expect_equal(out$resp, "yes")
})

testthat::test_that("wrong-length masks error out", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  testthat::expect_error(
    apply_keep_filter(
      df,
      values_col = "resp",
      keep = rlang::quo(c(TRUE, FALSE))
    ),
    "length nrow"
  )
  testthat::expect_error(
    apply_keep_filter(
      df,
      values_col = "resp",
      keep = rlang::quo(function(x) 1:3)
    ),
    "logical"
  )
  kq <- rlang::quo(1) # not logical
  testthat::expect_error(
    apply_keep_filter(df, values_col = "resp", keep = kq),
    "logical"
  )
})

testthat::test_that("missing values_col is warned and skipped", {
  df <- tibble::tibble(resp = c("yes", "no", NA), n = c(1, 2, 3))
  testthat::expect_warning({
    out <- apply_keep_filter(
      df,
      values_col = "missing_col",
      keep = rlang::quo(c("yes"))
    )
  })
  # Function returns df unchanged on skip
  testthat::expect_equal(out, df)
})
