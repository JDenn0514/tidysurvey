#' Compute weighted frequencies, optionally grouped and/or across multiple variables
#'
#' @description
#' `get_freqs()` computes weighted frequency tables for survey-style data. It
#' supports:
#'
#' - Plain data frames with an optional weight column
#'
#' - `survey.design` and `svyrep.design` objects (from the survey/srvyr ecosystem)
#'
#' - Single-variable or multi-variable inputs
#'
#' - Optional grouping variables
#'
#' - Optional inclusion/exclusion of zero-count levels (see Limitations for
#'   survey multi-variable)
#'
#' For single-variable inputs, the response column in the output retains the
#' original variable name. For multi-variable inputs, responses are pivoted to
#' long format using `names_to` and `values_to`.
#'
#' @param data A data frame/tibble, `survey.design`, or `svyrep.design` object.
#' @param x <[`tidy-select`][dplyr_tidy_select]> Columns selecting one or more
#'   variables. You can pass:
#'
#'   - A bare column name (e.g., `x = q1`)
#'
#'   - A tidyselect expression (e.g., `x = tidyselect::starts_with("q")`)
#'
#'   - A vector of strings or symbols `c("q1", "q2")` or `c(q1, q2)`.
#'
#'   - An object containing a vector `tidyselect::all_of(variables)`.
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns
#'   to group the data. This operates very similarly to `.by` from dplyr
#'   (for more info on that see [?dplyr_by][dplyr_by]). It can also be a
#'   character vector. If using an external character vector it must be
#'   wrapped in curly brackets (`{{}}`).
#'
#'   In addition, grouped data can be piped in via `dplyr::group_by()` or
#'   `srvyr::group_by()`. If data is a `grouped_df` and `group` is provided,
#'   `get_means()` will combine the variable(s) used in either `group_by`
#'   function and the variable(s) supplied in `group` to calculate
#'   frequencies.
#' @param wt Optional weight column (numeric). Ignored for `survey.design`
#'  or `svyrep.design` inputs, where weights come from the design. If omitted
#'  for data frames, will output unweighted frequencies.
#' @param names_to A character vector specifying the new column or columns to
#'   create from the information stored in the column names of `data` specified
#'   by `cols`.  Default is `"values"`.
#'
#'   - If length 0, or if `NULL` is supplied, no columns will be created.
#'
#'   - If length 1, a single column will be created which will contain the
#'     column names specified by `cols`.
#'
#'   - If length >1, multiple columns will be created. In this case, one of
#'     `names_sep` or `names_pattern` must be supplied to specify how the
#'     column names should be split. There are also two additional character
#'     values you can take advantage of:
#'
#'     - `NA` will discard the corresponding component of the column name.
#'
#'     - `".value"` indicates that the corresponding component of the column
#'       name defines the name of the output column containing the cell values,
#'       overriding `values_to` entirely.
#' @param values_to A string specifying the name of the column to create
#'   from the data stored in cell values. If `names_to` is a character
#'   containing the special `.value` sentinel, this value will be ignored,
#'   and the name of the value column will be derived from part of the
#'   existing column names. Default is `"values"`
#' @param name_label Optional label to attach to the `names_to` column in
#'   multi-variable outputs (e.g., a question preface). If missing, will check
#'   the data for a `question_preface` attribute.
#' @param keep Optional post-aggregation filter applied only to multi-variable outputs:
#'
#'   - character vector: keep only rows where `values_to` is in this set
#'
#'   - function: predicate on the `values_to` vector; returns logical mask (length
#'     `nrow` or scalar `TRUE`)
#'
#'   - tidy expression: a dplyr-style filter expression evaluated in the result
#'     context
#' @param drop_zero Logical; whether to drop zero-count rows from the output.
#'
#'   - Default path (data.frame): combined with `dplyr::count(.drop = drop_zero)` to
#'     control inclusion of zero levels.
#'
#'   - Survey path (`survey.design`):
#'
#'     - Single-variable: zero-count response levels can be included when
#'       `drop_zero = FALSE`.
#'
#'     - Multi-variable: zero-count levels are not kept at this time.
#' @param decimals Number of decimal places for rounding counts (`n`). Percent
#'   (`pct`) is rounded to `decimals + 2` so that it contains the right number
#'   of decimals when multiplying by 100.
#' @param na.rm Logical; whether to remove rows with missing values in `x` and `group`
#'   before computing frequencies. Default is `TRUE`.
#'
#' @details
#' The `keep` argument is applied after aggregation in multi-variable outputs to filter
#' rows of the result based on the response column (`values_to`). It is ignored for
#' single-variable calls. Accepted forms:
#'
#' - Character vector: `keep = c("yes", "no")`
#'
#' - Function: `keep = \(v) v %in% c("yes", "no")` or any predicate that returns a
#'   logical vector of length `nrow(result)` or a single `TRUE` (no filtering). `NA`
#'   entries are dropped.
#'
#' - Tidy expression: `keep = .data[[values_to]] %in% c("yes","no")` or simply
#'   `resp != "skip"` when `values_to = "resp"`.
#'
#' Note: For tidy expressions, the expression is evaluated in the context of the
#' result tibble.
#'
#' @return A tibble with columns:
#'
#'   - For single-variable inputs: `[x variable]`, `n`, `pct`, and any grouping
#'     columns.
#'
#'   - For multi-variable inputs: `[grouping columns if any]`, `[names_to]`,
#'     `[values_to]`, `n`, `pct`.
#'
#'   The result has class `"tidysurvey_freqs"` and common attributes:
#'
#'   - `attr(., "dataset")`: the original dataset
#'
#'   - `attr(., "variable_label")`, `attr(., "variable_name")`
#'
#'   - For grouped outputs: `attr(., "group_names")` and `attr(., "group_labels")`
#'
#'   - For multi-variable: `attr(., "item_names")`, `attr(., "item_labels")`,
#'     `attr(., "x_expr")`
#'
#' @section Methods:
#' - `get_freqs.default()`: Operates on data frames/tibbles. If `wt` is omitted,
#'   simple unweighted frequencies are reported. Calculation is from `dplyr::count()`
#'   with `.drop = drop_zero`; zero-count levels can be included when
#'   `drop_zero = FALSE` and the variables are factors with unused levels.
#'
#' - `get_freqs.survey.design()`: Operates on `survey.design` objects. Weights are taken from
#'   the design. Grouping is honored inside low-level survey computations.
#'
#' - `get_freqs.svyrep.design()`: Operates on `svyrep.design` objects.
#'
#' @section Limitations (survey.design with multiple variables):
#' For multi-variable `survey.design` inputs (`x` selects multiple variables), zero-count response
#' levels are not currently expanded. Results include only observed levels per item, regardless of
#' `drop_zero`. This differs from the default (non-survey) path. For single-variable
#' `survey.design` inputs, zero-count levels can be included when `drop_zero = FALSE`.
#'
#' @examples
#' # here's a basic unweighted frequency for satisfaction_service
#' get_freqs(df, x = satisfaction_service)
#'
#' # now check it with weights
#' get_freqs(df, x = satisfaction_service, wt = wts)
#'
#' # now check grouped
#' get_freqs(df, x = satisfaction_service, group = grp, wt = wts)
#'
#' # groups can also be added by using `group_by()` ahead of time
#' df |>
#'   dplyr::group_by() |>
#'   get_freqs(df, x = satisfaction_service, wt = wts)
#'
#' # Now check with multiple x variables
#' get_freqs(
#'   df,
#'   x = c("x1", "x2"),
#'   wt = wts,
#'   na.rm = TRUE
#' )
#'
#' # rename the outputs
#' get_freqs(
#'   df,
#'   x = c("x1", "x2"),
#'   wt = wts,
#'   names_to = "item",
#'   values_to = "resp",
#'   na.rm = TRUE
#' )
#'
#' # now check multiple x variables with a grouping variable
#' get_freqs(
#'   df,
#'   x = c("x1", "x2"),
#'   group = "grp",
#'   wt = wts,
#'   na.rm = TRUE
#' )
#'
#' # ---- keep examples (multi-variable, data frame) ----
#'
#' # 1) keep as a character vector: retain only "yes" responses across items
#' get_freqs(
#'   df,
#'   x = c("x1", "x2"),
#'   wt = wts,
#'   keep = c("yes"),
#'   na.rm = TRUE
#' )
#'
#' # Survey design  ------------
#' @examplesIf requireNamespace("srvyr", quietly = TRUE)
#' df_svy <- srvyr::as_survey_design(
#'    ids = id,
#'    strata = strata,
#'    weights = "wts",
#'    .data = _
#'  )
#'
#' # basic example
#' get_freqs(df_svy, x = satisfaction_service)
#'
#' # multi-variable example
#' get_freqs(df_svy, c(x1, x2))
#'
#' # grouped example
#' get_freqs(df_svy, satisfaction_service, grp)
#'
#' # grouped example with multi-variable
#' get_freqs(df_svy, c(x1, x2), grp)
#'
#'
#' @seealso
#' - [survey::svydesign()], [srvyr::as_survey()]
#' - [dplyr::count()], [tidyr::pivot_longer()]
#'
#' @export
get_freqs <- function(
  data,
  x,
  group,
  wt,
  names_to = "names",
  values_to = "values",
  name_label,
  keep,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  UseMethod("get_freqs")
}


# ---- default method ----
#' @export
get_freqs.default <- function(
  data,
  x,
  group,
  wt,
  names_to = "names",
  values_to = "values",
  name_label,
  keep,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  # Capture the 'keep' argument as a quosure so we can evaluate later if needed
  keep_quo <- rlang::enquo(keep)

  # Capture the original expression passed to x (for attaching attributes later)
  x_expr <- rlang::enexpr(x)

  # Resolve x into column names (supports tidyselect input)
  x_cols <- get_col_names(data, {{ x }})
  # get the groups specified by dplyr::group_by()
  existing_groups <- dplyr::group_vars(data)
  # remove groups from x_cols
  x_cols <- setdiff(x_cols, existing_groups)

  # return an error if no columns given
  if (!length(x_cols)) {
    cli::cli_abort("x must select at least one column.")
  } # guard: need ≥1 column

  # Resolve group columns from tidyselect, in the order they should appear
  group_names <- compose_group_names(data, {{ group }})
  # merge the groups (preserve order: explicit > existing, then unique)
  group_names <- unique(c(group_names, existing_groups))
  # ensure no groups are in x_cols
  x_cols <- setdiff(x_cols, group_names)

  # Cache group labels (variable labels) from the pristine data; used to reattach later
  if (length(group_names)) {
    cached_group_labels <- attr_var_label(data[, group_names], if_null = "name")
  } else {
    cached_group_labels <- character(0)
  }

  # Prepare weights: returns possibly modified data plus the resolved weight column name
  wt_res <- ensure_weight(data, {{ wt }})
  data <- wt_res$data
  wt_name <- wt_res$wt_name

  # Flag whether we are in the multi-variable path (more than one x column)
  is_multi <- length(x_cols) > 1

  # Cache the variable label for single-x so we can reattach it later
  cached_x_label <- if (!is_multi) attr_var_label(data[[x_cols[1]]]) else NULL

  # ---------------------------
  # 1) PIVOT (when multi-x)
  # ---------------------------
  if (is_multi) {
    # Select the columns we need for analysis: x variables, groups, and weights
    keep_cols <- unique(c(x_cols, group_names, wt_name))

    narrow <- data[, keep_cols, drop = FALSE]

    # Pivot from wide (multiple x columns) to long with one 'values_to' and one 'names_to'
    # name_label is forwarded so pivot can label the item column appropriately
    long <- pivot_longer_values(
      data = narrow,
      cols = tidyselect::all_of(x_cols),
      names_to = {{ names_to }},
      values_to = {{ values_to }},
      name_label = name_label
    )

    # ---------------------------
    # 2) FACTORIZE (multi path)
    # ---------------------------
    # Factorize the long data frame:
    # - values_to: turn into labeled factor using value labels (if provided)
    # - names_to: turn item names into labeled factor using variable labels
    # - group_names: also factorize group vars with label-aware rules
    long <- factorize_multi_path(
      df = long,
      values_to = values_to,
      names_to = names_to,
      group_names = group_names,
      drop_zero = drop_zero,
      na.rm = na.rm
    )

    # If your pipeline separates "group-only" factorization semantics, ensure consistency here
    long <- factorize_multi_groups_only(long, group_names, drop_zero, na.rm)

    # Establish working dataset and the logical column roles for downstream steps
    data_work <- long
    value_col <- values_to # response column in long format
    name_col <- names_to # item column in long format
  } else {
    # ---------------------------
    # SINGLE-X: no pivot needed
    # ---------------------------
    # Keep only required columns for analysis: x, groups, and weights
    keep_cols <- unique(c(x_cols[1], group_names, wt_name))

    data_work <- data[, keep_cols, drop = FALSE]

    # ---------------------------
    # 2) FACTORIZE (single path)
    # ---------------------------
    # Factorize the x variable into a labeled factor and factorize groups
    data_work <- factorize_single_path(
      data_work,
      x_name = x_cols[1],
      group_names = group_names,
      drop_zero = drop_zero,
      na.rm = na.rm
    )

    # Establish roles for downstream
    value_col <- x_cols[1] # single response column
    name_col <- NULL # no item column in single-x
  }

  # ------------------------------------------
  # 3) REMOVE NAs AFTER factorization
  # ------------------------------------------
  # Important: Do this after factorization so labels/levels are not lost
  if (na.rm) {
    # Determine which columns to check for completeness
    chk <- unique(
      if (is_multi) c(values_to, group_names) else c(value_col, group_names)
    )
    # Assert we won't drop everything (helps produce a friendly error)
    assert_nonempty_after_filter(data_work, chk, context = "default path")
    if (length(chk)) {
      # Subset to complete cases across the selected analysis columns
      data_work <- data_work[
        stats::complete.cases(data_work[, chk, drop = FALSE]),
      ]
    }
  }

  # ---------------------------
  # 4) GROUP
  # ---------------------------
  if (length(group_names)) {
    # Group by explicit group variables (if any were supplied)
    data_work <- dplyr::group_by(
      data_work,
      dplyr::across(tidyselect::all_of(group_names))
    )
  } else if (is_multi) {
    # If no explicit groups but multi-x, group by item so low_freqs can compute per item
    data_work <- dplyr::group_by(data_work, .data[[names_to]])
  }

  # ---------------------------
  # 5) low_freqs
  # ---------------------------
  # Compute weighted frequencies; low_freqs reads grouping from data_work’s dplyr groups
  out <- low_freqs(
    data = data_work,
    value_col = value_col,
    name_col = name_col,
    wt = wt_name,
    drop_zero = drop_zero,
    decimals = decimals,
    values_to = values_to
  )

  # Apply keep filter on multi-x outputs if requested
  if (is_multi && !rlang::quo_is_missing(keep_quo)) {
    # use keep to filter values output
    out <- apply_keep_filter(out, values_col = values_to, keep = keep_quo)
  }

  # Reattach group labels (variable labels on group columns)
  out <- attach_group_labels(out, data, group_names)

  # Reattach x variable metadata (labels, original x expression)
  if (is_multi) {
    out <- attach_multi_x_attrs(
      out,
      data,
      x_expr,
      x_cols = x_cols,
      names_to = names_to,
      name_label = name_label
    )
  } else {
    out <- attach_single_x_attrs(
      out,
      data,
      x_expr,
      value_col,
      precomputed_label = cached_x_label
    )
  }

  # Finalize shared attributes (e.g., dataset reference, class tagging)
  out <- finalize_common_attrs(out, dataset = data)
  out
}

# ---- survey.design method ----
#' @export
get_freqs.survey.design <- function(
  data,
  x,
  group,
  wt,
  names_to = "names",
  values_to = "values",
  name_label,
  keep,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  # Capture keep for later evaluation
  keep_quo <- rlang::enquo(keep)

  # Capture x expression for attribute attachment
  x_expr <- rlang::enexpr(x)

  # Work on the variables data frame inside the survey design
  survey_data <- data$variables

  # Resolve x columns
  x_cols <- get_col_names(survey_data, {{ x }})
  # get the groups specified by dplyr::group_by()
  existing_groups <- dplyr::group_vars(survey_data)
  # remove groups from x_cols
  x_cols <- setdiff(x_cols, existing_groups)
  # If name_label not provided, derive a reasonable default from the first x var
  if (missing(name_label)) {
    name_label <- attr_question_preface(survey_data[[x_cols[1]]])
  }

  # Guard: need ≥1 column
  if (!length(x_cols)) {
    cli::cli_abort("x must select at least one column.")
  }

  # Resolve group columns from tidyselect
  group_names <- compose_group_names(survey_data, {{ group }})
  # merge the groups (preserve order: explicit > existing, then unique)
  group_names <- unique(c(group_names, existing_groups))

  # ensure again that now groups are in x_cols
  x_cols <- setdiff(x_cols, group_names)

  # Cache group labels from pristine survey data, so we can reattach later
  if (length(group_names)) {
    cached_group_labels <- attr_var_label(
      survey_data[, group_names],
      if_null = "name"
    )
  } else {
    cached_group_labels <- character(0)
  }

  # Multi-x flag
  is_multi <- length(x_cols) > 1

  # Cache the variable label for single-x
  cached_x_label <- if (!is_multi) {
    attr_var_label(survey_data[[x_cols[1]]])
  } else {
    NULL
  }

  if (is_multi) {
    # --------------------------------
    # 1) PIVOT the survey design
    # --------------------------------
    # Note: pivot_longer_values should handle survey.design, returning a new design with
    # variables pivoted long. This preserves weights/ids in the design object.
    long_design <- pivot_longer_values(
      data = data,
      cols = tidyselect::all_of(x_cols),
      names_to = {{ names_to }},
      values_to = {{ values_to }},
      name_label = name_label
    )

    # --------------------------------
    # 2) FACTORIZE on the variables frame
    # --------------------------------
    # Extract the long variables data frame for factorization
    long_df <- long_design$variables

    # Factorize values_to, names_to (item), and group variables using label-aware rules
    long_df <- factorize_multi_path(
      df = long_df,
      values_to = values_to,
      names_to = names_to,
      group_names = group_names,
      drop_zero = drop_zero,
      na.rm = na.rm
    )

    # Ensure group-only factorization semantics are consistent if needed
    long_df <- factorize_multi_groups_only(
      long_df,
      group_names,
      drop_zero,
      na.rm
    )

    # Write factorized variables back into the survey design
    long_design$variables <- long_df

    # Set up working design and column roles for downstream steps
    data_work <- long_design
    value_col <- values_to # response column name (in variables)
    name_col <- names_to # item column name (in variables)

    # --------------------------------
    # 3) REMOVE NAs AFTER factorization (subset the design)
    # --------------------------------
    if (na.rm) {
      # Identify columns to use for completeness check
      chk <- unique(c(values_to, group_names))
      # Assert we won't drop everything, using the variables df in the design
      assert_nonempty_after_filter(
        data_work$variables,
        chk,
        context = "survey path"
      )
      if (length(chk)) {
        # Determine complete rows in the variables
        keep_rows <- stats::complete.cases(data_work$variables[,
          chk,
          drop = FALSE
        ])
        # Subset the survey design by these rows (preserves weights/design metadata)
        data_work <- data_work[keep_rows, ]
      }
    }

    # --------------------------------
    # 4) GROUP (on variables only, if groups supplied)
    # --------------------------------
    # srvyr/low_freqs will use by_cols, but we set dplyr groups on variables for consistency
    if (length(group_names)) {
      data_work$variables <- dplyr::group_by(
        data_work$variables,
        dplyr::across(tidyselect::all_of(group_names))
      )
    }
  } else {
    # --------------------------------
    # SINGLE-X path
    # --------------------------------
    # 1) No pivot needed; we work on the design as-is
    # 2) FACTORIZE the single x var and groups on the variables frame
    x1 <- x_cols[1]
    survey_data <- factorize_single_path(
      survey_data,
      x1,
      group_names,
      drop_zero,
      na.rm
    )
    data$variables <- survey_data
    data_work <- data
    value_col <- x1
    name_col <- NULL

    # 3) REMOVE NAs AFTER factorization by subsetting the design
    if (na.rm) {
      chk <- unique(c(x1, group_names))
      assert_nonempty_after_filter(
        data_work$variables,
        chk,
        context = "survey path"
      )
      if (length(chk)) {
        keep_rows <- stats::complete.cases(data_work$variables[,
          chk,
          drop = FALSE
        ])
        data_work <- data_work[keep_rows, ]
      }
    }

    # 4) GROUP variables if group columns were supplied
    if (length(group_names)) {
      data_work$variables <- dplyr::group_by(
        data_work$variables,
        dplyr::across(tidyselect::all_of(group_names))
      )
    }
  }

  # --------------------------------
  # 5) low_freqs (survey path)
  # --------------------------------
  # Compute weighted frequencies for survey data; 'wt' is ignored since the design carries weights.
  out <- low_freqs(
    data = data_work,
    value_col = value_col,
    name_col = name_col,
    wt = NULL, # ignored for survey.design
    drop_zero = drop_zero,
    decimals = decimals,
    na.rm = na.rm,
    values_to = values_to
  )

  # Apply keep filter on multi-x outputs if requested
  if (is_multi && !rlang::quo_is_missing(keep_quo)) {
    # use keep to filter values output
    out <- apply_keep_filter(out, values_col = values_to, keep = keep_quo)
  }

  # Reattach group labels (variable labels for group columns)
  out <- attach_group_labels(
    out,
    data,
    group_names,
    precomputed_labels = cached_group_labels
  )

  # Variable label attributes and shape harmonization
  if (is_multi) {
    # Attach multi-x attributes (e.g., mapping from x_expr/x_cols to names_to)
    out <- attach_multi_x_attrs(
      out,
      data,
      x_expr,
      x_cols = x_cols,
      names_to = names_to,
      name_label = name_label
    )
  } else {
    # For single-x, standardize: rename "value" back to the original x var for compatibility
    value_col <- x_cols[1]
    names(out)[names(out) == "value"] <- value_col
    out <- attach_single_x_attrs(
      out,
      data,
      x_expr,
      value_col,
      precomputed_label = cached_x_label
    )
  }

  # Finalize object attributes (dataset reference, classes, etc.)
  out <- finalize_common_attrs(out, dataset = data)
  out
}


# low level frequency function -------------------------------------------

low_freqs <- function(
  data,
  value_col, # name of the value column in the input (long), often same as values_to passed by caller
  name_col = NULL, # name of the item column in the input (long) for multi-x (e.g., names_to), or NULL for single-x
  wt = NULL, # ignored for survey
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE,
  values_to = NULL # NEW: explicit output column name for the response (recommended to pass the same values_to used upstream)
) {
  # Backward compatibility: if values_to is missing, use value_col as the output name
  values_to <- if (is.null(values_to)) value_col else values_to

  is_svy <- inherits(data, "survey.design")

  if (!is_svy) {
    # if not a survey object:

    # rename data as df
    df <- data
    # get the group variables
    group_vars <- dplyr::group_vars(df)
    # if name_col is not NULL, include it in group_vars
    if (!is.null(name_col)) {
      group_vars <- c(group_vars, name_col)
    }

    # calculate frequencies
    out <- df |>
      dplyr::count(
        dplyr::across(tidyselect::all_of(group_vars)),
        .data[[value_col]],
        wt = if (!is.null(wt)) .data[[wt]] else NULL,
        .drop = drop_zero,
        name = "n"
      )

    # clean up the response column name
    if (!is.null(name_col)) {
      # if name_col is not null, it is multi_x
      # if values_to is NULL use value_col as resp_col_name, if it's not NULL use values_to as resp_col_name
      resp_col_name <- value_col
    } else {
      # if name_col is NULL or missing or something use value_col
      resp_col_name <- value_col
    }

    if (!(resp_col_name %in% names(out))) {
      # if resp_col_name is not in out, then replace value_col with resp_col_name
      names(out)[names(out) == value_col] <- resp_col_name
    }

    # create the final frequencies df
    out <- out |>
      # group by all group_vars
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
      # create n an pct
      dplyr::mutate(
        n = round(n, decimals),
        pct = round(n / sum(n), decimals + 2)
      ) |>
      # ungroup the data
      dplyr::ungroup()

    cols_front <- unique(c(group_vars, resp_col_name))
    out <- out[, c(cols_front, "n", "pct"), drop = FALSE]

    # return out
    return(out)
  }

  # Survey path
  d <- data
  d_vars <- d$variables
  gvars <- dplyr::group_vars(d_vars)
  has_groups <- length(gvars) > 0
  has_item <- !is.null(name_col)

  if (!has_groups && !has_item) {
    # Case A: single x, no groups

    if (!value_col %in% names(d$variables)) {
      cli::cli_abort("`{value_col}` not found in survey design variables.")
    }
    vc <- d$variables[[value_col]]
    if (all(is.na(vc))) {
      cli::cli_abort(
        "`{value_col}` is empty after removing NAs in survey path."
      )
    }

    # 1) Convert design to srvyr object
    ds <- srvyr::as_survey(d)

    # 2) Only drop NA observations when na.rm = TRUE
    if (isTRUE(na.rm)) {
      ds <- ds |> dplyr::filter(!is.na(.data[[value_col]]))
    }

    # 3) Group by the value column (includes NA group when na.rm = FALSE)
    out <- ds |>
      dplyr::group_by(.data[[value_col]]) |>
      srvyr::summarise(
        n = srvyr::survey_total(vartype = NULL),
        .groups = "drop"
      )

    # 4) Optional factor ordering (does not remove NA)
    vlevs <- levels(d$variables[[value_col]])
    if (!is.null(vlevs)) {
      out[[value_col]] <- factor(out[[value_col]], levels = vlevs)
    }

    # 5) Only complete across non-NA levels when na.rm = TRUE
    if (isTRUE(na.rm) && !is.null(vlevs)) {
      out <- tidyr::complete(
        out,
        !!rlang::sym(value_col),
        fill = list(n = 0)
      )
    }

    # 6) Percentages and optional zero-drop
    s <- sum(out$n)
    out$n <- round(out$n, decimals)
    out$pct <- if (s > 0) round(out$n / s, decimals + 2) else NA_real_

    if (drop_zero) out <- out[out$n > 0, , drop = FALSE]
  } else if (has_groups && !has_item) {
    # Case B: single x, with groups

    ds <- srvyr::as_survey(d)

    # Only drop NA observations when na.rm = TRUE
    if (isTRUE(na.rm)) {
      ds <- ds |> dplyr::filter(!is.na(.data[[value_col]]))
    }

    out <- ds |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(
        gvars,
        value_col
      )))) |>
      srvyr::summarise(
        n = srvyr::survey_total(vartype = NULL),
        .groups = "drop"
      )

    # Factor ordering for the value column (does not remove NA)
    vlevs <- levels(d$variables[[value_col]])
    if (!is.null(vlevs)) {
      out[[value_col]] <- factor(out[[value_col]], levels = vlevs)
    }

    # Complete across groups x value levels only when na.rm = TRUE
    if (isTRUE(na.rm) && !is.null(vlevs)) {
      out <- out |>
        tidyr::complete(
          tidyr::nesting(!!!rlang::syms(gvars)),
          !!rlang::sym(value_col),
          fill = list(n = 0)
        )
    }

    out <- out |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(gvars))) |>
      dplyr::mutate(
        n = round(n, decimals),
        pct = round(n / sum(n), decimals + 2)
      ) |>
      dplyr::ungroup()

    if (drop_zero) out <- out[out$n > 0, , drop = FALSE]
  } else if (!has_groups && has_item) {
    # Case C: multi x, no groups (name_col present)

    if (
      !value_col %in% names(d$variables) || !name_col %in% names(d$variables)
    ) {
      cli::cli_abort(
        "`{name_col}` or `{value_col}` not found in survey design variables."
      )
    }
    if (
      all(is.na(d$variables[[value_col]])) ||
        all(is.na(d$variables[[name_col]]))
    ) {
      cli::cli_abort(
        "`{name_col}` or `{value_col}` is empty after removing NAs in survey path."
      )
    }

    ds <- srvyr::as_survey(d)

    # Only drop NA responses when na.rm = TRUE; keep NA otherwise
    if (isTRUE(na.rm)) {
      ds <- ds |> dplyr::filter(!is.na(.data[[value_col]]))
    }

    out <- ds |>
      dplyr::group_by(.data[[name_col]], .data[[value_col]]) |>
      srvyr::summarise(
        n = srvyr::survey_total(vartype = NULL),
        .groups = "drop"
      )

    # Factor ordering for item and value (does not remove NA)
    item_levs <- levels(d$variables[[name_col]])
    val_levs <- levels(d$variables[[value_col]])
    if (!is.null(item_levs)) {
      out[[name_col]] <- factor(out[[name_col]], levels = item_levs)
    }
    if (!is.null(val_levs)) {
      out[[values_to]] <- {
        # Rename value_col to values_to here
        names(out)[names(out) == value_col] <- values_to
        factor(out[[values_to]], levels = val_levs)
      }
    }
    # If not renamed yet, rename now
    if (!(values_to %in% names(out))) {
      names(out)[names(out) == value_col] <- values_to
    }

    # Only complete across item x value (non-NA levels) when na.rm = TRUE
    if (isTRUE(na.rm) && !is.null(item_levs) && !is.null(val_levs)) {
      out <- tidyr::complete(
        out,
        !!rlang::sym(name_col),
        !!rlang::sym(values_to),
        fill = list(n = 0)
      )
    }

    out <- out |>
      dplyr::group_by(.data[[name_col]]) |>
      dplyr::mutate(
        n = round(as.numeric(.data$n), decimals),
        pct = round(.data$n / sum(.data$n), decimals + 2)
      ) |>
      dplyr::ungroup()

    if (drop_zero) out <- out[out$n > 0, , drop = FALSE]
  } else {
    # Case D: multi x, with groups

    ds <- srvyr::as_survey(d)

    # Only drop NA responses when na.rm = TRUE; keep NA otherwise
    if (isTRUE(na.rm)) {
      ds <- ds |> dplyr::filter(!is.na(.data[[value_col]]))
    }

    out <- ds |>
      dplyr::group_by(
        dplyr::across(
          tidyselect::all_of(
            c(
              gvars,
              name_col,
              value_col
            )
          )
        )
      ) |>
      srvyr::summarise(
        n = srvyr::survey_total(vartype = NULL),
        .groups = "drop"
      )

    # Rename value_col to values_to
    names(out)[names(out) == value_col] <- values_to

    # Factor ordering for value column (does not remove NA)
    vlevs <- levels(d$variables[[value_col]])
    if (!is.null(vlevs)) {
      out[[values_to]] <- factor(out[[values_to]], levels = vlevs)
    }

    # Only build the completion grid when na.rm = TRUE (non-NA levels only)
    if (isTRUE(na.rm)) {
      # Distinct combinations of groups + item (name_col)
      if (length(c(gvars, name_col))) {
        group_item_df <- dplyr::distinct(
          out,
          dplyr::across(tidyselect::all_of(c(gvars, name_col)))
        )
      } else {
        group_item_df <- tibble::tibble(.dummy = 1L)
      }

      # Distinct response values (non-NA observed set after filtering)
      values_df <- dplyr::distinct(out, !!rlang::sym(values_to))
      names(values_df) <- values_to

      # Build grid and join
      grid_df <- if (length(c(gvars, name_col))) {
        tidyr::crossing(group_item_df, values_df)
      } else {
        tidyr::crossing(values_df)
      }

      join_by <- c(
        stats::setNames(c(gvars, name_col), c(gvars, name_col)),
        stats::setNames(values_to, values_to)
      )
      out <- dplyr::left_join(grid_df, out, by = join_by) |>
        dplyr::mutate(n = dplyr::coalesce(.data$n, 0L))

      if (!length(c(gvars, name_col)) && ".dummy" %in% names(out)) {
        out <- dplyr::select(out, -".dummy")
      }
    }

    out <- out |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(gvars, name_col)))) |>
      dplyr::mutate(
        n = round(n, decimals),
        pct = round(n / sum(n), decimals + 2)
      ) |>
      dplyr::ungroup()

    if (drop_zero) out <- out[out$n > 0, , drop = FALSE]
  }

  if (all(c(values_to, "n", "pct") %in% names(out))) {
    cols <- names(out)
    cols <- cols[!cols %in% c(values_to, "n", "pct")]
    out <- out[, c(cols, values_to, "n", "pct"), drop = FALSE]
  }

  return(out)
}


# helpers ----------------------------------------------------------------

compose_group_names <- function(data, group) {
  if (inherits(data, "grouped_df")) {
    base_groups <- dplyr::group_vars(data)
  } else {
    base_groups <- character(0)
  }

  if (missing(group) || rlang::quo_is_missing(rlang::enquo(group))) {
    extra_groups <- character(0)
  } else {
    # Only call select_groups if non-missing
    extra_groups <- select_groups(
      {{ group }},
      if (inherits(data, "survey.design")) data$variables else data
    )
    extra_groups <- extra_groups[extra_groups != "c"]
  }

  unique(c(base_groups, extra_groups))
}


ensure_weight <- function(data, wt) {
  # Returns list(data = data_with_weight, wt_name = wt_col_name)

  # No wt supplied: create unit weights
  if (
    missing(wt) ||
      # is.null(wt) ||
      rlang::quo_is_missing(rlang::enquo(wt)) ||
      rlang::quo_is_null(rlang::enquo(wt))
  ) {
    wt_name <- "wts"
    data[[wt_name]] <- 1
    return(list(data = data, wt_name = wt_name))
  }

  wt_expr <- rlang::quo_squash(rlang::enexpr(wt))

  # Determine the column name from the expression
  if (rlang::is_symbol(wt_expr)) {
    # wt = wts
    wt_name <- rlang::as_name(wt_expr)
  } else if (rlang::is_string(wt_expr)) {
    # wt = "wts" (string literal in the call)
    wt_name <- rlang::as_string(wt_expr)
  } else {
    # In case wt was already evaluated to a character vector before reaching here
    wt_val <- rlang::eval_bare(wt_expr, env = rlang::caller_env())
    if (is.character(wt_val) && length(wt_val) == 1) {
      wt_name <- wt_val
    } else {
      cli::cli_abort(
        "`wt` must be a column name provided as a symbol (e.g., wt = wts) or a string (e.g., wt = \"wts\")."
      )
    }
  }

  if (!(wt_name %in% names(data))) {
    cli::cli_abort("Weight column `{wt_name}` not found in `data`.")
  }

  if (!is.numeric(data[[wt_name]])) {
    cli::cli_abort(c(
      "`{wt_name}` must be a numeric variable.",
      "x" = "Supplied variable is {class(data[[wt_name]])}."
    ))
  }

  data[[wt_name]][is.na(data[[wt_name]])] <- 0
  list(data = data, wt_name = wt_name)
}


# ---- factorization helpers ----

factorize_single_path <- function(df, x_name, group_names, drop_zero, na.rm) {
  x_label <- attr_var_label(df[[x_name]])
  value_labels <- attr_val_labels(df[[x_name]])

  if (is.numeric(df[[x_name]])) {
    labs <- sort(as.numeric(value_labels))
    vals <- sort(unique(as.numeric(df[[x_name]])))
  } else {
    labs <- sort(as.character(value_labels))
    vals <- sort(unique(as.character(df[[x_name]])))
  }

  if (!all(vals %in% labs)) {
    if (length(group_names)) {
      df[, group_names] <- lapply(
        df[, group_names, drop = FALSE],
        \(y) {
          make_factor(y, drop_levels = drop_zero, force = TRUE, na.rm = na.rm)
        }
      )
    }
  } else {
    df[[x_name]] <- make_factor(
      df[[x_name]],
      drop_levels = drop_zero,
      force = TRUE,
      na.rm = na.rm
    )
    if (length(group_names)) {
      df[, group_names] <- lapply(
        df[, group_names, drop = FALSE],
        \(y) {
          make_factor(y, drop_levels = drop_zero, force = TRUE, na.rm = na.rm)
        }
      )
    }
  }
  df
}

factorize_multi_path <- function(
  df,
  values_to,
  names_to,
  group_names = character(0),
  drop_zero = FALSE,
  na.rm = TRUE
) {
  # Build the set of grouping variables for coercion (names_to + any explicit groups)
  grp <- unique(c(names_to, group_names))

  # Build the set of grouping variables for coercion (names_to + any explicit groups)
  grp <- unique(c(group_names)) # exclude names_to here

  # 1) Factorize group vars with make_factor (always)
  if (length(grp)) {
    df[, grp] <- lapply(
      df[, grp, drop = FALSE],
      \(y) make_factor(y, drop_levels = drop_zero, force = TRUE, na.rm = na.rm)
    )
  }

  # 1b) Factorize names_to (item) more leniently
  if (!missing(names_to) && !is.null(names_to) && names_to %in% names(df)) {
    item_vec <- df[[names_to]]
    item_val_labs <- attr_val_labels(item_vec)
    if (!is.null(item_val_labs)) {
      # If it actually has value labels, use make_factor (strict, consistent)
      df[[names_to]] <- make_factor(
        item_vec,
        drop_levels = drop_zero,
        force = TRUE,
        na.rm = na.rm
      )
    } else {
      # No value labels on item names; do not call make_factor
      # Still turn into a factor (keep observed order to be stable)
      df[[names_to]] <- factor(item_vec, levels = unique(item_vec))
    }
  }

  # 2) Decide whether to factorize values_to
  # Try to extract value labels from values_to in the long df
  value_labels <- attr_val_labels(df[[values_to]])

  if (is.null(value_labels)) {
    # No value labels attached to the long column. Do NOT coerce values_to.
    return(df)
  }

  # Compute labs/vals according to type
  col_vec <- df[[values_to]]
  if (is.numeric(col_vec)) {
    labs <- sort(as.numeric(value_labels))
    vals <- sort(unique(as.numeric(col_vec)))
  } else {
    labs <- sort(as.character(value_labels))
    vals <- sort(unique(as.character(col_vec)))
  }

  # Only coerce values_to if all observed values have labels
  if (all(vals %in% labs)) {
    df[[values_to]] <- make_factor(
      df[[values_to]],
      drop_levels = drop_zero,
      force = TRUE,
      na.rm = na.rm
    )
  } else {
    # Optional: warn once (comment out if you prefer silent)
    cli::cli_warn(
      "Not all observed values in `{values_to}` have value labels; leaving as-is."
    )
  }

  df
}


factorize_multi_groups_only <- function(df, group_names, drop_zero, na.rm) {
  if (length(group_names)) {
    df[, group_names] <- lapply(
      df[, group_names, drop = FALSE],
      \(y) make_factor(y, drop_levels = drop_zero, force = TRUE, na.rm = na.rm)
    )
  }
  df
}

# ---- labeling helpers ----
attach_group_labels <- function(
  out,
  data,
  group_names,
  precomputed_labels = NULL
) {
  if (length(group_names)) {
    # if data is a survey object, extract the underlying data
    if ("survey.design" %in% class(data)) {
      data <- data$variables
    }

    # Use precomputed labels if supplied, else derive now
    if (is.null(precomputed_labels)) {
      group_labels <- attr_var_label(data[, group_names], if_null = "name")
    } else {
      # ensure named character with same names as group_names
      group_labels <- precomputed_labels[group_names]
      # any missing names, backfill from data to be safe
      missing_idx <- is.na(group_labels) | is.null(group_labels)
      if (any(missing_idx)) {
        fallback <- attr_var_label(
          data[, group_names[missing_idx]],
          if_null = "name"
        )
        group_labels[names(fallback)] <- fallback
      }
    }

    # Attach labels to matching columns in the output
    for (y in names(group_labels)) {
      if (y %in% names(out)) {
        attr(out[[y]], "label") <- group_labels[[y]]
      }
    }

    # add group_names and group_labels attributes
    attr(out, "group_names") <- group_names
    attr(out, "group_labels") <- group_labels
  }
  out
}


attach_single_x_attrs <- function(
  out,
  data,
  x_expr,
  x_name,
  precomputed_label = NULL
) {
  x_label <- if (!is.null(precomputed_label)) {
    precomputed_label
  } else {
    attr_var_label(data[[x_name]])
  }
  if (!is.null(x_label) && x_name %in% names(out)) {
    attr(out[[x_name]], "label") <- x_label
    attr(out, "variable_label") <- x_label
    attr(out, "variable_name") <- x_expr
  } else {
    attr(out, "variable_label") <- x_expr
    attr(out, "variable_name") <- x_expr
  }
  out
}


# Updated: supports name_label & names_to-driven attributes for multi-x
attach_multi_x_attrs <- function(
  out,
  data,
  x_expr,
  x_cols,
  names_to,
  name_label
) {
  # Per-item names and labels (keep this, it's useful downstream)
  item_names <- x_cols
  item_labels <- attr_var_label(
    data[, x_cols, drop = FALSE],
    unlist = TRUE,
    if_null = "name"
  )
  attr(out, "item_names") <- item_names
  attr(out, "item_labels") <- item_labels
  attr(out, "x_expr") <- x_expr

  # Set higher-level variable_label / variable_name consistent with your select-all function
  if (!missing(name_label)) {
    # User supplied
    if (names_to %in% names(out)) {
      attr(out[[names_to]], "label") <- name_label
    }
    attr(out, "variable_label") <- name_label
    attr(out, "variable_name") <- names_to
  } else {
    # Fall back to names_to as label and name (question preface was already used
    # by pivot_longer_values to label the column; we keep the data frame attributes here)
    attr(out, "variable_label") <- names_to
    attr(out, "variable_name") <- names_to
  }

  out
}

finalize_common_attrs <- function(out, dataset) {
  out <- tibble::as_tibble(out)
  attr(out$n, "label") <- "N"
  attr(out$pct, "label") <- "Percent"
  attr(out, "dataset") <- dataset
  class(out) <- c("tidysurvey_freqs", class(out))
  out
}

# ---- keep filter helper (for multi-x only) ----

# Apply a post-aggregation filter to the low_freqs() output
# - df is the already-computed result (has columns: value OR values_to, and maybe names_to)
# - values_to is the column name holding the response values in multi-x (default "values")
# - keep can be:
#     * character vector: keep rows where values column is in this set
#     * function: predicate on the values vector, returns logical mask
#     * tidy expression: dplyr-style filter expression evaluated in df context
#
# Notes:
# - For single-x: the column to filter is the x variable name, not "value"
#   (because you rename "value" back to x). So caller must pass the RIGHT column name.
# - For multi-x: we filter on the values_to column.
apply_keep_filter <- function(df, values_col, keep) {
  if (missing(keep) || is.null(keep) || rlang::quo_is_null(keep)) {
    return(df)
  }

  if (
    !is.character(values_col) ||
      length(values_col) != 1L ||
      !(values_col %in% names(df))
  ) {
    cli::cli_warn(
      "apply_keep_filter: column `{values_col}` not found; skipping keep filter."
    )
    return(df)
  }

  if (rlang::is_quosure(keep)) {
    # rlang is a quosure

    # get the expression within the quosure
    expr <- rlang::quo_get_expr(keep)
    env <- rlang::quo_get_env(keep)

    unwrap <- FALSE

    if (is.character(expr) || is.logical(expr)) {
      # Literal vectors
      unwrap <- TRUE
    } else if (rlang::is_symbol(expr)) {
      # A symbol that might reference a function or constant in env
      unwrap <- TRUE
    } else if (
      rlang::is_call(expr) && identical(rlang::call_name(expr), "function")
    ) {
      # NEW: detect function literal calls
      unwrap <- TRUE
    } else if (rlang::is_call(expr) && identical(rlang::call_name(expr), "c")) {
      # Calls: ONLY unwrap if it is c("a","b", ...) with all literal chars/logicals
      args <- as.list(expr)[-1]
      if (
        all(vapply(
          args,
          function(a) is.character(a) || is.logical(a),
          logical(1)
        ))
      ) {
        unwrap <- TRUE
      }
    }

    if (unwrap) {
      obj <- rlang::eval_tidy(keep, env = env) # evaluate without data
      if (is.character(obj) || is.function(obj) || is.logical(obj)) {
        keep <- obj
      }
    }
  }

  # 1) Character: membership on response
  if (is.character(keep)) {
    mask <- df[[values_col]] %in% keep
    mask <- !is.na(mask) & mask
    return(df[mask, , drop = FALSE])
  }

  # 2) Function: predicate on response vector
  if (is.function(keep)) {
    vals <- df[[values_col]]
    mask <- keep(vals)
    ok <- isTRUE(mask) || (is.logical(mask) && length(mask) == length(vals))
    if (!ok) {
      cli::cli_abort(
        "`keep` function must return a logical vector of length nrow(df) (or a TRUE scalar)."
      )
    }
    if (isTRUE(mask)) {
      return(df)
    }
    mask <- !is.na(mask) & mask
    return(df[mask, , drop = FALSE])
  }

  # 3) Quosure or formula: tidy expression evaluated in data mask
  if (rlang::is_quosure(keep)) {
    mask <- rlang::eval_tidy(keep, data = df)
    if (!is.logical(mask) || length(mask) != nrow(df)) {
      cli::cli_abort(
        "`keep` expression must evaluate to a logical vector of length nrow(df)."
      )
    }
    mask <- !is.na(mask) & mask
    return(df[mask, , drop = FALSE])
  }

  if (rlang::is_formula(keep)) {
    keep_q <- rlang::as_quosure(keep, env = rlang::caller_env())
    mask <- rlang::eval_tidy(keep_q, data = df)
    if (!is.logical(mask) || length(mask) != nrow(df)) {
      cli::cli_abort(
        "`keep` expression must evaluate to a logical vector of length nrow(df)."
      )
    }
    mask <- !is.na(mask) & mask
    return(df[mask, , drop = FALSE])
  }

  # 4) Logical vector (already computed)
  if (is.logical(keep)) {
    if (length(keep) == 1L && isTRUE(keep)) {
      return(df)
    }
    if (length(keep) != nrow(df)) {
      cli::cli_abort(
        "Logical `keep` must be length 1 (TRUE) or length nrow(df)."
      )
    }
    mask <- !is.na(keep) & keep
    return(df[mask, , drop = FALSE])
  }

  cli::cli_abort(
    "Unsupported `keep` type: {class(keep)}. Use character, function, tidy expression, or logical."
  )
}


assert_nonempty_after_filter <- function(df, vars, context = "data") {
  vars <- vars[vars %in% names(df)]
  if (!length(vars)) {
    return(invisible(TRUE))
  }
  keep_rows <- stats::complete.cases(df[, vars, drop = FALSE])
  if (!any(keep_rows)) {
    # Compose a readable message with variable names
    msg <- paste0(
      "After removing NAs, no rows remain for ",
      context,
      ". Variables causing empty result: ",
      paste(vars, collapse = ", ")
    )
    cli::cli_abort(msg)
  }
  invisible(TRUE)
}
