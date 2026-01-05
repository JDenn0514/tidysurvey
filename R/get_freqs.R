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
  name_label = NULL,
  keep,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  UseMethod("get_freqs")
}


# ---- default method ----

#' @export
get_freqs.data.frame <- function(
  data,
  x,
  group,
  wt,
  names_to = "names",
  values_to = "values",
  name_label = NULL,
  keep,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  keep_quo <- rlang::enquo(keep)

  prep <- prep_freqs_data(
    data = data,
    x = {{ x }},
    group = {{ group }},
    wt = {{ wt }},
    names_to = names_to,
    values_to = values_to,
    name_label = name_label,
    drop_zero = drop_zero,
    na.rm = na.rm,
    is_survey = FALSE
  )

  data_work <- prep$data
  is_multi <- prep$is_multi
  value_col <- prep$value_col
  name_col <- prep$name_col
  group_names <- prep$group_names
  wt_name <- prep$wt_name
  x_expr <- prep$x_expr
  cached_x_label <- prep$cached_x_label
  cached_group_labels <- prep$cached_group_labels
  names_to <- prep$names_to
  values_to <- prep$values_to
  name_label <- prep$name_label
  x_cols <- prep$x_cols

  # Grouping: explicit groups, and for multi‑x with no groups, group by item
  if (length(group_names)) {
    data_work <- dplyr::group_by(
      data_work,
      dplyr::across(tidyselect::all_of(group_names))
    )
  } else if (is_multi) {
    data_work <- dplyr::group_by(data_work, .data[[names_to]])
  }

  out <- df_freqs(
    data = data_work,
    value_col = value_col,
    name_col = if (is_multi) names_to else NULL,
    wt = wt_name,
    drop_zero = drop_zero,
    decimals = decimals,
    values_to = if (is_multi) values_to else value_col
  )
  # return(out)
  # keep filter only for multi‑x
  if (!rlang::quo_is_missing(keep_quo)) {
    out <- apply_keep_filter(out, values_col = value_col, keep = keep_quo)
  }

  out <- attach_group_labels(out, data, group_names)

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

  out <- finalize_common_attrs(out, dataset = data)
  out
}

#' @export
get_freqs.survey.design <- function(
  data,
  x,
  group,
  wt,
  names_to = "names",
  values_to = "values",
  name_label = NULL,
  keep,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  keep_quo <- rlang::enquo(keep)

  original_data <- data$variables

  prep <- prep_freqs_data(
    data = data,
    x = {{ x }},
    group = {{ group }},
    wt = NULL,
    names_to = names_to,
    values_to = values_to,
    name_label = name_label,
    drop_zero = drop_zero,
    na.rm = FALSE, # IMPORTANT: subset via design below
    is_survey = TRUE
  )

  design_work <- prep$design_work
  is_multi <- prep$is_multi
  x_cols <- prep$x_cols
  value_col <- prep$value_col # for multi: "resp", for single: "x1"
  name_col <- prep$name_col # for multi: "item"
  group_names <- prep$group_names
  x_expr <- prep$x_expr
  cached_x_label <- prep$cached_x_label
  cached_group_labels <- prep$cached_group_labels
  names_to <- prep$names_to
  values_to <- prep$values_to
  name_label <- prep$name_label

  # If na.rm: subset design_work using its variables
  if (na.rm) {
    keep_resp <- !is.na(design_work$variables[[value_col]])
    keep_resp[is.na(keep_resp)] <- FALSE
    design_work <- subset(design_work, keep_resp)

    if (nrow(design_work$variables) == 0L) {
      cli::cli_abort(
        "After removing NAs, no rows remain for survey path."
      )
    }

    if (length(group_names) > 0L) {
      keep_grp <- stats::complete.cases(design_work$variables[group_names])
      design_work <- subset(design_work, keep_grp)

      if (nrow(design_work$variables) == 0L) {
        cli::cli_abort(
          "After removing NAs, no rows remain for survey path."
        )
      }
    }
  }

  survey_data <- design_work$variables

  # Build grouping keys:
  key_cols <- if (is_multi) c(group_names, name_col) else group_names

  gi <- .grouping_index_freqs(survey_data, key_cols)

  # Build display keys: copy attrs (from original_data) THEN factorize
  display_keys <- copy_attrs(gi$keys, original_data, vars = key_cols)
  display_keys <- factorize_multi_groups_only(
    display_keys,
    group_names = key_cols,
    drop_zero = TRUE,
    na.rm = na.rm
  )

  # Compute grouped freqs
  out <- .survey_grouped_freqs(
    design = design_work,
    value_col = value_col,
    rows_by_group = gi$rows_by_group,
    drop_zero = drop_zero,
    decimals = decimals,
    na.rm = na.rm,
    values_to = if (is_multi) values_to else value_col,
    key_template = display_keys,
    key_names = key_cols
  )

  # Apply keep filter on the correct response column in the OUTPUT
  resp_out_col <- if (is_multi) values_to else value_col
  if (!rlang::quo_is_missing(keep_quo)) {
    out <- apply_keep_filter(out, values_col = resp_out_col, keep = keep_quo)
  }

  # Attach group labels (only real grouping vars, not item)
  out <- attach_group_labels(
    out,
    design_work,
    group_names,
    precomputed_labels = cached_group_labels
  )

  # Attach x metadata
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
      design_work,
      x_expr,
      value_col,
      precomputed_label = cached_x_label
    )
  }

  out <- finalize_common_attrs(out, dataset = design_work)
  out
}


# analysis helpers -------------------------------------------------------

df_freqs <- function(
  data,
  value_col,
  name_col = NULL,
  wt = NULL,
  drop_zero = FALSE,
  decimals = 3,
  values_to = NULL
) {
  values_to <- if (is.null(values_to)) value_col else values_to

  # group_vars = existing dplyr groups + optional item column
  group_vars <- dplyr::group_vars(data)
  if (!is.null(name_col)) {
    group_vars <- c(group_vars, name_col)
  }

  df <- data |>
    dplyr::count(
      dplyr::across(tidyselect::all_of(group_vars)),
      .data[[value_col]],
      wt = if (!is.null(wt)) .data[[wt]] else NULL,
      .drop = drop_zero,
      name = "n"
    )

  resp_col_name <- if (!is.null(values_to)) values_to else value_col

  if (!(resp_col_name %in% names(df))) {
    names(df)[names(df) == value_col] <- resp_col_name
  }

  df <- df |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
    dplyr::mutate(
      n = round(n, decimals),
      pct = round(n / sum(n), decimals + 2)
    ) |>
    dplyr::ungroup()

  cols_front <- unique(c(group_vars, resp_col_name))
  df <- df[, c(cols_front, "n", "pct"), drop = FALSE]

  df
}

svy_freqs <- function(
  design,
  value_col,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE,
  values_to = NULL
) {
  values_to <- if (is.null(values_to)) value_col else values_to

  # Work via srvyr, similar to your existing low_freqs survey branches
  ds <- srvyr::as_survey(design)

  if (isTRUE(na.rm)) {
    ds <- ds |> dplyr::filter(!is.na(.data[[value_col]]))
  }

  out <- ds |>
    dplyr::group_by(.data[[value_col]]) |>
    srvyr::summarise(
      n = srvyr::survey_total(vartype = NULL),
      .groups = "drop"
    )

  # Preserve factor ordering using original design levels
  vlevs <- levels(design$variables[[value_col]])
  if (!is.null(vlevs)) {
    out[[value_col]] <- factor(out[[value_col]], levels = vlevs)
  }

  # When na.rm = TRUE and we have factor levels, complete across all levels
  if (isTRUE(na.rm) && !is.null(vlevs)) {
    out <- tidyr::complete(
      out,
      !!rlang::sym(value_col),
      fill = list(n = 0)
    )
  }

  # Convert to numeric, compute pct
  s <- sum(out$n)
  out$n <- round(as.numeric(out$n), decimals)
  out$pct <- if (s > 0) round(out$n / s, decimals + 2) else NA_real_

  if (drop_zero) {
    out <- out[out$n > 0, , drop = FALSE]
  }

  # Rename response column if needed
  if (!(values_to %in% names(out))) {
    names(out)[names(out) == value_col] <- values_to
  }

  out
}

.grouping_index_freqs <- function(data, group_names) {
  n <- nrow(data)

  if (is.null(group_names) || length(group_names) == 0) {
    return(list(
      rows_by_group = list(seq_len(n)),
      keys = tibble::tibble(.group_id = 1L)
    ))
  }

  gkeys <- data[, group_names, drop = FALSE]

  # Group id per row (fast, vctrs)
  gid <- vctrs::vec_group_id(gkeys)

  # Split row indices by group id
  rows_by_group <- vctrs::vec_split(seq_len(n), gid)$val

  # Build display keys in a value-preserving way
  # This avoids vctrs proxy keys turning factors into integers.
  keys <- dplyr::distinct(tibble::as_tibble(gkeys))

  # Ensure the key order matches group id order:
  # compute the first occurrence of each gid and order keys by that.
  first_pos <- vapply(rows_by_group, function(idx) idx[[1]], integer(1))
  ord <- order(first_pos)

  list(
    rows_by_group = rows_by_group[ord],
    keys = keys[ord, , drop = FALSE]
  )
}


.survey_grouped_freqs <- function(
  design,
  value_col,
  rows_by_group,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE,
  values_to,
  # meta used to rebuild keys:
  key_template, # a tibble of keys (group columns and optionally item)
  key_names # character vector of column names for keys
) {
  n_tot <- nrow(design$variables)

  est_one_group <- function(rows) {
    if (is.numeric(rows)) {
      mask <- rep(FALSE, n_tot)
      idx <- rows[rows >= 1 & rows <= n_tot]
      if (length(idx) > 0L) mask[idx] <- TRUE
    } else {
      stop("rows must be integer indices")
    }

    d_sub <- subset(design, mask)

    out <- svy_freqs(
      design = d_sub,
      value_col = value_col,
      drop_zero = drop_zero,
      decimals = decimals,
      na.rm = na.rm,
      values_to = values_to
    )

    out
  }

  out_list <- lapply(rows_by_group, est_one_group)

  # Bind results with keys
  res <- dplyr::bind_rows(out_list)

  # Expand keys to match rows in res: each group’s freq table has rows,
  # we need to repeat keys for each set.
  key_ids <- rep(
    seq_len(nrow(key_template)),
    times = vapply(out_list, nrow, integer(1))
  )
  keys_rep <- key_template[key_ids, , drop = FALSE]

  # Bind keys and result
  out <- dplyr::bind_cols(keys_rep, res)

  # Order columns: keys, values_to, n, pct
  cols_front <- unique(c(key_names, values_to))
  cols_front <- cols_front[cols_front %in% names(out)]
  out <- out[, c(cols_front, "n", "pct"), drop = FALSE]

  out
}


# helper functions  ------------------------------------------------------

prep_freqs_data <- function(
  data,
  x,
  group = NULL,
  wt = NULL,
  names_to = "names",
  values_to = "values",
  name_label = NULL,
  drop_zero = FALSE,
  na.rm = TRUE,
  is_survey = FALSE
) {
  x_expr <- rlang::enexpr(x)

  # For survey inputs, resolve names against variables
  vars_df <- if (
    inherits(data, "survey.design") || inherits(data, "svyrep.design")
  ) {
    data$variables
  } else {
    data
  }

  # Resolve x column names
  x_cols <- get_col_names(vars_df, {{ x }})

  existing_groups <- if (inherits(vars_df, "grouped_df")) {
    dplyr::group_vars(vars_df)
  } else {
    character(0)
  }
  x_cols <- setdiff(x_cols, existing_groups)

  if (!length(x_cols)) {
    cli::cli_abort("x must select at least one column.")
  }

  # Resolve groups
  group_names <- compose_group_names(vars_df, {{ group }})
  group_names <- unique(c(group_names, existing_groups))
  x_cols <- setdiff(x_cols, group_names)

  # Cache group labels from pristine vars_df
  cached_group_labels <- if (length(group_names)) {
    attr_var_label(vars_df[, group_names, drop = FALSE], if_null = "name")
  } else {
    character(0)
  }

  # Weights only for non-survey
  if (!is_survey) {
    wt_res <- ensure_weight(vars_df, {{ wt }})
    df <- wt_res$data
    wt_name <- wt_res$wt_name
  } else {
    df <- vars_df
    wt_name <- NULL
  }

  is_multi <- length(x_cols) > 1
  cached_x_label <- if (!is_multi) attr_var_label(df[[x_cols[1]]]) else NULL

  # ---- SURVEY PATH ----
  if (is_survey) {
    if (is_multi) {
      if (
        is.null(name_label) ||
          (is.character(name_label) &&
            length(name_label) == 1 &&
            is.na(name_label))
      ) {
        # only default if user didn't supply
        name_label <- attr_question_preface(vars_df[[x_cols[1]]])
      }

      # # Pivot the DESIGN, not the variables df
      # long_design <- pivot_longer_values(
      #   data = data,
      #   cols = tidyselect::all_of(x_cols),
      #   names_to = names_to,
      #   values_to = values_to,
      #   name_label = name_label
      # )

      df <- data$variables

      long_df <- pivot_longer_values_core(
        df = df,
        cols = x_cols,
        names_to = names_to,
        values_to = values_to,
        name_label = name_label
      )

      long_design <- rebuild_srvyr_design(long_df, data)

      # sanity checks (highly recommended)
      if (!(names_to %in% names(long_design$variables))) {
        cli::cli_abort(
          "After pivot, column `{names_to}` is missing from design variables."
        )
      }
      if (!(values_to %in% names(long_design$variables))) {
        cli::cli_abort(
          "After pivot, column `{values_to}` is missing from design variables."
        )
      }

      # Factorize inside design variables
      long_df <- long_design$variables
      long_df <- factorize_multi_path(
        df = long_df,
        values_to = values_to,
        names_to = names_to,
        group_names = group_names,
        drop_zero = drop_zero,
        na.rm = na.rm
      )
      long_df <- factorize_multi_groups_only(
        long_df,
        group_names,
        drop_zero,
        na.rm
      )
      long_design$variables <- long_df

      return(list(
        design_work = long_design,
        is_multi = TRUE,
        x_cols = x_cols,
        value_col = values_to,
        name_col = names_to,
        group_names = group_names,
        wt_name = NULL,
        x_expr = x_expr,
        cached_x_label = NULL,
        cached_group_labels = cached_group_labels,
        names_to = names_to,
        values_to = values_to,
        name_label = name_label
      ))
    } else {
      # single-x: factorize variables inside design
      x1 <- x_cols[1]
      df2 <- df
      df2 <- factorize_single_path(
        df2,
        x_name = x1,
        group_names = group_names,
        drop_zero = drop_zero,
        na.rm = na.rm
      )

      data2 <- data
      data2$variables <- df2

      return(list(
        design_work = data2,
        is_multi = FALSE,
        x_cols = x_cols,
        value_col = x1,
        name_col = NULL,
        group_names = group_names,
        wt_name = NULL,
        x_expr = x_expr,
        cached_x_label = cached_x_label,
        cached_group_labels = cached_group_labels,
        names_to = names_to,
        values_to = values_to,
        name_label = NULL
      ))
    }
  }

  # ---- NON-SURVEY PATH (unchanged conceptually) ----
  if (is_multi) {
    keep_cols <- unique(c(x_cols, group_names, wt_name))
    narrow <- df[, keep_cols, drop = FALSE]

    if (missing(name_label) || is.null(name_label)) {
      name_label <- attr_question_preface(df[[x_cols[1]]])
    }

    long <- pivot_longer_values_core(
      data = narrow,
      cols = tidyselect::all_of(x_cols),
      names_to = names_to,
      values_to = values_to,
      name_label = name_label
    )

    long <- factorize_multi_path(
      long,
      values_to,
      names_to,
      group_names,
      drop_zero,
      na.rm
    )
    long <- factorize_multi_groups_only(long, group_names, drop_zero, na.rm)

    data_work <- long
    value_col <- values_to
    name_col <- names_to
  } else {
    keep_cols <- unique(c(x_cols[1], group_names, wt_name))
    data_work <- df[, keep_cols, drop = FALSE]
    data_work <- factorize_single_path(
      data_work,
      x_cols[1],
      group_names,
      drop_zero,
      na.rm
    )
    value_col <- x_cols[1]
    name_col <- NULL
    name_label <- NULL
  }

  if (na.rm) {
    chk <- unique(
      if (is_multi) c(values_to, group_names) else c(value_col, group_names)
    )
    assert_nonempty_after_filter(data_work, chk, context = "default path")
    data_work <- data_work[
      stats::complete.cases(data_work[, chk, drop = FALSE]),
      ,
      drop = FALSE
    ]
  }

  list(
    data = data_work,
    is_multi = is_multi,
    x_cols = x_cols,
    value_col = value_col,
    name_col = name_col,
    group_names = group_names,
    wt_name = wt_name,
    x_expr = x_expr,
    cached_x_label = cached_x_label,
    cached_group_labels = cached_group_labels,
    names_to = names_to,
    values_to = values_to,
    name_label = name_label
  )
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

  # 1) Factorize group vars with make_factor (always)
  if (length(group_names)) {
    df[, group_names] <- lapply(
      df[, group_names, drop = FALSE],
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

# Optionally reattach haven-style attrs from original_vars to display_keys
copy_attrs <- function(dst_df, src_df, vars) {
  for (v in vars) {
    if (v %in% names(dst_df) && v %in% names(src_df)) {
      lab <- attr_var_label(src_df[[v]])
      if (!is.null(lab)) {
        attr(dst_df[[v]], "label") <- lab
      }
      lbls <- attr_val_labels(src_df[[v]])
      if (!is.null(lbls)) {
        attr(dst_df[[v]], "labels") <- lbls
      }
      qp <- attr_question_preface(src_df[[v]])
      if (!is.null(qp)) {
        attr(dst_df[[v]], "question_preface") <- qp
      }
    }
  }
  dst_df
}
