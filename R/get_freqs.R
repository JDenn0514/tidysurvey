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
#' @param names_to A string specifying the name of the column to create
#'   to hold the variable names (or labels) when `x` selects multiple variables.
#'   Default is `"name"`.
#' @param values_to A string specifying the name of the column to create
#'   to hold the responses when `x` selects multiple variables.
#'   Default is `"value"`.

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
#'   If `FALSE`, zero-count levels will be included by using `tidyr::complete()`.
#'   Default is `FALSE`.
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
  group = NULL,
  wt = NULL,
  names_to = "name",
  values_to = "value",
  name_label = NULL,
  keep = NULL,
  drop_zero = FALSE,
  decimals = 1,
  na.rm = TRUE
) {
  UseMethod("get_freqs")
}

#' @export
get_freqs.data.frame <- function(
  data,
  x,
  group = NULL,
  wt = NULL,
  names_to = "name",
  values_to = "value",
  name_label = NULL,
  keep = NULL,
  drop_zero = FALSE,
  decimals = 1,
  na.rm = TRUE
) {
  # 1. Validation & Selection
  prep <- prep_freqs_data(
    data,
    {{ x }},
    {{ group }},
    {{ wt }},
    is_survey = FALSE
  )

  df_work <- prep$data
  x_cols <- prep$x_cols
  group_names <- prep$group_names
  wt_name <- prep$wt_name
  is_multi <- length(x_cols) > 1

  # Detect Common Labels (for ordering output later)
  common_val_labs <- if (is_multi) {
    get_common_value_labels(data, x_cols)
  } else {
    NULL
  }

  # 2. Factorize First (Corrects values 1 -> "Yes")
  for (col in x_cols) {
    df_work[[col]] <- make_factor(
      df_work[[col]],
      drop_levels = drop_zero,
      force = TRUE,
      na.rm = na.rm
    )
  }

  if (length(group_names) > 0) {
    for (col in group_names) {
      df_work[[col]] <- make_factor(
        df_work[[col]],
        drop_levels = drop_zero,
        force = TRUE,
        na.rm = na.rm
      )
    }
  }

  # 3. Reshaping
  if (is_multi) {
    df_work <- df_work %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(x_cols),
        names_to = names_to,
        values_to = values_to
      )

    # --- FIX 1: Factorize Names (Item) Column ---
    # Order by the original order of x_cols
    x_labels_map <- attr_var_label(
      data[x_cols],
      unlist = TRUE,
      if_null = "name"
    )

    # We set the levels to the labels in the order of x_cols
    # and map the values simultaneously
    df_work[[names_to]] <- factor(
      df_work[[names_to]],
      levels = x_cols,
      labels = x_labels_map
    )

    # --- FIX 2: Factorize Values (Response) Column ---
    # If variables shared a scale, preserve that factor order
    if (!is.null(common_val_labs)) {
      # make_factor converts codes to labels (Names of the named vector)
      # So the desired levels are the Names of common_val_labs
      desired_levels <- names(common_val_labs)

      # Ensure existing values match desired levels before converting
      # (They should, because we ran make_factor earlier)
      df_work[[values_to]] <- factor(
        as.character(df_work[[values_to]]),
        levels = desired_levels
      )
    } else {
      # If mixed scales, convert to character to be safe
      df_work[[values_to]] <- as.character(df_work[[values_to]])
    }

    calc_val_col <- values_to
    calc_group_cols <- c(group_names, names_to)
  } else {
    calc_val_col <- x_cols
    calc_group_cols <- group_names
  }

  # 4. Calculation
  if (na.rm) {
    check_cols <- c(calc_group_cols, calc_val_col)
    df_work <- df_work[stats::complete.cases(df_work[, check_cols]), ]
    if (nrow(df_work) == 0) {
      cli::cli_abort("After removing NAs, no rows remain.")
    }
  }

  out <- df_work %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      calc_group_cols,
      calc_val_col
    )))) %>%
    dplyr::summarise(
      n = if (!is.null(wt_name)) sum(.data[[wt_name]]) else dplyr::n(),
      .groups = "drop_last"
    )

  # 5. Completion & Pct
  if (!drop_zero) {
    out <- out %>%
      tidyr::complete(
        !!rlang::sym(calc_val_col),
        fill = list(n = 0)
      )
  } else {
    out <- out %>% dplyr::filter(n > 0)
  }

  out <- out %>%
    dplyr::mutate(
      n = round(n, decimals),
      pct = round(n / sum(n), decimals + 2)
    ) %>%
    dplyr::ungroup()

  filter_col <- if (is_multi) values_to else x_cols[1]

  # 6. Attributes & Finalize
  if (!rlang::quo_is_null(rlang::enquo(keep))) {
    out <- apply_keep_filter(out, filter_col, rlang::enquo(keep))
  }

  out <- attach_group_labels(out, data, group_names)

  if (is_multi) {
    out <- attach_multi_x_attrs(
      out,
      data,
      rlang::enexpr(x),
      x_cols,
      names_to,
      name_label
    )
  } else {
    out <- attach_single_x_attrs(out, data, rlang::enexpr(x), x_cols[1])
  }

  finalize_common_attrs(out, data)
}

#' @export
get_freqs.survey.design <- function(
  data,
  x,
  group = NULL,
  wt = NULL,
  names_to = "name",
  values_to = "value",
  name_label = NULL,
  keep = NULL,
  drop_zero = FALSE,
  decimals = 1,
  na.rm = TRUE
) {
  if (!requireNamespace("srvyr", quietly = TRUE)) {
    cli::cli_abort("srvyr required.")
  }

  prep <- prep_freqs_data(
    data,
    {{ x }},
    {{ group }},
    wt = NULL,
    is_survey = TRUE
  )
  x_cols <- prep$x_cols
  group_names <- prep$group_names
  is_multi <- length(x_cols) > 1

  d_svy <- if (inherits(data, "tbl_svy")) data else srvyr::as_survey(data)

  # Check common labels for ordering later
  common_val_labs <- if (is_multi) {
    get_common_value_labels(data$variables, x_cols)
  } else {
    NULL
  }
  x_labels <- attr_var_label(
    d_svy$variables[x_cols],
    unlist = TRUE,
    if_null = "name"
  )

  # Iterate
  calc_list <- lapply(x_cols, function(curr_x) {
    calc_survey_single_var(
      d_svy = d_svy,
      x_name = curr_x,
      group_names = group_names,
      x_label_val = x_labels[[curr_x]],
      names_to = names_to,
      values_to = values_to,
      na.rm = na.rm,
      drop_zero = drop_zero,
      is_multi = is_multi
    )
  })

  out <- dplyr::bind_rows(calc_list)
  if (nrow(out) == 0) {
    cli::cli_abort("After removing NAs, no rows remain for survey path.")
  }

  # Re-Factorize Output Columns (Multi-X only)
  if (is_multi) {
    out[[names_to]] <- factor(out[[names_to]], levels = x_labels)
    if (!is.null(common_val_labs)) {
      out[[values_to]] <- factor(
        out[[values_to]],
        levels = names(common_val_labs)
      )
    }
  }

  # Arrange Rows
  sort_cols <- c(group_names)
  if (is_multi) {
    sort_cols <- c(sort_cols, names_to)
  }
  sort_cols <- c(sort_cols, values_to)

  out <- out %>% dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))

  # Recalculate Pct
  pct_group_vars <- group_names
  if (is_multi) {
    pct_group_vars <- c(pct_group_vars, names_to)
  }

  if (length(pct_group_vars) > 0) {
    out <- out %>% dplyr::group_by(dplyr::across(dplyr::all_of(pct_group_vars)))
  }

  out <- out %>%
    dplyr::mutate(
      n = round(n, decimals),
      pct = round(n / sum(n), decimals + 2)
    ) %>%
    dplyr::ungroup()

  # Rename Single X columns back BEFORE filtering
  if (!is_multi) {
    # Force rename 'value' -> 'x1'.
    # We use dplyr::rename for safety; if 'values_to' isn't there, it will error informatively.
    out <- out %>%
      dplyr::rename(!!rlang::sym(x_cols[1]) := !!rlang::sym(values_to))

    # Restore factor levels for single variable output
    # (Because we converted to character in calc_survey_single_var)
    orig_fac <- data$variables[[x_cols[1]]]
    if (!is.null(attr(orig_fac, "labels"))) {
      attr(out[[x_cols[1]]], "labels") <- attr(orig_fac, "labels")
    }

    # Re-factorize to ensure levels are ordered correctly according to the design
    out[[x_cols[1]]] <- make_factor(
      out[[x_cols[1]]],
      force = TRUE,
      drop_levels = drop_zero
    )
  }

  # Apply Keep Filter
  # We determine the column to filter based on mode.
  filter_col <- if (is_multi) values_to else x_cols[1]

  if (!rlang::quo_is_null(rlang::enquo(keep))) {
    out <- apply_keep_filter(out, filter_col, rlang::enquo(keep))
  }

  # Attributes & Finalize
  out <- attach_group_labels(out, data$variables, group_names)

  if (is_multi) {
    final_cols <- c(group_names, names_to, values_to, "n", "pct")
    out <- out %>% dplyr::select(dplyr::any_of(final_cols), dplyr::everything())
    out <- attach_multi_x_attrs(
      out,
      data$variables,
      rlang::enexpr(x),
      x_cols,
      names_to,
      name_label
    )
  } else {
    final_cols <- c(group_names, x_cols, "n", "pct")
    out <- out %>% dplyr::select(dplyr::any_of(final_cols), dplyr::everything())
    out <- attach_single_x_attrs(out, data$variables, rlang::enexpr(x), x_cols)
  }

  finalize_common_attrs(out, data)
}

#' @export
get_freqs.svyrep.design <- get_freqs.survey.design
#' @export
get_freqs.tbl_svy <- get_freqs.survey.design


# analysis helpers -------------------------------------------------------

calc_survey_single_var <- function(
  d_svy,
  x_name,
  group_names,
  x_label_val,
  names_to,
  values_to,
  na.rm,
  drop_zero,
  is_multi
) {
  # 1. Filter NA
  if (na.rm) {
    d_svy <- tidyr::drop_na(
      d_svy,
      tidyselect::all_of(x_name)
    )

    if (length(group_names) > 0) {
      d_svy <- tidyr::drop_na(
        d_svy,
        tidyselect::all_of(group_names)
      )
    }

    # --- FIX: Check for empty design ---
    # srvyr crashes if we summarise an empty design.
    # Return NULL so bind_rows skips this variable.
    if (nrow(d_svy) == 0) return(NULL)
  }

  # 2. Group & Summarise
  calc_groups <- c(group_names, x_name)

  res <- d_svy %>%
    srvyr::group_by(dplyr::across(dplyr::all_of(calc_groups))) %>%
    srvyr::summarise(
      n = srvyr::survey_total(vartype = NULL),
      .groups = "drop_last"
    )

  # 3. Attributes & Factorize
  # Re-attach attributes so make_factor works on the summary column
  orig_var <- d_svy$variables[[x_name]]
  if (!is.null(attr(orig_var, "labels"))) {
    attr(res[[x_name]], "labels") <- attr(orig_var, "labels")
  }
  if (!is.null(attr(orig_var, "label"))) {
    attr(res[[x_name]], "label") <- attr(orig_var, "label")
  }

  res[[x_name]] <- make_factor(
    res[[x_name]],
    drop_levels = drop_zero,
    force = TRUE,
    na.rm = FALSE # NAs already filtered
  )

  # Rename
  res <- res %>% dplyr::rename(!!values_to := !!rlang::sym(x_name))

  # 4. Completion
  if (!drop_zero) {
    # Use complete without nesting to ensure unobserved levels appear (n=0)
    res <- res %>%
      tidyr::complete(!!rlang::sym(values_to), fill = list(n = 0))
  } else {
    res <- res %>% dplyr::filter(n > 0)
  }

  # 5. Standardize (Char for binding)
  res[[values_to]] <- as.character(res[[values_to]])

  if (is_multi) {
    res[[names_to]] <- x_label_val
  }

  return(res)
}


# helper functions  ------------------------------------------------------

prep_freqs_data <- function(data, x, group, wt, is_survey = FALSE) {
  # Extract data frame for name resolution
  df_for_names <- if (is_survey) data$variables else data

  # Resolve X columns
  x_cols <- tidyselect::eval_select(rlang::enquo(x), df_for_names)
  if (length(x_cols) == 0) {
    cli::cli_abort("x must select at least one column.")
  }
  x_cols <- names(x_cols)

  # Resolve Group columns
  group_names <- compose_group_names(df_for_names, {{ group }})

  # Ensure Groups aren't in X
  if (any(group_names %in% x_cols)) {
    cli::cli_warn(
      "Variables found in both `x` and `group`. Removing them from `x`."
    )
    x_cols <- setdiff(x_cols, group_names)
  }

  # Resolve Weight (only for DF)
  wt_name <- NULL
  data_out <- df_for_names # Default

  if (!is_survey) {
    wt_res <- ensure_weight(data, {{ wt }})
    data_out <- wt_res$data
    wt_name <- wt_res$wt_name
  }

  list(
    data = data_out, # DF with weight col added
    x_cols = x_cols,
    group_names = group_names,
    wt_name = wt_name
  )
}

get_common_value_labels <- function(data, cols) {
  # Extract labels for all columns
  all_labs <- lapply(cols, function(col) {
    attr(data[[col]], "labels", exact = TRUE)
  })

  # Check if list is empty
  if (length(all_labs) == 0) {
    return(NULL)
  }

  # Check if all are identical to the first one
  first_lab <- all_labs[[1]]

  # If first is NULL, they must all be NULL to be "common" (but irrelevant)
  if (is.null(first_lab)) {
    return(NULL)
  }

  is_common <- all(vapply(
    all_labs,
    function(x) identical(x, first_lab),
    logical(1)
  ))

  if (is_common) first_lab else NULL
}

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
