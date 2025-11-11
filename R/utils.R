#' Select variables from the group variable
#' @param group `tidy_select` columns to group the data by
#' @param data The data set that the columns are from (this to make sure they exist)
#'
#' @keywords internal
select_groups <- function(group, data) {
  group_vars <- as.character(rlang::quo_squash(rlang::enexpr(group)))
  group_vars <- group_vars[group_vars != "c"]

  if (!all(group_vars %in% colnames(data))) {
    data_name <- substitute(data)
    group_vars <- group_vars[c(!group_vars %in% colnames(data))]
    cli::cli_abort(c(
      "Column `{group_vars}` is not found in `{data_name}`",
      "i" = "Make sure all variables supplied to {.var group} are present in {.var data}"
    ))
  }
  return(group_vars)
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


# recreate survey objects ------------------------------------------------

make_svydes_from_df <- function(df, design_vars) {
  # Start with the data
  survey_args <- list(.data = df)

  # Populate ids, strata, weights, fpc dynamically
  survey_args <- purrr::reduce(
    c("ids", "strata", "weights", "fpc"),
    ~ process_design(design_vars, .x, .y),
    .init = survey_args
  )

  # Determine if nesting is needed (overlapping PSUs in strata)
  nest_value <- FALSE
  if (!is.null(survey_args$ids) && !is.null(survey_args$strata)) {
    ids_var <- survey_args$ids[1]
    strata_var <- survey_args$strata[1]
    psu_strata_tab <- table(df[[ids_var]], df[[strata_var]])
    if (any(rowSums(psu_strata_tab > 0) > 1)) {
      nest_value <- TRUE
    }
  }
  if (nest_value) {
    survey_args$nest <- TRUE
  }

  print(str(survey_args))

  # Build the survey design
  do.call(srvyr::as_survey_design, survey_args)
}


process_design <- function(design_vars, survey_args, var_name) {
  if (var_name == "ids") {
    if (!is.null(design_vars$ids)) {
      ids <- rlang::as_name(design_vars$ids[[1]])
      if (ids == "1") {
        survey_args$ids <- 1
      } else if (grepl("\\+", ids)) {
        ids_vars <- strsplit(ids, split = " + ", fixed = TRUE)[[1]]
        survey_args$ids <- ids_vars
      } else {
        survey_args$ids <- ids
      }
    }
  } else {
    if (!is.null(design_vars[[var_name]])) {
      var <- design_vars[[var_name]][[1]]

      # Handle ids = 1 explicitly
      if (identical(var, 1) || identical(as.character(var), "1")) {
        survey_args[[var_name]] <- 1
      } else {
        var_chr <- rlang::as_name(var)
        if (grepl("\\+", var_chr)) {
          survey_args[[var_name]] <- strsplit(var_chr, " + ", fixed = TRUE)[[1]]
        } else {
          survey_args[[var_name]] <- var_chr
        }
      }
    }
  }

  survey_args
}
