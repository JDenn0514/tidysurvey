#' Compute survey means with CIs, SD, and N for grouped complex designs
#'
#' `get_means()` computes design-based means and confidence intervals for a single
#' numeric variable from complex survey objects, optionally grouped by one or more
#' variables. It supports `survey.design`, `svyrep.design`, `srvyr::tbl_svy`, and
#' plain `data.frame` inputs (with a simpler weighted/unweighted implementation).
#'
#' @param data A survey object (survey.design, svyrep.design, or srvyr::tbl_svy),
#'   or a plain data.frame.
#' @param x The variable with which you wany to get the mean. Variable must be
#'   numeric but can be input as a string or symbol (e.g., "var" or var).
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns
#'   to group the data. This operates very similarly to `.by` from dplyr
#'   (for more info on that see [?dplyr_by][dplyr_by]). It can also be a
#'   character vector. If using an external character vector it must be
#'   wrapped in curly brackets (`{{}}`).
#'
#'   In addition, grouped data can be piped in via `dplyr::group_by()` or
#'   `srvyr::group_by()`. If data is a `grouped_df` and `group` is provided,
#'   `get_means()` will combine the variable(s) used in either `group_by`
#'   function and the variable(s) supplied in `group` to calculate means.
#' @param wt Optional weights column name (for data.frame method only). Ignored for
#'   survey objects, since weights/replicates are stored in the design.
#' @param decimals Number of decimals to round the results to. Default is 3.
#' @param na.rm Logical; whether to remove rows with missing values in `x` and `group`
#'   before computing frequencies. Default is `TRUE`.
#' @param conf_level Determine the confidence level for Wald CIs. Default is 0.95.
#' @param df Degrees of freedom for t critical values. Defaults to `Inf` (z-based).
#'   For  designs, you may prefer survey::degf(design); for replicate designs,
#'   Inf is conventional.
#'
#' @return A tibble with one row per group (or one row if ungrouped). With the
#'   following columns:
#'
#'   - grouping columns (preserving factor/label semantics for display)
#'
#'   - `mean`: design-based mean of x
#'
#'   - `sd`: unweighted sample standard deviation within each group
#'
#'   - `n`: unweighted row count per group
#'
#'   - `conf_low`, `conf_high`: Wald confidence interval bounds
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
#' @details
#' - Survey objects are subset to remove NAs in x and grouping variables before grouping,
#'   preserving design alignment (ids/strata/weights/replicates).
#'
#' - Means and standard errors are computed via survey::svymean(). CIs are Wald intervals
#'   using either z (df = Inf) or t (finite df) critical values.
#'
#' - sd and n are descriptive (unweighted) and are not used in the CI calculations.
#'
#' - Group output order follows factor levels for factors, alphabetical for characters,
#'   and ascending for numerics.
#'
#' - Value labels for grouping variables are preserved in the output by factorizing keys
#'   against original labels; variable-level labels are copied to output columns.
#'
#' @section Methods:
#' - `get_means.survey.design()`: Complex survey designs. For more information visit
#'   `survey::svydesign()` and `srvyr::as_survey_design()`.
#'
#' - `get_means.svyrep.design()`: Replicate-weight survey designs. For more
#'   information visit `survey::as.svrepdesign()` and `srvyr::as_survey_rep()`
#'
#' - `get_means.tbl_svy()``: Unwraps srvyr::tbl_svy and dispatches to the appropriate
#'   survey method. For more info, visit `srvyr::as_survey_design()` and
#'   `srvyr::as_survey_rep()`.
#'
#' - `get_means.data.frame()`: Non-design summary with optional weights.
#'
#' @examples
#' # Setup example data
#' df <- make_expanded_df()
#' df <- label_vars(df)
#'
#' # Build a Taylor survey design (srvyr wrapper for convenience)
#' library(srvyr)
#' df_svy <- df |>
#'   srvyr::as_survey_design(
#'     ids = psu,
#'     strata = strata,
#'     weights = wts
#'   )
#'
#' # Ungrouped mean (design-based)
#' get_means(df_svy, x = satisfaction_price)
#'
#' # Grouped by a labelled factor and a labelled character
#' get_means(df_svy, x = satisfaction_price, group = c(satisfaction_service, x1))
#'
#' # Grouped by a simple factor
#' get_means(df_svy, x = satisfaction_price, group = grp)
#'
#' # Plain data.frame (non-design) example
#' # Weighted mean and simple t-based CI
#' get_means(df, x = satisfaction_price, group = grp, wt = wts)
#'
#' @seealso
#' - `survey::svymean()` for design-based means and SEs
#'
#' - `srvyr::as_survey_design()`, `srvyr::as_survey_rep()` for building
#'   survey designs
#'
#' @export
get_means <- function(
  data,
  x,
  group = NULL,
  wt = NULL,
  decimals = 3,
  na.rm = TRUE,
  conf_level = 0.95,
  df = Inf
) {
  UseMethod("get_means")
}


#' @export
get_means.data.frame <- function(
  data,
  x,
  group = NULL,
  wt = NULL,
  decimals = 3,
  na.rm = TRUE,
  conf_level = 0.95,
  df = Inf # ignored here
) {
  # Prep the data and get all necessary components
  prep <- prep_means_data(
    data = data,
    x = {{ x }},
    group = {{ group }},
    wt = {{ wt }},
    na.rm = na.rm,
    is_survey = FALSE
  )

  # Extract components from prep
  data <- prep$data
  x <- prep$x
  x_expr <- prep$x_expr
  group_names <- prep$group_names
  wt_name <- prep$wt_name
  cached_x_label <- prep$cached_x_label
  cached_group_labels <- prep$cached_group_labels

  if (!is.null(group_names)) {
    # if the group arg is not missing, apply grouping based on group_names
    data <- data %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_names)))
  }

  # Summarize data
  out <- data %>%
    dplyr::summarise(
      # calculate the weighted n
      n = dplyr::n(),
      # calculate the mean (weighted sum / n)
      mean = stats::sum(.data[[x]] * .data[[wt_name]], na.rm = TRUE) / n,
      # calculate the weighted sd
      sd = stats::sd(.data[[x]]),
      # remove the groups
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # calculate std.error
      se = sd / sqrt(n),
      # calculate the confidence invtervals
      conf_low = mean -
        stats::qt(1 - ((1 - conf_level) / 2), n - 1) * se,
      conf_high = mean +
        stats::qt(1 - ((1 - conf_level) / 2), n - 1) * se,
      # convert all group variables to a factor
      dplyr::across(
        # run the function over the variables in group_names
        tidyselect::all_of(group_names),
        # convert to a factor, removing levels, forcing to factor, and keeping NA as NA
        \(x) make_factor(x, drop_levels = TRUE, force = TRUE, na.rm = TRUE)
      ),
      # round all numeric columns
      dplyr::across(
        tidyselect::where(is.numeric),
        \(var) round(var, decimals)
      )
    )

  out <- out[c(group_names, "mean", "sd", "n", "conf_low", "conf_high")]

  out <- add_mean_attributes(
    out,
    x_expr = x_expr,
    cached_x_label = cached_x_label,
    group_names = group_names,
    cached_group_labels = cached_group_labels
  )

  out <- structure(
    out,
    class = c("tidysurvey_means", "tbl_df", "tbl", "data.frame")
  )

  out
}

#' @export
get_means.tbl_svy <- function(data, ...) {
  NextMethod("get_means")
}

#' @export
get_means.survey.design <- function(
  data,
  x,
  group = NULL,
  wt = NULL, # ignored for survey data
  decimals = 3,
  na.rm = TRUE,
  conf_level = 0.95,
  df = Inf
) {
  # 1) Start from the design’s current variables
  original_data <- data$variables

  # prep on the current variables to resolve x and group names
  prep <- prep_means_data(
    data = original_data,
    x = {{ x }},
    group = {{ group }},
    wt = NULL,
    na.rm = FALSE, # do NA handling via design subset below
    is_survey = TRUE
  )

  # Extract components
  x_name <- prep$x
  x_expr <- prep$x_expr
  group_names <- prep$group_names
  cached_x_label <- prep$cached_x_label
  cached_group_labels <- prep$cached_group_labels

  # 3) If na.rm, subset the design using a logical mask built on the design’s variables
  if (na.rm) {
    keep <- !is.na(data$variables[[x_name]])
    keep[is.na(keep)] <- FALSE
    # IMPORTANT: subset via the survey method, with a logical mask
    data <- subset(data, keep)

    # 3b) Drop NAs in grouping variables (if any groups specified)
    if (!is.null(group_names) && length(group_names) > 0L) {
      # Build a logical mask that keeps rows where all group vars are non-NA
      grp_df <- data$variables[group_names]
      keep_grp <- stats::complete.cases(grp_df)
      data <- subset(data, keep_grp)
    }
  }

  # extract cleaned survey data (after NA removal)
  survey_data <- data$variables

  # build grouping index from the aligned survey_data
  gi <- .grouping_index(survey_data, group_names)

  # compute grouped means. Inside, we’ll subset by logical masks.
  res <- .survey_grouped_means(
    design = data,
    x = x_name,
    rows_by_group = gi$rows_by_group,
    conf_level = conf_level,
    df = df,
    na.rm = na.rm
  )

  # ensure attributes from the original data set are in new data
  for (group in group_names) {
    attributes(gi$keys[[group]]) <- attributes(original_data[[group]])
  }
  # convert gi$keys to factors
  display_keys <- factorize_multi_groups_only(
    gi$keys,
    group_names,
    drop_zero = TRUE,
    na.rm = na.rm
  )

  # Bind keys to results
  out <- dplyr::bind_cols(display_keys, res) |>
    # round numeric columns
    dplyr::mutate(
      dplyr::across(
        c(mean, sd, conf_low, conf_high),
        \(x) round(x, decimals)
      )
    )

  # set the column order
  cols_order <- c(
    group_names,
    "mean",
    "sd",
    "n",
    "conf_low",
    "conf_high"
  )
  # identify which columsn from cols_order are in out
  cols_order <- intersect(cols_order, names(out))
  # reorder out based on the order of cols_order
  out <- out[, cols_order, drop = FALSE]

  # 11) Attributes and class
  out <- add_mean_attributes(
    out,
    x_expr = x_expr,
    cached_x_label = cached_x_label,
    group_names = group_names,
    cached_group_labels = cached_group_labels
  )

  # ensure output is a tibble
  out <- tibble::as_tibble(out)
  # add custom class
  class(out) <- c("tidysurvey_means", class(out))
  out
}

#' @export
get_means.svyrep.design <- function(
  data,
  x,
  group = NULL,
  wt = NULL, # ignored for survey data
  decimals = 3,
  na.rm = TRUE,
  conf_level = 0.95,
  df = Inf
) {
  # get the data from the survey design
  original_data <- data$variables

  # 2) Prep on the current variables to resolve x and group names
  prep <- prep_means_data(
    data = original_data,
    x = {{ x }},
    group = {{ group }},
    wt = NULL,
    na.rm = FALSE, # do NA handling via design subset below
    is_survey = TRUE
  )

  # Extract components
  x_name <- prep$x
  x_expr <- prep$x_expr
  group_names <- prep$group_names
  cached_x_label <- prep$cached_x_label
  cached_group_labels <- prep$cached_group_labels

  # 3) If na.rm, subset the design using a logical mask built on the design’s variables
  if (na.rm) {
    keep <- !is.na(data$variables[[x_name]])
    keep[is.na(keep)] <- FALSE
    # IMPORTANT: subset via the survey method, with a logical mask
    data <- subset(data, keep)

    # 3b) Drop NAs in grouping variables (if any groups specified)
    if (!is.null(group_names) && length(group_names) > 0L) {
      # Build a logical mask that keeps rows where all group vars are non-NA
      grp_df <- data$variables[group_names]
      keep_grp <- stats::complete.cases(grp_df)
      data <- subset(data, keep_grp)
    }
  }

  # extract cleaned survey data (after NA removal)
  survey_data <- data$variables

  # build grouping index from the aligned survey_data
  gi <- .grouping_index(survey_data, group_names)

  # compute grouped means. Inside, we’ll subset by logical masks.
  res <- .survey_grouped_means(
    design = data,
    x = x_name,
    rows_by_group = gi$rows_by_group,
    conf_level = conf_level,
    df = df,
    na.rm = na.rm
  )

  # add attributes from the original data set
  for (x in group_names) {
    attributes(gi$keys[[x]]) <- attributes(original_data[[x]])
  }
  # convert gi$keys to factors
  display_keys <- factorize_multi_groups_only(
    gi$keys,
    group_names,
    drop_zero = TRUE,
    na.rm = na.rm
  )

  # Bind keys to results
  out <- dplyr::bind_cols(display_keys, res) |>
    # round numeric columns
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        \(x) round(x, decimals)
      )
    )

  # 10) Column order
  cols_order <- c(
    group_names,
    "mean",
    "sd",
    "n",
    "conf_low",
    "conf_high"
  )
  cols_order <- intersect(cols_order, names(out))
  out <- out[, cols_order, drop = FALSE]

  # 11) Attributes and class
  out <- add_mean_attributes(
    out,
    x_expr = x_expr,
    cached_x_label = cached_x_label,
    group_names = group_names,
    cached_group_labels = cached_group_labels
  )

  out <- tibble::as_tibble(out)
  class(out) <- c("tidysurvey_means", class(out))
  out
}

# calculation helpers ------------------------------------------------------------

svy_means <- function(
  data,
  x,
  na.rm = TRUE,
  conf_level = 0.95,
  df = Inf
) {
  form_x <- reformulate(termlabels = x)
  mean <- survey::svymean(form_x, data, na.rm = na.rm)

  # Calculate CIs
  out <- calc_ci(mean, conf_level = conf_level, df = df)
  out
}

.grouping_index <- function(data, group_names) {
  if (is.null(group_names) || length(group_names) == 0) {
    # if no group_names were provided, treat data as one group
    return(list(
      # all rows belong to one group; store a single vector
      rows_by_group = list(seq_len(nrow(data))),
      # a 1-row tibble to represent key
      # since there is no group, set .group_id to 1
      keys = tibble::tibble(.group_id = 1)
    ))
  }
  # Extract the grouping columns from the data in the provided order.
  # gkeys is a data.frame/tibble containing only the grouping columns.
  gkeys <- data[group_names]
  # create group identifier for each row based on combos of gkeys
  # constructed in the order in which they appar
  gid <- vctrs::vec_group_id(gkeys)
  # split row indices into a list by group id
  split_obj <- vctrs::vec_split(seq_len(nrow(data)), gid)
  # get the list of indices by group id
  rows_list <- split_obj$val
  # make a data.frame with unique groups and their locations
  # unique groups are in same order as gkeys
  loc <- vctrs::vec_group_loc(gkeys)
  # extract the keys
  keys <- loc$key
  # set the name of the keys with group_names
  names(keys) <- group_names

  # Build a multi-column order using base order on each column
  ord <- do.call(order, c(as.list(keys), list(method = "radix")))

  list(rows_by_group = rows_list[ord], keys = keys[ord, , drop = FALSE])
}

.survey_grouped_means <- function(
  design,
  x,
  rows_by_group,
  conf_level = 0.95,
  df = Inf,
  na.rm = TRUE
) {
  stopifnot(length(x) >= 1L)

  n_tot <- nrow(design$variables)

  est_one_group <- function(rows) {
    # Convert integer indices to a logical mask of length n
    if (is.numeric(rows)) {
      mask <- rep(FALSE, n_tot)
      idx <- rows[rows >= 1 & rows <= n_tot]
      if (length(idx) > 0L) mask[idx] <- TRUE
    } else {
      # In case rows_by_group was built differently
      stop("rows must be integer indices")
    }
    # subset the data
    d_sub <- subset(design, mask)
    # extract subset data
    d_sub_data <- d_sub$variables
    # print(d_sub_data)
    # calculate means
    out <- svy_means(
      data = d_sub,
      x = x,
      na.rm = na.rm,
      conf_level = conf_level,
      df = df
    )

    # get unweighted n
    n <- nrow(d_sub_data)
    # unweighted sd
    sd <- stats::sd(d_sub_data[[x]], na.rm = na.rm)
    out$n <- n
    out$sd <- sd
    out
  }

  out_list <- lapply(rows_by_group, est_one_group)
  dplyr::bind_rows(out_list)
}


# data cleaning and prep -------------------------------------------------

prep_means_data <- function(
  data,
  x,
  group = NULL,
  wt = NULL,
  na.rm = TRUE,
  is_survey = FALSE
) {
  # Ensure inputs are symbols or strings
  x <- rlang::as_name(rlang::ensym(x))

  # Capture the original expression passed to x (for attaching attributes later)
  x_expr <- rlang::enexpr(x)

  # Resolve group columns from tidyselect, in the order they should appear
  group_names <- compose_group_names(data, {{ group }})

  # Cache group labels (variable labels) from the pristine data
  if (length(group_names) > 0) {
    cached_group_labels <- attr_var_label(data[, group_names], if_null = "name")
  } else {
    cached_group_labels <- character(0)
  }

  # Prepare weights: returns possibly modified data plus the resolved weight column name
  # Only do this for non-survey data
  if (!is_survey) {
    wt_res <- ensure_weight(data, {{ wt }})
    data <- wt_res$data
    wt_name <- wt_res$wt_name
  } else {
    wt_name <- NULL
  }

  # Cache the variable label so we can reattach it later
  cached_x_label <- attr_var_label(data[[x_expr]])

  # Check for numeric x
  if (!is.numeric(data[[x]])) {
    cli::cli_abort(c(
      "{.arg x} must be of class `numeric`",
      "i" = "`{x_expr}` is of class {class(data[[x]])}"
    ))
  }

  # Drop NAs if requested
  if (na.rm) {
    chk <- unique(c(x, group_names))
    assert_nonempty_after_filter(data, chk, context = "means default")
    data <- dplyr::ungroup(data)
    data <- dplyr::filter(
      data,
      stats::complete.cases(dplyr::across(tidyselect::all_of(chk)))
    )
  }

  # Return a list with all the prepared components
  list(
    data = data,
    x = x,
    x_expr = x_expr,
    group_names = group_names,
    wt_name = wt_name,
    cached_x_label = cached_x_label,
    cached_group_labels = cached_group_labels
  )
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


add_mean_attributes <- function(
  data,
  x_expr,
  cached_x_label,
  group_names,
  cached_group_labels
) {
  # add a variable for the n variable
  attr(data$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(data$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(data$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(data$conf_low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(data$conf_high, "label") <- "High CI"

  if (length(group_names) > 0) {
    # if there are groups add the value labels

    # for each value in names(group_labels) add the variable label from group_labels
    for (y in names(cached_group_labels)) {
      attr(data[[y]], "label") <- cached_group_labels[[y]]
    }

    attr(data, "group_names") <- group_names
    attr(data, "group_labels") <- cached_group_labels
  }

  if (!is.null(cached_x_label)) {
    attr(data, "variable_label") <- cached_x_label
    attr(data, "variable_name") <- x_expr
  } else {
    attr(data, "variable_label") <- x_expr
    attr(data, "variable_name") <- x_expr
  }

  data
}
