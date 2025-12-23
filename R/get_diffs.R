#' Calculate difference in means
#'
#' @description
#' This function calculates the difference in means using a
#' bivariate regression, as well the p-value indicating how
#' significant each difference is. The main function doing the
#' calculations `lm()`.
#'
#' NOTE: This function does not perform an actual Dunnet Test as it
#' does not calculate the quantile of the multivariate t-distribution
#' when determining the confidence intervals and p-values. If you need
#' to perform an actual Dunnett Test use the `dunnett()` function
#' instead. Please be aware that that function is far slower when
#' there are many comparison groups due to the nature of
#' `mvtnorm::qmvt()` and high dimensional data.
#'
#' @param data A data frame or tibble.
#' @param x A numeric vector that will be used to calculate the means.
#'   This can be a string or symbol.
#' @param treats A variable whose values are used to determine if the means
#'   are statistically significantly different from each other. Should be
#'   a factor or character vector. This can be a string or symbol.
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]).
#'   See examples to see how it operates.
#' @param wt Weights. Add if you have a weighting variable and want to perform
#'   Dunnett's test with weighted means.
#' @param ref_level A string that specifies the level of the reference group
#'   through which the others will be tested.
#' @param pval_adj Method for adjusting p-values for multiple comparisons.
#'   Passed directly to `stats::p.adjust`. Options include "holm", "hochberg",
#'   "hommel", "bonferroni", "BH", "BY", "fdr", "none". Default is `NULL` (no adjustment).
#' @param conf_level A number between 0 and 1 that signifies the width of the
#'   desired confidence interval. Default is 0.95, which corresponds to a 95%
#'   confidence interval.
#' @param conf_method Determines whether the confidence intervals are calculated
#'   using the profile likelihood or the Wald method. Obviously has two options,
#'   "profile" and "wald". Wald is between 3 to 25 times as fast but not as
#'   reliable for small sample sizes (n < 50). For larger sample sizes,
#'   (n > 100), they will be very similar. **The default is Wald.**
#' @param show_means Logical. Default is `FALSE` which does not show the mean
#'   values for the levels. If `TRUE`, will add a column called `mean` that
#'   contains the means.
#' @param show_pct_change Logical. Default is `FALSE` which does not show the
#'   percent change from the reference category to the other categories. If
#'   `TRUE`, will show the percent change.
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 3.
#' @param na.rm Logical. Default is `TRUE` which removes NAs prior to
#'   calculation.
#'
#' @returns A tibble with one row if no `group` is provided and `data`
#'   is not of class `"grouped_df"`. If data is of class `"grouped_df"` or
#'   `group` is provided, it will return one row for each unique observation
#'   if one group is provides and one row per unique combination of observations
#'   if multiple groups are used.
#'
#' @export
get_diffs <- function(
  data,
  x,
  treats,
  group,
  wt,
  ref_level = NULL,
  pval_adj = NULL, # NEW ARGUMENT
  conf_level = 0.95,
  conf_method = c("wald", "profile"),
  show_means = TRUE,
  show_pct_change = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  UseMethod("get_diffs")
}


#' @export
get_diffs.data.frame <- function(
  data,
  x,
  treats,
  group,
  wt,
  ref_level = NULL,
  pval_adj = NULL, # NEW ARGUMENT
  conf_level = 0.95,
  conf_method = c("wald", "profile"),
  show_means = TRUE,
  show_pct_change = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  # Prep the data and get all necessary components
  prep <- prep_diffs_data(
    data = data,
    x = {{ x }},
    treats = {{ treats }},
    group = {{ group }},
    wt = {{ wt }},
    ref_level = ref_level,
    na.rm = na.rm,
    is_survey = FALSE
  )

  # Extract components from prep
  data <- prep$data
  x <- prep$x
  x_expr <- prep$x_expr
  treats <- prep$treats
  group_names <- prep$group_names
  wt_name <- prep$wt_name
  ref_level <- prep$ref_level
  cached_x_label <- prep$cached_x_label
  cached_treats_label <- prep$cached_treats_label
  cached_group_labels <- prep$cached_group_labels

  if (length(group_names) > 0) {
    nest_data <- make_nested(data, {{ group_names }})
    # iterate over each nest tibble from
    out <- purrr::map(
      nest_data$data,
      \(var) {
        bivariate_reg(
          var,
          x = {{ x }},
          treats = {{ treats }},
          wt = {{ wt_name }},
          show_means = show_means,
          show_pct_change = show_pct_change,
          ref_level = ref_level,
          pval_adj = pval_adj, # Pass to helper
          conf_level = conf_level
        )
      }
    ) %>%
      stats::setNames(nest_data$name)

    # combine the lists
    out <- vctrs::vec_rbind(!!!out, .names_to = "groups") %>%
      tidyr::separate_wider_delim(
        cols = "groups",
        delim = "_",
        names = group_names
      )
  } else {
    out <- bivariate_reg(
      data = data,
      x = {{ x }},
      treats = {{ treats }},
      wt = {{ wt_name }},
      show_means = show_means,
      show_pct_change = show_pct_change,
      ref_level = ref_level,
      pval_adj = pval_adj, # Pass to helper
      conf_level = conf_level,
      conf_method = conf_method
    )
  }

  # clean up the treats variable by removing it from the string in the term col
  out[[treats]] <- gsub(
    pattern = treats,
    replacement = "",
    x = out$term,
    fixed = TRUE
  )

  # Reorder and rename columns
  col_order <- c(
    group_names,
    treats,
    "estimate",
    "pct_change",
    "mean",
    "n",
    "conf_low",
    "conf_high",
    "p_value",
    "stars"
  )
  col_order <- intersect(col_order, names(out))
  out <- out[col_order]

  names(out)[names(out) == "estimate"] <- "diffs"

  out <- round_diffs(out, decimals)

  out <- add_diff_attributes(
    out,
    x_expr = x_expr,
    cached_x_label = cached_x_label,
    treats = treats,
    cached_treats_label = cached_treats_label,
    group_names = group_names,
    cached_group_labels = cached_group_labels,
    ref_level = ref_level
  )

  out <- structure(
    out,
    class = c("tidysurvey_mean_diffs", "tbl_df", "tbl", "data.frame")
  )

  out
}


#' @export
get_diffs.survey.design <- function(
  data,
  x,
  treats,
  group,
  wt,
  ref_level = NULL,
  pval_adj = NULL, # NEW ARGUMENT
  conf_level = 0.95,
  conf_method = c("wald", "profile"),
  show_means = TRUE,
  show_pct_change = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  survey_data <- data$variables

  # Prep the data and get all necessary components
  prep <- prep_diffs_data(
    data = survey_data,
    x = {{ x }},
    treats = {{ treats }},
    group = {{ group }},
    wt = NULL,
    ref_level = ref_level,
    na.rm = na.rm,
    is_survey = TRUE
  )

  # Extract components from prep
  data$variables <- prep$data
  x <- prep$x
  x_expr <- prep$x_expr
  treats <- prep$treats
  group_names <- prep$group_names
  ref_level <- prep$ref_level
  cached_x_label <- prep$cached_x_label
  cached_treats_label <- prep$cached_treats_label
  cached_group_labels <- prep$cached_group_labels

  if (length(group_names) > 0) {
    nest_data <- make_nested(data, {{ group_names }})
    # iterate over each nest tibble from
    out <- purrr::map(
      nest_data$data,
      \(var) {
        bivariate_reg(
          var,
          x = {{ x }},
          treats = {{ treats }},
          show_means = show_means,
          show_pct_change = show_pct_change,
          ref_level = ref_level,
          pval_adj = pval_adj, # Pass to helper
          conf_level = conf_level,
          conf_method = conf_method
        )
      }
    ) %>%
      stats::setNames(nest_data$name)

    # combine the lists
    out <- vctrs::vec_rbind(!!!out, .names_to = "groups") %>%
      tidyr::separate_wider_delim(
        cols = "groups",
        delim = "_",
        names = group_names
      )
  } else {
    out <- bivariate_reg(
      data = data,
      x = {{ x }},
      treats = {{ treats }},
      show_means = show_means,
      show_pct_change = show_pct_change,
      ref_level = ref_level,
      pval_adj = pval_adj, # Pass to helper
      conf_level = conf_level,
      conf_method = conf_method
    )
  }

  # clean up the treats variable by removing it from the string in the term col
  out[[treats]] <- gsub(
    pattern = treats,
    replacement = "",
    x = out$term,
    fixed = TRUE
  )

  # Reorder and rename columns
  col_order <- c(
    group_names,
    treats,
    "estimate",
    "pct_change",
    "mean",
    "n",
    "conf_low",
    "conf_high",
    "p_value",
    "stars"
  )

  col_order <- intersect(col_order, names(out))
  out <- out[col_order]

  names(out)[names(out) == "estimate"] <- "diffs"

  out <- round_diffs(out, decimals)

  out <- add_diff_attributes(
    out,
    x_expr = x_expr,
    cached_x_label = cached_x_label,
    treats = treats,
    cached_treats_label = cached_treats_label,
    group_names = group_names,
    cached_group_labels = cached_group_labels,
    ref_level = ref_level
  )

  out <- structure(
    out,
    class = c("tidysurvey_mean_diffs", "tbl_df", "tbl", "data.frame")
  )

  out
}


#' @export
get_diffs.svyrep.design <- function(
  data,
  x,
  treats,
  group,
  wt,
  ref_level = NULL,
  pval_adj = NULL, # NEW ARGUMENT
  conf_level = 0.95,
  conf_method = c("wald", "profile"),
  show_means = TRUE,
  show_pct_change = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  survey_data <- data$variables

  # Prep the data and get all necessary components
  prep <- prep_diffs_data(
    data = survey_data,
    x = {{ x }},
    treats = {{ treats }},
    group = {{ group }},
    wt = NULL,
    ref_level = ref_level,
    na.rm = na.rm,
    is_survey = TRUE
  )

  # Extract components from prep
  data$variables <- prep$data
  x <- prep$x
  x_expr <- prep$x_expr
  treats <- prep$treats
  group_names <- prep$group_names
  ref_level <- prep$ref_level
  cached_x_label <- prep$cached_x_label
  cached_treats_label <- prep$cached_treats_label
  cached_group_labels <- prep$cached_group_labels

  if (length(group_names) > 0) {
    nest_data <- make_nested(data, {{ group_names }})
    # iterate over each nest tibble from
    out <- purrr::map(
      nest_data$data,
      \(var) {
        bivariate_reg(
          var,
          x = {{ x }},
          treats = {{ treats }},
          show_means = show_means,
          show_pct_change = show_pct_change,
          ref_level = ref_level,
          pval_adj = pval_adj, # Pass to helper
          conf_level = conf_level,
          conf_method = conf_method
        )
      }
    ) %>%
      stats::setNames(nest_data$name)

    # combine the lists
    out <- vctrs::vec_rbind(!!!out, .names_to = "groups") %>%
      tidyr::separate_wider_delim(
        cols = "groups",
        delim = "_",
        names = group_names
      )
  } else {
    out <- bivariate_reg(
      data = data,
      x = {{ x }},
      treats = {{ treats }},
      show_means = show_means,
      show_pct_change = show_pct_change,
      ref_level = ref_level,
      pval_adj = pval_adj, # Pass to helper
      conf_level = conf_level,
      conf_method = conf_method
    )
  }

  # clean up the treats variable by removing it from the string in the term col
  out[[treats]] <- gsub(
    pattern = treats,
    replacement = "",
    x = out$term,
    fixed = TRUE
  )

  # Reorder and rename columns
  col_order <- c(
    group_names,
    treats,
    "estimate",
    "pct_change",
    "mean",
    "n",
    "conf_low",
    "conf_high",
    "p_value",
    "stars"
  )

  col_order <- intersect(col_order, names(out))
  out <- out[col_order]

  names(out)[names(out) == "estimate"] <- "diffs"

  out <- round_diffs(out, decimals)

  out <- add_diff_attributes(
    out,
    x_expr = x_expr,
    cached_x_label = cached_x_label,
    treats = treats,
    cached_treats_label = cached_treats_label,
    group_names = group_names,
    cached_group_labels = cached_group_labels,
    ref_level = ref_level
  )

  out <- structure(
    out,
    class = c("tidysurvey_mean_diffs", "tbl_df", "tbl", "data.frame")
  )

  out
}


### main helper --------------------------------------------------------
### main helper --------------------------------------------------------
bivariate_reg <- function(
  data,
  x,
  treats,
  wt,
  show_means = FALSE,
  show_pct_change = FALSE,
  ref_level,
  pval_adj = NULL,
  conf_level = 0.95,
  conf_method = c("wald", "profile"),
  decimals = 3
) {
  # --- 1. Data Preparation (Survey vs Data Frame) ---
  if (inherits(data, "survey.design") || inherits(data, "svyrep.design")) {
    survey_data <- data$variables

    survey_data[[treats]] <- make_factor(
      survey_data[[treats]],
      drop_levels = TRUE,
      force = TRUE
    )

    if (!missing(ref_level) && ref_level != levels(survey_data[[treats]])[1]) {
      survey_data[[treats]] <- stats::relevel(survey_data[[treats]], ref_level)
    }

    data$variables <- survey_data

    # create the model
    model <- survey::svyglm(
      stats::reformulate(treats, x),
      design = data
    )
  } else {
    data[[treats]] <- make_factor(
      data[[treats]],
      drop_levels = TRUE,
      force = TRUE
    )

    if (!missing(ref_level) && ref_level != levels(data[[treats]])[1]) {
      data[[treats]] <- stats::relevel(data[[treats]], ref_level)
    }

    # create the model
    model <- stats::lm(
      stats::reformulate(treats, x),
      data = data,
      weights = data[[wt]]
    )
  }

  # --- 2. Extract Coefficients ---
  coefs <- clean(
    model,
    conf_level = conf_level,
    include_reference = TRUE,
    conf_method = conf_method
  )

  # --- 3. P-Value Adjustment (Placed Early) ---
  # We adjust here before splitting into Ref/Non-Ref.
  # Crucially, we exclude the "(Intercept)" from adjustment because we
  # don't want to adjust for the test of "Mean != 0", only "Treat != Control".
  if (!is.null(pval_adj)) {
    # Identify treatment comparisons (exclude Intercept)
    is_comparison <- !grepl("(Intercept)", coefs$term, fixed = TRUE)

    # We also ensure we only adjust non-NA p-values (to be safe against singular models)
    to_adjust <- is_comparison & !is.na(coefs$p_value)

    if (any(to_adjust)) {
      coefs$p_value[to_adjust] <- stats::p.adjust(
        coefs$p_value[to_adjust],
        method = pval_adj
      )
    }
  }

  # --- 4. Formatting Output (Means / Pct Change) ---
  if (missing(ref_level)) {
    ref_level <- coefs[2, ]$term
  }

  if (show_pct_change || show_means) {
    # get the reference stats
    ref <- coefs[grepl("(Intercept)", coefs$term, fixed = TRUE), ]
    # Get non-reference stats
    non_ref <- coefs[!grepl("(Intercept)", coefs$term, fixed = TRUE), ]

    ref$n <- non_ref[1, ]$n
    ref$term <- ref_level

    # remove the first row from non_ref col
    non_ref <- non_ref[-1, ]

    if (show_pct_change) {
      ref$pct_change <- NA
      # add the percent change from the ref group to the non-ref groups
      non_ref$pct_change <- non_ref$estimate / ref$estimate
    }

    if (show_means) {
      # if show_means = TRUE, calculate the means

      #### clean up the ref stats row
      ref$mean <- ref$estimate
      # set the various cols to NA (This creates the NAs you wanted to avoid handling later)
      ref[c(
        "p_value",
        "moe",
        "conf_high",
        "conf_low"
      )] <- NA
      ref$estimate <- 0

      #### clean up the non-reference stats
      non_ref$mean <- ref$mean + non_ref$estimate

      ### combine reference and non-reference stats
      list_out <- list(ref, non_ref)
      out <- vctrs::vec_rbind(!!!list_out)
    } else {
      # if show_pct_change is true but show_means is false return just non_ref
      out <- non_ref
    }
  } else {
    out <- coefs[-c(1:2), ]
  }

  # --- 5. Generate Stars (Uses the potentially adjusted p-values) ---
  out$stars <- stars_pval(out$p_value)
  out
}


# other helpers ----------------------------------------------------------

prep_diffs_data <- function(
  data,
  x,
  treats,
  group = NULL,
  wt = NULL,
  ref_level = NULL,
  na.rm = TRUE,
  is_survey = FALSE
) {
  # Ensure inputs are symbols or strings
  x <- rlang::as_name(rlang::ensym(x))
  treats <- rlang::as_name(rlang::ensym(treats))

  cached_treats_label <- attr_var_label(data[[treats]])

  if (is.null(cached_treats_label)) {
    cached_treats_label <- treats
  }

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

  # Force the treats variable to a factor
  data[[treats]] <- make_factor(
    data[[treats]],
    drop_levels = TRUE,
    force = TRUE
  )

  # If ref_level is missing, set it to the first level in the treats variable
  if (is.null(ref_level)) {
    ref_level <- levels(data[[treats]])[1]
  }

  # Drop NAs if requested
  if (na.rm) {
    chk <- unique(c(x, group_names, treats))
    assert_nonempty_after_filter(data, chk, context = "diffs default")
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
    treats = treats,
    group_names = group_names,
    wt_name = wt_name,
    ref_level = ref_level,
    cached_x_label = cached_x_label,
    cached_treats_label = cached_treats_label,
    cached_group_labels = cached_group_labels
  )
}

round_diffs <- function(data, decimals) {
  # Round numeric columns
  data$diffs <- round(data$diffs, decimals)
  data$conf_low <- round(data$conf_low, decimals)
  data$conf_high <- round(data$conf_high, decimals)
  data$p_value <- round(data$p_value, decimals)

  if (any("mean" == names(data))) {
    data$mean <- round(data$mean, decimals)
  }

  if (any("pct_change" == names(data))) {
    data$pct_change <- round(data$pct_change, decimals + 2)
  }
  data
}

add_diff_attributes <- function(
  data,
  x_expr,
  cached_x_label,
  treats,
  cached_treats_label,
  group_names,
  cached_group_labels,
  ref_level
) {
  # Add attributes
  attr(data[[treats]], "label") <- cached_treats_label
  attr(data$diffs, "label") <- paste(
    "Difference in means relative to",
    ref_level
  )
  attr(data$n, "label") <- "N"
  attr(data$conf_low, "label") <- "Low CI"
  attr(data$conf_high, "label") <- "High CI"
  attr(data$p_value, "label") <- "P-Value"
  attr(data$stars, "label") <- ""

  if (length(group_names) > 0) {
    # if there are groups add the value labels

    # for each value in names(group_labels) add the variable label from group_labels
    for (y in names(cached_group_labels)) {
      attr(data[[y]], "label") <- cached_group_labels[[y]]
    }

    attr(data, "group_names") <- group_names
    attr(data, "group_labels") <- cached_group_labels
  }

  if (any("mean" == names(data))) {
    attr(data$mean, "label") <- "Mean"
  }

  if (any("pct_change" == names(data))) {
    attr(data$pct_change, "label") <- "% Change"
  }

  if (!is.null(cached_x_label)) {
    attr(data, "variable_label") <- cached_x_label
    attr(data, "variable_name") <- x_expr
  } else {
    attr(data, "variable_label") <- x_expr
    attr(data, "variable_name") <- x_expr
  }

  attr(data, "ref_level") <- paste("Reference level is", ref_level)

  data
}
