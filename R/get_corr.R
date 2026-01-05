# #' Calculate (survey) weighted correlations
# #'
# #' This function calculates weighted Pearson correlations between two variables.
# #' It also allows you to group the data and calculate correlations along each
# #' level of the grouping variable. If data is not grouped and no group is
# #' specified, then it will return the same output as [wtd_corr()].
# #'
# #' @param data An object of type data.frame or tibble. If piping the data into the
# #'   function, this is not required.
# #' @param x,y Can be either character strings or symbols. Name of two variables
# #'   in the data you want to calculate the correlation between.
# #' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to
# #'   group the data by in addition to `treats`. This operates very similarly
# #'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]).
# #'   See examples to see how it operates.
# #' @param wt Can be either character strings or symbols. Weights. Add if
# #'   you have a weighting variable and want to get weighted correlations
# #' @param decimals Number of decimals each number should be rounded to. Default
# #'   is 3.
# #'
# #' @returns A tibble showing correlations (`correlation`), number of observations
# #'   (`n`), the p-value (`p_value`), and stars indicating it's statistical
# #'   significance. If data is of class `"grouped_df"` or the `group` argument is
# #'   specified, it will return one row for each unique observation if one group is
# #'   provided and one row per unique combination of observations if multiple groups
# #'   are used.
# #' @export
# #'
# #'
# tidysurvey_corr <- S7::new_class(
#   "tidysurvey_corr",
#   parent = S7::class_data.frame,
#   properties = list(
#     group_names = class_character,
#     group_labels = class_list
#   )
# )

# get_corr <- S7::new_generic(
#   "get_corr",
#   "data",
#   function(
#     data,
#     x,
#     y,
#     group = NULL,
#     wt = NULL,
#     decimals = 3,
#     na.rm = TRUE
#   ) {
#     # Default behavior or error message
#     stop(paste("No method defined for object of class", class(data)[1]))
#   }
# )

# S7::method(
#   get_corr,
#   "survey.design",
#   function(
#     data,
#     x,
#     y,
#     group = NULL,
#     wt = NULL,
#     decimals = 3,
#     na.rm = TRUE
#   ) {
#     # get the data from the survey design
#     original_data <- data$variables

#     # prepare the data, clean up variable, group names, etc.
#     prep <- prep_corr_data(
#       data = data,
#       x = {{ x }},
#       y = {{ y }},
#       group = {{ group }},
#       wt = {{ wt }},
#       na.rm = na.rm,
#       is_survey = FALSE
#     )

#     # Extract components
#     data <- prep$data
#     x_name <- prep$x
#     x_expr <- prep$x_expr
#     y_name <- prep$y
#     y_expr <- prep$y_expr
#     group_names <- prep$group_names
#     cached_x_label <- prep$cached_x_label
#     cached_y_label <- prep$cached_y_label
#     cached_group_labels <- prep$cached_group_labels

#     # extract cleaned survey data (after NA removal)
#     survey_data <- data$variables

#     # build grouping index from the aligned survey_data
#     gi <- .grouping_index(survey_data, group_names)

#     # compute grouped means. Inside, we’ll subset by logical masks.
#     res <- .survey_grouped_corr(
#       design = data,
#       x = x_name,
#       y = y_name,
#       rows_by_group = gi$rows_by_group
#     )

#     # add attributes from the original data set
#     for (x in group_names) {
#       attributes(gi$keys[[x]]) <- attributes(original_data[[x]])
#     }

#     # convert gi$keys to factors
#     display_keys <- factorize_multi_groups_only(
#       gi$keys,
#       group_names,
#       drop_zero = TRUE,
#       na.rm = na.rm
#     )

#     # Bind keys to results
#     out <- dplyr::bind_cols(display_keys, res) |>
#       # round numeric columns
#       dplyr::mutate(
#         dplyr::across(
#           tidyselect::where(is.numeric),
#           \(x) round(x, decimals)
#         )
#       )

#     # add an attribute containing the names of the grouping variables
#     attr(out, "group_names") <- group_names

#     attr(out$correlation, "label") <- "Correlation"
#     attr(out$n, "label") <- "N"
#     attr(out$p_value, "label") <- "P-Value"

#     # set the class
#     class(out) <- c("tidysurvey_corr", "tbl_df", "tbl", "data.frame")

#     out
#   }
# )

# # calculation helpers ----------------------------------------------------

# svy_corr <- function(
#   data,
#   x,
#   y
# ) {
#   form_x <- reformulate(y, x)

#   # get correlation from svyvar
#   v <- survey::svyvar(form_x, data)

#   cov_xy <- v[1, 2]
#   var_x <- v[1, 1]
#   var_y <- v[2, 2]

#   rho <- cov_xy / sqrt(var_x * var_y)

#   # get p-value via svyglm
#   fit <- survey::svyglm(form_x, data)
#   p <- summary(fit)$coefficients[2, 4] # p-value for slope

#   tibble::tibble(correlation = rho, p_value = p)
# }

# .grouping_index <- function(data, group_names) {
#   if (is.null(group_names) || length(group_names) == 0) {
#     # if no group_names were provided, treat data as one group
#     return(list(
#       # all rows belong to one group; store a single vector
#       rows_by_group = list(seq_len(nrow(data))),
#       # a 1-row tibble to represent key
#       # since there is no group, set .group_id to 1
#       keys = tibble::tibble(.group_id = 1)
#     ))
#   }
#   # Extract the grouping columns from the data in the provided order.
#   # gkeys is a data.frame/tibble containing only the grouping columns.
#   gkeys <- data[group_names]
#   # create group identifier for each row based on combos of gkeys
#   # constructed in the order in which they appar
#   gid <- vctrs::vec_group_id(gkeys)
#   # split row indices into a list by group id
#   split_obj <- vctrs::vec_split(seq_len(nrow(data)), gid)
#   # get the list of indices by group id
#   rows_list <- split_obj$val
#   # make a data.frame with unique groups and their locations
#   # unique groups are in same order as gkeys
#   loc <- vctrs::vec_group_loc(gkeys)
#   # extract the keys
#   keys <- loc$key
#   # set the name of the keys with group_names
#   names(keys) <- group_names

#   # Build a multi-column order using base order on each column
#   ord <- do.call(order, c(as.list(keys), list(method = "radix")))

#   list(rows_by_group = rows_list[ord], keys = keys[ord, , drop = FALSE])
# }

# .survey_grouped_corr <- function(design, x, y, rows_by_group) {
#   n_tot <- nrow(design$variables)

#   est_one_group <- function(rows) {
#     mask <- rep(FALSE, n_tot)
#     mask[rows] <- TRUE

#     d_sub <- subset(design, mask)
#     d_sub_data <- d_sub$variables

#     out <- svy_corr(d_sub, x, y)
#     out$n <- nrow(d_sub_data)
#     out
#   }

#   results <- lapply(rows_by_group, est_one_group)
#   dplyr::bind_rows(results)
# }

# # data prep functions -------------------------------------------------------

# prep_corr_data <- function(
#   data,
#   x,
#   y,
#   group = NULL,
#   wt = NULL,
#   na.rm = TRUE,
#   is_survey = FALSE
# ) {
#   # Ensure inputs are symbols or strings
#   x <- rlang::as_name(rlang::ensym(x))
#   y <- rlang::as_name(rlang::ensym(y))

#   # Capture the original expression passed to x and y (for attaching attributes later)
#   x_expr <- rlang::enexpr(x)
#   y_expr <- rlang::enexpr(y)

#   # Resolve group columns from tidyselect, in the order they should appear
#   group_names <- compose_group_names(data, {{ group }})

#   # Cache group labels (variable labels) from the pristine data
#   if (length(group_names) > 0) {
#     cached_group_labels <- attr_var_label(data[, group_names], if_null = "name")
#   } else {
#     cached_group_labels <- character(0)
#   }

#   # Prepare weights: returns possibly modified data plus the resolved weight column name
#   # Only do this for non-survey data
#   if (!is_survey) {
#     wt_res <- ensure_weight(data, {{ wt }})
#     data <- wt_res$data
#     wt_name <- wt_res$wt_name
#   } else {
#     wt_name <- NULL
#   }

#   # Cache the variable label so we can reattach it later
#   cached_x_label <- attr_var_label(data[[x_expr]])
#   cached_y_label <- attr_var_label(data[[y_expr]])

#   # Check for numeric x
#   if (!is.numeric(data[[x]])) {
#     cli::cli_abort(c(
#       "{.arg x} must be of class `numeric`",
#       "i" = "`{x_expr}` is of class {class(data[[x]])}"
#     ))
#   }

#   # Check for numeric x
#   if (!is.numeric(data[[y]])) {
#     cli::cli_abort(c(
#       "{.arg y} must be of class `numeric`",
#       "i" = "`{y_expr}` is of class {class(data[[y]])}"
#     ))
#   }

#   # Drop NAs if requested
#   if (na.rm) {
#     chk <- unique(c(x, y, group_names))
#     assert_nonempty_after_filter(data, chk, context = "corr default")
#     data <- dplyr::ungroup(data)
#     data <- dplyr::filter(
#       data,
#       stats::complete.cases(dplyr::across(tidyselect::all_of(chk)))
#     )
#   }

#   # Return a list with all the prepared components
#   list(
#     data = data,
#     x = x,
#     y = y,
#     x_expr = x_expr,
#     y_expr = y_expr,
#     group_names = group_names,
#     wt_name = wt_name,
#     cached_x_label = cached_x_label,
#     cached_y_label = cached_y_label,
#     cached_group_labels = cached_group_labels
#   )
# }
