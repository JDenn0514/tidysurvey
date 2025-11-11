# tidysurvey_means <- new_class(
#   "tidysurvey_means",
#   parent = tibble::tbl_df,
#   properties = list(
#     variable_label = class_character,
#     variable_name  = class_character,
#     group_names    = class_character,
#     group_labels   = class_list
#   )
# )

# get_means <- S7::new_generic("get_means","data")

# get_means <- S7::new_generic(
#   "get_means",
#   "data",
#   function(
#     data,
#     x,
#     group = NULL,
#     wt = NULL,
#     decimals = 3,
#     na.rm = TRUE,
#     conf_level = 0.95,
#     df = Inf
#   ) {
#     # Default behavior or error message
#     stop(paste("No method defined for object of class", class(data)[1]))
#   }
# )

# S7::method(get_means, "data.frame", function(
#   data,
#   x,
#   group = NULL,
#   wt = NULL,
#   decimals = 3,
#   na.rm = TRUE,
#   conf_level = 0.95,
#   df = Inf
# ) {

#   prep <- prep_means_data(
#     data = data,
#     x = {{ x }},
#     group = {{ group }},
#     wt = {{ wt }},
#     na.rm = na.rm,
#     is_survey = FALSE
#   )

#   # same summarization logic
#   data <- prep$data
#   x <- prep$x
#   group_names <- prep$group_names
#   wt_name <- prep$wt_name
#   cached_x_label <- prep$cached_x_label
#   cached_group_labels <- prep$cached_group_labels

#   if (!is.null(group_names)) {
#     data <- dplyr::group_by(data, dplyr::across(tidyselect::all_of(group_names)))
#   }

#   out <- data |>
#     dplyr::summarise(
#       n = dplyr::n(),
#       mean = sum(.data[[x]] * .data[[wt_name]], na.rm = TRUE) / n,
#       sd = stats::sd(.data[[x]]),
#       .groups = "drop"
#     ) |>
#     dplyr::mutate(
#       se = sd / sqrt(n),
#       conf_low = mean - stats::qt(1 - ((1 - conf_level) / 2), n - 1) * se,
#       conf_high = mean + stats::qt(1 - ((1 - conf_level) / 2), n - 1) * se,
#       dplyr::across(where(is.numeric), \(v) round(v, decimals))
#     )

#   out <- out[c(group_names, "mean", "sd", "n", "conf_low", "conf_high")]

#   S7::new_object(
#     tidysurvey_means,
#     out,
#     variable_label = cached_x_label,
#     variable_name = prep$x_expr,
#     group_names = group_names,
#     group_labels = cached_group_labels
#   )
# })

# S7::method(get_means, "survey.design", function(
#   data,
#   x,
#   group = NULL,
#   wt = NULL, # ignored for survey data
#   decimals = 3,
#   na.rm = TRUE,
#   conf_level = 0.95,
#   df = Inf
# ) {
#   # 1) Start from the design’s current variables
#   original_data <- data$variables

#   # prep on the current variables to resolve x and group names
#   prep <- prep_means_data(
#     data = original_data,
#     x = {{ x }},
#     group = {{ group }},
#     wt = NULL,
#     na.rm = FALSE, # do NA handling via design subset below
#     is_survey = TRUE
#   )

#   # Extract components
#   x_name <- prep$x
#   x_expr <- prep$x_expr
#   group_names <- prep$group_names
#   cached_x_label <- prep$cached_x_label
#   cached_group_labels <- prep$cached_group_labels

#   # 3) If na.rm, subset the design using a logical mask built on the design’s variables
#   if (na.rm) {
#     keep <- !is.na(data$variables[[x_name]])
#     keep[is.na(keep)] <- FALSE
#     # IMPORTANT: subset via the survey method, with a logical mask
#     data <- subset(data, keep)

#     # 3b) Drop NAs in grouping variables (if any groups specified)
#     if (!is.null(group_names) && length(group_names) > 0L) {
#       # Build a logical mask that keeps rows where all group vars are non-NA
#       grp_df <- data$variables[group_names]
#       keep_grp <- stats::complete.cases(grp_df)
#       data <- subset(data, keep_grp)
#     }
#   }

#   # extract cleaned survey data (after NA removal)
#   survey_data <- data$variables

#   # build grouping index from the aligned survey_data
#   gi <- .grouping_index(survey_data, group_names)

#   # compute grouped means. Inside, we’ll subset by logical masks.
#   res <- .survey_grouped_means(
#     design = data,
#     x = x_name,
#     rows_by_group = gi$rows_by_group,
#     conf_level = conf_level,
#     df = df,
#     na.rm = na.rm
#   )

#   # ensure attributes from the original data set are in new data
#   for (group in group_names) {
#     attributes(gi$keys[[group]]) <- attributes(original_data[[group]])
#   }
#   # convert gi$keys to factors
#   display_keys <- factorize_multi_groups_only(
#     gi$keys,
#     group_names,
#     drop_zero = TRUE,
#     na.rm = na.rm
#   )

#   # Bind keys to results
#   out <- dplyr::bind_cols(display_keys, res) |>
#     # round numeric columns
#     dplyr::mutate(
#       dplyr::across(
#         c(mean, sd, conf_low, conf_high),
#         \(x) round(x, decimals)
#       )
#     )

#   # set the column order
#   cols_order <- c(
#     group_names,
#     "mean",
#     "sd",
#     "n",
#     "conf_low",
#     "conf_high"
#   )
#   # identify which columsn from cols_order are in out
#   cols_order <- intersect(cols_order, names(out))
#   # reorder out based on the order of cols_order
#   out <- out[, cols_order, drop = FALSE]

#   out <- tibble::as_tibble(out)

#   S7::new_object(
#     tidysurvey_means,
#     out,
#     variable_label = cached_x_label,
#     variable_name = prep$x_expr,
#     group_names = group_names,
#     group_labels = cached_group_labels
#   )
# })

# S7::method(get_means, "svyrep.design", function(
#   data,
#   x,
#   group = NULL,
#   wt = NULL, # ignored for survey data
#   decimals = 3,
#   na.rm = TRUE,
#   conf_level = 0.95,
#   df = Inf
# ) {
#   # get the data from the survey design
#   original_data <- data$variables

#   # 2) Prep on the current variables to resolve x and group names
#   prep <- prep_means_data(
#     data = original_data,
#     x = {{ x }},
#     group = {{ group }},
#     wt = NULL,
#     na.rm = FALSE, # do NA handling via design subset below
#     is_survey = TRUE
#   )

#   # Extract components
#   x_name <- prep$x
#   x_expr <- prep$x_expr
#   group_names <- prep$group_names
#   cached_x_label <- prep$cached_x_label
#   cached_group_labels <- prep$cached_group_labels

#   # 3) If na.rm, subset the design using a logical mask built on the design’s variables
#   if (na.rm) {
#     keep <- !is.na(data$variables[[x_name]])
#     keep[is.na(keep)] <- FALSE
#     # IMPORTANT: subset via the survey method, with a logical mask
#     data <- subset(data, keep)

#     # 3b) Drop NAs in grouping variables (if any groups specified)
#     if (!is.null(group_names) && length(group_names) > 0L) {
#       # Build a logical mask that keeps rows where all group vars are non-NA
#       grp_df <- data$variables[group_names]
#       keep_grp <- stats::complete.cases(grp_df)
#       data <- subset(data, keep_grp)
#     }
#   }

#   # extract cleaned survey data (after NA removal)
#   survey_data <- data$variables

#   # build grouping index from the aligned survey_data
#   gi <- .grouping_index(survey_data, group_names)

#   # compute grouped means. Inside, we’ll subset by logical masks.
#   res <- .survey_grouped_means(
#     design = data,
#     x = x_name,
#     rows_by_group = gi$rows_by_group,
#     conf_level = conf_level,
#     df = df,
#     na.rm = na.rm
#   )

#   # add attributes from the original data set
#   for (x in group_names) {
#     attributes(gi$keys[[x]]) <- attributes(original_data[[x]])
#   }
#   # convert gi$keys to factors
#   display_keys <- factorize_multi_groups_only(
#     gi$keys,
#     group_names,
#     drop_zero = TRUE,
#     na.rm = na.rm
#   )

#   # Bind keys to results
#   out <- dplyr::bind_cols(display_keys, res) |>
#     # round numeric columns
#     dplyr::mutate(
#       dplyr::across(
#         tidyselect::where(is.numeric),
#         \(x) round(x, decimals)
#       )
#     )

#   # 10) Column order
#   cols_order <- c(
#     group_names,
#     "mean",
#     "sd",
#     "n",
#     "conf_low",
#     "conf_high"
#   )
#   cols_order <- intersect(cols_order, names(out))
#   out <- out[, cols_order, drop = FALSE]

#   out <- tibble::as_tibble(out)

#   S7::new_object(
#     tidysurvey_means,
#     out,
#     variable_label = cached_x_label,
#     variable_name = prep$x_expr,
#     group_names = group_names,
#     group_labels = cached_group_labels
#   )
# })

