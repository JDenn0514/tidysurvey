#' Nest a data frame or survey object by one or more grouping variables
#'
#' @description
#' `make_nested()` creates a nested tibble where each row corresponds to a
#' unique combination of one or more grouping variables. The resulting tibble
#' contains a list-column `data` with the subset of the original data for
#' that group.
#'
#' This function has S3 methods for `data.frame`, `survey.design`, and
#' `svyrep.design` objects. For more information on how this function
#' differs from `tidyr::nest()` and how it works for `survey.design` and
#' `svyrep.design` objects, check the details section below.
#'
#' @param data A data frame or survey design object (`survey.design`).
#' @param group One or more grouping variables. Can be supplied as bare names
#'   (unquoted) or a character vector of column names.
#' @param na.rm Logical, whether to remove rows with missing values in the
#'   grouping variables. Defaults to `TRUE`.
#' @param sep Character string used to separate levels when composing the
#'   `name` column for each group. Defaults to `"_"`.
#'
#' @details
#'
#' While `make_nested()` operates similarly to `tidyr::nest()`, there are
#' some important differences:
#' * It is optimized for survey data and preserves weights, strata, and PSU
#'   information when nesting `survey.design` objects and replication
#'   weights when nesting `svyrep.design` objects.
#' * It uses `vctrs::vec_split()` for faster grouping.
#' * The output always contains a `name` column that concatenates the levels
#'   of the grouping variables, which is useful for labeling and further
#'   analysis.
#' * Unlike `tidyr::nest()`, you cannot control which columns go into the
#'   inner list-column; the inner data frames always include all columns
#'   not used for grouping.
#' * Unlike `tidyr::nest()` where `.by` supersedes any `dplyr::group_by()`
#'   grouping, `make_nested()` combines any existing `dplyr::group_by()`
#'   variables with the supplied `group` argument when nesting.
#'
#' When nesting a `survey.design` object, each nested group is subsetted and
#' converted into a new `survey.design` object using `srvyr::as_survey_rep`.
#' The `.data`, `ids`, `strata`, `weights`, `fpc`, and `nest` arguments are
#' specified.
#'
#' **Note:** If a nested group contains only one primary sampling unit
#' (PSU), `svydesign()` cannot compute variance estimates and the function
#' will fail for that group. This is most likely to occur with small
#' datasets, highly granular grouping variables, or a small number of PSUs
#' per stratum. For typical survey datasets with sufficient PSUs, this is
#' unlikely to occur.
#'
#' When nesting a `svyrep.design` object, each nested group is subsetted and
#' converted into a new `svyrep.design` object using
#' `srvyr::as_survey_rep()`.
#'
#' **Note:** The finite population corrections
#' (`fpc`) are not carried over when
#'
#' @returns A tibble with one row per unique combination of the grouping
#'   variables. The tibble contains the following columns:
#'   - The grouping variables
#'   - `data`: a list-column with the subset of data or survey design for each group
#'   - `name`: a string combining the levels of the grouping variables
#'
#' @examples
#' # Example with a data frame
#' df <- make_basic_df()
#' nested_df <- make_nested(df, grp)
#'
#' # Nested by multiple variables
#' nested_df2 <- make_nested(df, c(grp, x2))
#'
#' # Example with a survey.design object
#' library(survey)
#' design <- svydesign(
#'   ids = ~psu,
#'   strata = ~strata,
#'   weights = ~wts,
#'   data = df,
#'   fpc = ~fpc_psu
#' )
#' nested_svy <- make_nested(design, grp)
#'
#' @export
make_nested <- function(data, group, na.rm = TRUE, sep = "_") {
  UseMethod("make_nested")
}

#' @export
make_nested.data.frame <- function(data, group, na.rm = TRUE, sep = "_") {
  group_names <- compose_group_names(data, {{ group }})

  if (!length(group_names) || is.null(group_names)) {
    stop("No grouping variables were provided. Must supply at least one.")
  }

  # split up the data frame using vec_split
  res <- vctrs::vec_split(
    # the data frame to split, use setdiff to get the columns not in group_names
    x = data[!names(data) %in% c(group_names)],
    # split the data by the grouping variables
    by = data[group_names]
  )
  # create a nested data frame based on the split data from res
  nest_data <- vctrs::vec_cbind(
    # this creates columns with the levels from the variables used to split the data
    res$key,
    # this creates a new tibble from each combination of levels used to split the data
    tibble::new_tibble(list(data = res$val))
  )
  # get the columns in group_names as a list and unname it
  cols <- unname(as.list(nest_data[group_names]))
  # using the list of columns, paste them together using do.call and paste
  nest_data$name <- do.call(paste, c(cols, sep = sep))

  if (na.rm) {
    nest_data <- stats::na.omit(nest_data)
  }

  sort_by(nest_data, nest_data[group_names])
}

#' @export
make_nested.tbl_svy <- function(data, ...) {
  NextMethod("make_nested")
}


#' @export
make_nested.survey.design <- function(data, group, na.rm = TRUE, sep = "_") {
  # Compose the grouping variable names
  group_names <- compose_group_names(data, {{ group }})
  if (length(group_names) == 0) {
    stop("No grouping variables provided.")
  }

  # 2. extract survey data
  survey_data <- data$variables

  # split up the data frame using vec_split
  res <- vctrs::vec_split(
    # the data frame to split, use setdiff to get the columns not in group_names
    x = survey_data[setdiff(names(survey_data), group_names)],
    # split the data by the grouping variables
    by = survey_data[group_names]
  )

  # extract survey design
  survey_vars <- extract_survey_design(data)

  # create nested surveys
  nested_survey <- purrr::map(
    res$val,
    \(x) {
      survey_vars <- c(list(.data = x), survey_vars)
      do.call(srvyr::as_survey_design, survey_vars)
    }
  )

  # create new tibble with a column for res$key and teh list of nested_surveys
  nested_data <- vctrs::vec_cbind(
    res$key,
    tibble::new_tibble(list(data = nested_survey))
  )

  # Compose group name strings
  cols <- unname(as.list(nested_data[group_names]))
  nested_data$name <- do.call(paste, c(cols, sep = sep))

  if (na.rm) {
    nested_data <- stats::na.omit(nested_data)
  }

  # Sort by group variables
  sort_by(nested_data, nested_data[group_names])
}

#' @export
make_nested.svyrep.design <- function(data, group, na.rm = TRUE, sep = "_") {
  # 1. Compose the grouping variable names
  group_names <- compose_group_names(data, {{ group }})

  if (length(group_names) == 0) {
    stop("No grouping variables provided.")
  }

  # 2. extract survey data
  survey_data <- data$variables

  # split up the data frame using vec_split
  res <- vctrs::vec_split(
    # the data frame to split, use setdiff to get the columns not in group_names
    x = survey_data[setdiff(names(survey_data), group_names)],
    # split the data by the grouping variables
    by = survey_data[group_names]
  )

  survey_vars <- extract_svyrep_design(data)

  # 9. create a list of survey objects based on the split data from res
  nested_survey <- purrr::map(
    res$val,
    \(x) {
      survey_vars <- c(list(.data = x), survey_vars)
      do.call(srvyr::as_survey_rep, survey_vars)
    }
  )

  # 10. Combine the group keys with the nested data
  nested_data <- vctrs::vec_cbind(
    res$key,
    tibble::new_tibble(list(data = nested_survey))
  )

  # 11. Compose group name strings
  cols <- unname(as.list(nested_data[group_names]))
  nested_data$name <- do.call(paste, c(cols, sep = sep))

  if (na.rm) {
    nested_data <- stats::na.omit(nested_data)
  }

  # 12. Sort by group variables
  sort_by(nested_data, nested_data[group_names])
}
