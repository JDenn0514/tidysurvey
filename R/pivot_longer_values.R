#' Pivot data from wide to long with value labels
#'
#' This function is a wrapper around \code{\link[tidyr]{pivot_longer}}. This
#' function operates in pretty much the exact same way but uses the variable
#' labels from the variables specified in `cols` to make new value labels in the
#' new variable created in the `names_to` variable.
#'
#' An additional note is that this function also works with survey objects
#' created with either `survey::svydesign()` or `sryvr::as_survey_design()`.
#' The function first pivots the data, then re-creates the survey object
#' using the same variables used
#'
#' @inheritParams tidyr::pivot_longer
#' @param name_label Add a variable label to the new column with the names of
#'   the columns
#' @param ... Additional arguments passed to \code{\link[tidyr]{pivot_longer}}.
#'
#' @returns A "long" data.frame.
#'
#' @export
pivot_longer_values <- function(
  data,
  cols,
  names_to = "names",
  values_to = "values",
  name_label,
  ...
) {
  UseMethod("pivot_longer_values")
}

#' @export
pivot_longer_values.data.frame <- function(
  data,
  cols,
  names_to = "names",
  values_to = "values",
  name_label,
  ...
) {
  # convert names_to and values_to to strings
  names_to <- rlang::as_name(rlang::ensym(names_to))
  values_to <- rlang::as_name(rlang::ensym(values_to))

  # get the columns that are getting pivoted
  cols <- get_col_names(data, {{ cols }})

  # create the long data frame
  long <- data |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(cols),
      names_to = names_to,
      values_to = values_to,
      ...
    )

  # get the newly created vector of names and values
  names <- long[[names_to]]
  values <- long[[values_to]]

  # get the variable labels to go into names as value labels
  var_labs <- attr_var_label(data[cols])

  # flip the names and values of the vector
  var_labs <- stats::setNames(names(var_labs), var_labs)

  # if the
  if (missing(name_label)) {
    name_label <- attr_question_preface(data[[cols[1]]])
  }

  attr(long[[names_to]], "labels") <- var_labs
  attr(long[[names_to]], "label") <- name_label

  return(long)
}

#' @export
pivot_longer_values.tbl_svy <- function(data, ...) {
  NextMethod("pivot_longer_values")
}


#' @export
pivot_longer_values.survey.design <- function(
  data,
  cols,
  names_to = "names",
  values_to = "values",
  name_label,
  ...
) {
  # convert names_to and values_to to strings
  names_to <- rlang::as_name(rlang::ensym(names_to))
  values_to <- rlang::as_name(rlang::ensym(values_to))

  # Extract the data frame from the srvyr object
  df <- data$variables
  design_vars <- attr(data, "survey_vars")

  cols <- get_col_names(df, {{ cols }})

  # Get variable labels before pivoting
  var_labs <- attr_var_label(df[cols])
  var_labs <- stats::setNames(names(var_labs), var_labs)

  if (missing(name_label)) {
    name_label <- attr_question_preface(df[[cols[1]]])
  }

  # Perform the pivot on the data
  long_data <- df |>
    tidyr::pivot_longer(
      cols = all_of(cols),
      names_to = names_to,
      values_to = values_to,
      ...
    )

  # Update the names column with proper labels
  names_col <- long_data[[names_to]]
  names_col <- structure(
    names_col,
    labels = var_labs,
    label = name_label
  )
  long_data[[names_to]] <- names_col

  new_survey <- make_svydes_from_df(long_data, design_vars)

  return(new_survey)
}

#' @export
pivot_longer_values.svyrep.design <- function(
  data,
  cols,
  names_to = "names",
  values_to = "values",
  name_label,
  ...
) {
  # Convert names_to and values_to to strings
  names_to <- rlang::as_name(rlang::ensym(names_to))
  values_to <- rlang::as_name(rlang::ensym(values_to))

  # Extract the data frame from the srvyr object
  df <- data$variables

  # get the column names as a character vector
  cols <- get_col_names(df, {{ cols }})

  # get the cols used in repweights as a character vector
  rep_cols <- names(data$repweights)

  # Get variable labels before pivoting
  var_labs <- attr_var_label(df[cols])
  var_labs <- stats::setNames(names(var_labs), var_labs)

  if (missing(name_label)) {
    name_label <- attr_question_preface(df[[cols[1]]])
  }

  # Perform the pivot on the data
  long_data <- df |>
    tidyr::pivot_longer(
      cols = all_of(cols),
      names_to = names_to,
      values_to = values_to,
      ...
    )

  # Update the names column with proper labels
  names_col <- long_data[[names_to]]
  names_col <- structure(
    names_col,
    labels = var_labs,
    label = name_label
  )
  long_data[[names_to]] <- names_col

  # Extract other design components
  design_vars <- attr(data, "survey_vars")

  rep_info <- list(
    repweights = names(data$repweights),
    type = data$type,
    scale = data$scale,
    rscales = data$rscales
  )

  new_survey <- make_svyrep_from_df(long_data, design_vars, rep_info)

  return(new_survey)
}

make_svyrep_from_df <- function(df, design_vars, rep_info) {
  # Start with the data
  survey_args <- list(.data = df)

  # Add replicate-weighting info
  survey_args$repweights <- rep_info$repweights
  survey_args$type <- rep_info$type
  survey_args$scale <- rep_info$scale
  survey_args$rscales <- rep_info$rscales

  # Handle weights if present
  if (!is.null(design_vars$weights)) {
    weights <- rlang::as_name(design_vars$weights[[1]])
    survey_args$weights <- weights
  }

  # Special case for BRR: survey::svrepdesign does not want scale
  if (!is.null(survey_args$scale) && survey_args$type == "BRR") {
    survey_args$scale <- NULL
  }

  # Build the survey
  do.call(srvyr::as_survey_rep, survey_args)
}
