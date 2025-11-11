#' Get variable label
#'
#' This function makes it easy to get the variable labels from either an
#' individual vector or a data frame. NOTE: it is not possible to set or modify
#' the variable labels with this function.
#'
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object.
#' @param data A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#' @param unlist Logical. If `TRUE`, the default, returns a named vector. If
#'   `FALSE`, returns a list. This only works when `x` is a `data.frame`
#' @param if_null String. This determines what to return should there be no
#'   variable label. There are three options:
#'
#'     - `NULL` - This is the default. Will return `NULL`
#'
#'     - "name" - This returns the variable name
#'
#'     - "NA" - This returns an `NA` value
#'
#' @returns If `x` is a variable or vector, a string containing the "label"
#'   attribute, if one is present, is returned. If `x` is a `data.frame` then a
#'   named vector or list with the "label" attribute from each variable is
#'   returned.
#'
#' @examples
#' # load dplyr so we can see how it might work in a typical workflow
#' library(dplyr)
#' # check for an individual vector
#' attr_var_label(test_data$top)
#' # get the variable label for the entire data set
#' attr_var_label(test_data)
#' # same, but as a list
#' attr_var_label(test_data, unlist = FALSE)
#'
#' # now let's do it on a variable without a label
#' top <- sample(c(1:3), 10, replace = TRUE)
#' # if no label is present and if_null = "name", it will use the variable name
#' attr_var_label(top, if_null = "name")
#' # if it's se to "NA" it will give NA
#' attr_var_label(top, if_null = "NA")
#'
#'
#' @export
attr_var_label <- function(data, x, unlist = TRUE, if_null = NULL) {
  if (missing(x)) {
    if (any("data.frame" == class(data))) {
      out <- purrr::map(
        names(data),
        \(x) low_var_label({{ x }}, data, if_null = if_null)
      ) |>
        stats::setNames(names(data))

      if (isTRUE(unlist)) out <- unlist(out)
    } else {
      out <- low_var_label({{ data }}, if_null = if_null)
    }
  } else {
    # if x is not missing, get a data frame with x
    # do it this way so you can select multiple variables
    data <- data |> dplyr::select({{ x }})

    # use purrr::map() to iterate low_var_label over each variable in data
    out <- purrr::map(
      names(data),
      \(x) low_var_label({{ x }}, data, if_null = if_null)
    ) |>
      stats::setNames(names(data))

    if (isTRUE(unlist)) {
      # if unlist is TRUE, unlist the output and use the names
      out <- unlist(out, use.names = TRUE)
    }
  }

  out
}

# a low level function for getting variable labels that powers the attr_var_label
low_var_label <- function(x, data, if_null = NULL) {
  x_name <- rlang::quo_get_expr(rlang::expr({{ x }}))

  if (missing(data)) {
    x <- attr(x, "label", exact = TRUE)
  } else {
    x <- attr(data[[x]], "label", exact = TRUE)
  }

  if (is.null(x) && !is.null(if_null)) {
    if (if_null == "name") {
      x <- x_name
    } else if (if_null == "NA" && !is.null(if_null)) {
      x <- NA
    }
  }
  x
}
