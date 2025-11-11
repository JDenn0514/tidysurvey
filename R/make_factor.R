#' Convert a labelled vector into a factor
#'
#' @description
#'
#' `make_factor()` takes a labelled vector and converts it to a factor variable
#' using the value labels. This works with numeric, character, and factor vectors.
#'
#' This function is very similar to [`haven::as_factor()`](https://haven.tidyverse.org/reference/as_factor.html)
#' and
#' [`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
#' and is heavily based on both. However, it has some key differences. The main
#' difference compared to both functions  is that `make_factor()` adds a
#' "transformation" attribute to the new variable  indicating how it was
#' created. You can see this in the examples.
#'
#' Compared to [`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
#' it is not as extensive. For example, while [`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
#' works with data.frames and vectors, `make_factor()` only works with vectors.
#' In addition, [`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
#' has many different arguments that enable you to control the appearance of the
#' labels, NAs, and other things. `make_factor()` on the other hand is much
#' simpler. Similarly,
#' [`haven::as_factor()`](https://haven.tidyverse.org/reference/as_factor.html)
#' also enables more customization over the output of the labels. Another key
#' difference between this function and those is that if there are values without
#' labels, this function returns an error.
#'
#'
#' @param x A vector with value labels. Can be numeric, character, or a factor
#' @param ordered Logical. Determines if the factor be ordered. Defaults to `TRUE.`
#' @param drop_levels Logical. Determines if unused factor levels should be dropped.
#'   Defaults to `TRUE.`
#' @param force Logical. Determines if `x` should be forced to a vector even
#'   if there are no value labels. Defaults to `TRUE.`
#' @param na.rm Logical. Determines if tags should be removed from NAs. Defaults
#'   to `FALSE`.
#'
#' @returns A factor vector of same length as `x`.
#'
#' @examples
#'
#' library(adlgraphs)
#' library(dplyr)
#'
#' # let's make a new variable and data set
#' new_df <- test_data |>
#'   # convert top into a factor
#'   mutate(top_f = make_factor(top))
#'
#' # compare the "top_f" to "top"
#' new_df |> select(top, top_f)
#'
#' # check the attributes to see the label and transformation
#' attributes(new_df$top_f)
#'
#' @export
make_factor <- function(
  x,
  ordered = FALSE,
  drop_levels = TRUE,
  force = TRUE,
  na.rm = FALSE
) {
  # Capture a user-friendly name for x using base R.
  # Note: Using deparse(substitute(x)) works even when called as data[[x]].
  x_name <- deparse(substitute(x))

  # Retrieve the variable-level label (e.g., "Q1. Agree?"), if present.
  variable_label <- attr_var_label(x)

  # Retrieve the value labels mapping (codes -> labels).
  # Conventionally a named character vector; names(value_labels) are codes, values are labels.
  value_labels <- attr_val_labels(x)

  # If there are no value labels, coerce x in the most sensible way and return early.
  if (is.null(value_labels)) {
    if (is.factor(x)) {
      # Already a factor: preserve it. Ensure it has a variable label attribute.
      if (is.null(variable_label)) {
        attr(x, "label") <- x_name
      }
      return(x)
    } else if (is.character(x)) {
      # Character vector: convert to a factor (no value labels to enforce).
      x <- factor(x)
      # Document the transformation for auditability.
      attr(x, "transformation") <- paste0(
        "Updated '",
        x_name,
        "' from a character vector to a factor"
      )
      # Preserve/use the variable label if available; otherwise use the column name.
      attr(x, "label") <- variable_label %||% x_name
      return(x)
    } else if (is.numeric(x) && isTRUE(force)) {
      # Numeric vector without labels: force to factor if the caller asked for it.
      warning(
        "`x` has no value labels so forcing to a factor with `as.factor()`"
      )
      x <- as.factor(x)
      attr(x, "transformation") <- paste0(
        "Converted '",
        x_name,
        "' from a numeric vector to a factor"
      )
      attr(x, "label") <- variable_label %||% x_name
      return(x)
    } else if (is.numeric(x) && isFALSE(force)) {
      # Numeric vector without labels and not forcing: error to prevent silent coercion.
      cli::cli_abort(
        paste0(
          "The vector provided in `",
          x_name,
          "` does not have value labels."
        ),
        "i" = "If you want to force it to a factor, set `force = TRUE`."
      )
    }
  }

  # From here on, value labels exist. We validate that every observed value is labeled,
  # and we do so in a way that handles NA correctly and distinguishes codes from labels.

  # Define the known domains:
  # - codes: the "raw" values expected in the data (names of the label vector)
  # - labs:  the human-readable display labels (values of the label vector)
  if (haven::is.labelled(x)) {
    labs <- names(value_labels)
    codes <- unname(value_labels)
  } else {
    codes <- names(value_labels)
    labs <- unname(value_labels)
  }

  # Convert x to character for reliable comparison (works for numeric, factor, character).
  x_chr <- as.character(x)
  # Unique observed values in x.
  uvals <- unique(x_chr)
  # Exclude NA from label completeness checks; NA is not a level and shouldn’t require a label.
  uvals_no_na <- uvals[!is.na(uvals)]

  # Determine whether x currently contains codes or labels.
  # - in_codes: all non-NA observed values are within the known codes.
  # - in_labels: all non-NA observed values are within the known labels.
  in_codes <- !is.null(codes) &&
    length(codes) > 0 &&
    all(uvals_no_na %in% codes)
  in_labels <- all(uvals_no_na %in% labs)

  # If x doesn’t match either domain, we have unlabeled values; build an actionable error.
  if (!(in_codes || in_labels)) {
    # Construct the set of all known values (codes ∪ labels) for comparison.
    known <- union(codes %||% character(0), labs)
    # Which actual values are not accounted for in either domain?
    missing_vals <- setdiff(uvals_no_na, known)

    # Pretty-print the missing values, quoted. If none, provide a guard message.
    if (length(missing_vals)) {
      missing_fmt <- paste(paste0("'", missing_vals, "'"), collapse = ", ")
    }

    # Provide a compact summary of the known domains for debugging.
    domain_summary <- paste0(
      "codes: [",
      paste(codes %||% character(0), collapse = ", "),
      "]; ",
      "labels: [",
      paste(labs, collapse = ", "),
      "]"
    )

    # Abort with clear bullets:
    # - The main problem statement, including the variable name
    # - Exactly which values are unlabeled
    # - What the known domains are (helps diagnose label/codes mismatch quickly)
    cli::cli_abort(c(
      paste("Each value in `", x_name, "` must have value labels.", sep = ""),
      "x" = paste("Unlabeled values detected: ", missing_fmt, sep = ""),
      "i" = paste("Known ", domain_summary, sep = "")
    ))
  }

  # if na.rm is TRUE remove NAs
  if (na.rm) {
    x[is.na(x)] <- NA
  }

  # Map from labels -> codes so final factors use the codes as levels.
  # This aligns with your original semantics and keeps levels stable and predictable.
  nm <- names(value_labels) # codes
  values <- unname(value_labels) # labels

  # Replace any label occurrences in x with the corresponding code.
  # If x already contains codes, this is a no-op (no matches) — safe and fast enough.
  x <- replace_with(x, values, nm)

  # Establish the factor levels using the codes in the order defined by the label vector.
  x_levels <- unique(nm)
  # Optionally drop levels not present in the data (prevents extraneous zero-levels).
  if (drop_levels) {
    x_levels <- x_levels[x_levels %in% unique(x)]
  }

  # Build the final factor. Levels are codes; ordering can be controlled via `ordered`.
  x <- factor(x, levels = x_levels, ordered = ordered)

  # Attach the variable label and record the transformation for traceability.
  if (!is.null(variable_label)) {
    attr(x, "label") <- variable_label
    attr(x, "transformation") <- paste(
      "Converted '",
      x_name,
      "' into a factor based on its value labels",
      sep = ""
    )
  } else {
    attr(x, "transformation") <- paste(
      "Converted '",
      x_name,
      "' into a factor based on its value labels",
      sep = ""
    )
    attr(x, "label") <- x_name
  }

  # Return the labeled factor with preserved attributes and controlled levels.
  x
}


# from the haven package
replace_with <- function(x, from, to) {
  stopifnot(length(from) == length(to))

  if (is.numeric(x)) {
    x <- as.numeric(x)
  } else if (is.character(x)) {
    x <- as.character(x)
  }

  out <- x
  # First replace regular values
  matches <- match(x, from, incomparables = NA)
  if (anyNA(matches)) {
    out[!is.na(matches)] <- to[matches[!is.na(matches)]]
  } else {
    out <- to[matches]
  }

  # Then tagged missing values
  tagged <- haven::is_tagged_na(x)
  if (!any(tagged)) {
    return(out)
  }

  matches <- match(haven::na_tag(x), haven::na_tag(from), incomparables = NA)

  # Could possibly be faster to use anyNA(matches)
  out[!is.na(matches)] <- to[matches[!is.na(matches)]]
  out
}
