#' @keywords internal
# Recover design variable names (works for srvyr objects and svydesign objects)
extract_survey_design <- function(design) {
  design_name <- rlang::as_name(rlang::ensym(design))

  if (!inherits(design, "survey.design")) {
    cli::cli_abort(c(
      "`{design_name}` must be class `survey.design`.",
      "i" = "Supplied object has class `{class(design)}`."
    ))
  }

  # 1) If srvyr has already stored "survey_vars", use that
  survey_vars <- attr(design, "survey_vars")

  if (!is.null(survey_vars)) {
    out <- purrr::map(
      c("cluster", "probs", "strata", "weights", "fpc"),
      \(x) get_from_svy(design, survey_vars, x)
    ) |>
      # add the names
      stats::setNames(c("ids", "probs", "strata", "weights", "fpc"))

    # Determine if nesting is needed (overlapping PSUs in strata)
    out$nest <- FALSE
    out$nest <- should_nest(design$variables, out$ids, out$strata)
  } else {
    # use purrr::map to iterate over the function
    out <- purrr::map(
      c("id", "probs", "strata", "weights", "fpc", "nest"),
      \(x) get_from_call(design, x)
    ) |>
      # add the names
      stats::setNames(c("ids", "probs", "strata", "weights", "fpc", "nest"))
  }

  out[lengths(out) != 0]
}

#' @keywords internal
get_from_svy <- function(design, survey_vars, var) {
  if (!is.null(survey_vars[[var]])) {
    out <- rlang::as_name(survey_vars[[var]][[1]])
    if (grepl("\\+", out)) {
      out <- strsplit(out, " + ", fixed = TRUE)[[1]]
    }
  } else if (var == "cluster") {
    # need to do ids
    out <- attr(design[[var]], "terms")
    if (!is.null(out)) {
      out <- attr(out, "term.labels")
    }
  } else {
    out <- NULL
  }
  out
}

#' @keywords internal
get_from_call <- function(design, var) {
  call <- design$call

  if (!is.null(call[[var]])) {
    # if not null, get the call
    out <- call[[var]]

    # if not logical use all.vars to extract it
    if (!is.logical(out)) {
      out <- all.vars(call[[var]])
    }
  } else {
    # if null, set out to NULL
    out <- NULL
  }

  if (var == "nest" && is.null(out)) {
    # if argname is nest and out is NULL, convert to FALSE
    out <- FALSE
  }
  out
}

#' @keywords internal
should_nest <- function(df, ids, strata) {
  if (is.null(ids) || is.null(strata)) {
    return(FALSE)
  }

  ids_df <- df[, ids, drop = FALSE]
  strata_df <- df[, strata, drop = FALSE]

  # Only check top-level cluster vs top-level strata (like survey does)
  top_id <- ids_df[[1]]
  top_strata <- strata_df[[1]]

  tab <- table(top_id, top_strata)
  sc <- rowSums(tab > 0)

  any(sc > 1)
}
