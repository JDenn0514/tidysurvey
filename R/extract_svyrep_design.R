#' @keywords internal
extract_svyrep_design <- function(design) {
  design_name <- rlang::as_name(rlang::ensym(design))

  if (!inherits(design, "svyrep.design")) {
    cli::cli_abort(c(
      "`{design_name}` must be class `svyrep.design`.",
      "i" = "Supplied object has class `{class(design)}`."
    ))
  }

  # If srvyr object, check survey_vars first
  survey_vars <- attr(design, "survey_vars")

  if (!is.null(survey_vars)) {
    weights <- survey_vars$weights[[1]]
    if (!is.null(weights)) {
      weights <- rlang::as_name(weights)
    }
  } else {
    # Plain svyrep.design
    weights <- all.vars(design$call$weights)
  }

  # Initialize output list
  out <- list(
    repweights = names(design$repweights),
    weights = weights,
    type = design$type,
    combined_weights = design$combined.weights,
    rho = design$rho,
    scale = design$scale,
    rscales = design$rscales,
    mse = design$mse
  )

  if (out$type %in% c("JK2", "successive-difference", "ACS")) {
    out$scale <- NULL
    out$rscales <- NULL
  }

  if (out$type == "BRR") {
    out$scale <- NULL
  }

  out[lengths(out) != 0]
}
