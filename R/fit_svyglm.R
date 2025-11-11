#' Calculate R-Squared and Adjusted R-Squared
#'
#' Based on McFaddden's pseudo-R-squared. This shows how good of a
#' fit the model is. **NOTE: It does not show the percentage of
#' variability explained by the model**
fit.svyglm = function(svyglm, decimals = 3) {
  if (methods::is(svyglm, "svyglm") != TRUE) {
    message("Warning: Not a svyglm object.")
  }

  # get McFadden's adjusted R squared.
  r2 <- 1 - (svyglm$deviance / svyglm$null.deviance)
  # calculate the adjustment scale
  adjust <- svyglm$df.null / svyglm$df.residual
  # calcualte adjusted r-squared
  value <- 1 - ((1 - r2) * adjust)
  # create the results
  results <- c(
    R2 = round(r2, decimals),
    adjR2 = round(value, decimals)
  )

  attr(results$R2, "label") <- "R-Squared"
  attr(results$R2, "label") <- "Adjusted R-Squared"

  return(results)
}
