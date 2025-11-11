#' Add stars based on the p-value
#' @param p.value A vector of p-values to determine what stars to give
#' @export
stars_pval <- function(p.value) {
  unclass(
    stats::symnum(
      p.value,
      corr = FALSE,
      na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", "")
    )
  )
}
