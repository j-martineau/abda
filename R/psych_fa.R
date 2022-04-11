#' @name psych_fa_results
#' @title Run psych::fa
#' @desciption Submit a case by variable data file to the \code{fa} factor
#'   analysis function of the package \code{psych} using a specific rotation
#'   with every possible level of dimensionality from 1 to \code{floor(ncol(x) /
#'   2)}.
#' @param x A \code{nc} by \code{nv} numeric matrix containing variable values
#'   for \code{nv} variables and \code{nc} cases. Variables are expected to be
#'   continuous and interval or ratio scaled.
#' @param min Integer scalar: the minimum level of dimensionality to estimate.
#' @param max Integer scalar: the maximum level of dimensionality to estimate.
#' @param rotate Character scalar: the type of factor rotation to use. Valid
#'   values are described in documentation for \link[=psych::fa]{psych::fa}.
#' @param ... Additional named arguments submitted to
#'   \link[=psych::fa]{psych::fa}.
#' @return A list of lists containing the results of running
#'   \link[=psych::fa]{psych::fa} at each level of dimensionality specified. The
#'   name of each top-level element is the level of dimensionality estimated
#'   for that set of results.
#' @export
psych_fa_results <- function(x, min, max, rotate, ...) {
  y <- NULL                                                                      # initialize the result
  cat("\n Calculating correlations among variables in [x]")                      # update the user
  R <- cor(x, use = "pairwise.complete.obs", method = "pearson")                 # correlations among variables in [x]
  for (i in min:max) {                                                           # for each level of dimensionality
    cat(paste0("\n psych::fa at dim = ", i, " (", min, " to ", max, ")"))        # > update the user
    y <- c(y, list(psych::fa(R, nfactors = i, rotate = rotate)))               } # > run factor analysis and append result to list
  names(y) <- min:max                                                            # name elements with level of dimensionality
  list(results = y, rotate = rotate)                                             # add rotation to results
}
