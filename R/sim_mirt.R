#' @name sim_mirt
#' @title Simulate multidimensional item score data and factor analyze it
#' @description Settings include the levels of dimensionality to simulate,
#'   correlation among dimensions, number of items, number of persons, and
#'   type of factor rotation.
#' @param A positive integer vector giving the set of dimensionalities to run.
#' @param corr a numeric vector giving the set of interdimension correlations to
#'   run.
#' @param ni positive integer scalar giving the number of items to analyze.
#' @param np positive integer scalar giving the number of people to analyze.
#' @param rotate character scalar naming the type of rotation to use.
sim_mirt <- function(dims = 1:6, corr = c(0, 0.5, 0.9, 0.95), ni = 30, np = 10000, rotate = "promax") {
  library(MASS); library(tidyverse); library(mirt)                               # load required packages
  y <- NULL                                                                      # initialize results
  for (d in dims) {for (r in corr) {                                             # for each combination of level of dimensionality and level of interdimension correlation
    cat("\nDimensionality =", d, "and interdimension correlation =", r, "\n")    # > update the user
    N <- 1:max(5, 2 * d)                                                         # > specify the levels of dimensionality to estimate
    D <- gen_design(d, ni)                                                       # > generate the design matrix
    t <- gen_thetas(np, rnorm(d), r)                                             # > generate the person thetas
    p <- gen_item_pars(D)                                                        # > generate the item parameters
    x <- gen_item_scores(t, p$a, p$d)                                            # > generate the item scores
    a <- mirt_a(x, N, rotate = rotate, true.dim = d)                             # > estimate the MIRT model
    t <- list(d = d, r = r, ni = ni, np = np, design = D, theta = t,             # > place the results in a list of lists
              a.gen = p$a, d.gen = p$d, a.est = a, rotate = rotate)              # >   using the specified settings
    y <- c(y, list(t))                                                        }} # > append to the results list
  y
}
