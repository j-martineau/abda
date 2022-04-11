#' @title Estimate MIRT Loading Parameters
#' @param x An \code{np × ni} matrix of 0/1 item scores with one row per person
#'   and one column per item.
#' @param dims Positive integer vector giving the set of dimensionalities to
#'   estimate parameters for.
#' @param rotate The type of rotation to use. Valid values are the same as those
#'   for the \code{\link[mirt]{summary}} function in the \code{mirt} package.
#' @param true.dim Optional integer scalar giving the true level of
#'   dimensionality to support adaptive tolerance specification
mirt_a <- function(x, dims, rotate = "promax", true.dim = NA) {
  library(mirt)                                                                  # load the mirt package
  mirt::mirtCluster(4)                                                           # setting to use parallel processing
  IT <- rep.int("2PL", ncol(x))                                                  # vector of column types
  y  <- NULL                                                                     # initialize the results list
  for (d in dims) {                                                              # for each level of dimensionality to model
    if      (is.na(true.dim)       ) {tol <- 0.0001}                             # > adaptive tolerance (0.0001 if true.dim is not specified)
    else if (abs(d - true.dim) >= 2) {tol <- 0.0010}                             # >   (0.0010 if more than 2 away from true dimensionality)
    else if (abs(d - true.dim) == 1) {tol <- 0.0005}                             # >   (0.0005 if 1 away from true dimensionality)
    else                             {tol <- 0.0001}                             # >   (0.0001 if at true dimensionality)
    cat(paste0("MIRT Exploratory Factor Analysis with ", d, " Factors\n"))       # > update the user
    if (d < 4) {V <- mirt(x, d, itemtype = IT, method = "EM"   , TOL = tol)}     # > estimate the model
    else       {V <- mirt(x, d, itemtype = IT, method = "QMCEM", TOL = tol)}     # > works better with high dimensionality
    V <- summary(V, rotate = rotate)                                             # > rotate the factor loadings
    y <- c(y, list(V$rotF))                                                    } # > append the result to the results list
  mirtCluster(remove = TRUE)                                                     # release parallel processing clusters
  names(y) <- dims                                                               # label list elements with their level of dimensionality
  y
}
