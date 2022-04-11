#' @name abda_process_data
#' @title Calculate angles from loadings
#' @param x A 2-element list with \code{x$loadings} being a list of numeric
#'   matrices with one row per variable/item/feature and one column per factor
#'   estimated for each matrix and \code{$rotate} being a character scalar
#'   giving the type of rotation used for factor analyses.
#' @return A 4-element list with \code{$rotate} being the type of rotation used
#'   in factor analysis, \code{$loadings} being a named list of D numeric
#'   matrices, \code{$angles} being a list of D symmetric numeric matrices of
#'   angles between vectors of loadings in the corresponding element of
#'   \code{$loadings}, and \code{changes} being a list of D-1 numeric matrices
#'   giving absolute differences between successive elements of \code{$angles}.
#'   \cr\cr
#'   Each element of \code{$loadings} has row names associated with the
#'   variables submitted to factor analysis and columns associated with one
#'   estimated factor.
#'   \cr\cr
#'   Each element of \code{$angles} and \code{deltas} has row names and column
#'   names associated with the variables submitted to factor analysis.
#' @export
abda_process_data <- function(x) {
  RR <- x$rotate                                                                 # the type of rotation used
  XX <- x$results                                                                # the results of running MIRT or factor analyses
  DD <- names(XX)                                                                # levels of dimensionality
  VV <- rownames(XX[[1]]$loadings)                                               # variable/item names
  ND <- length(DD)                                                               # number of dimensions
  NV <- length(VV)                                                               # number of variables/items
  LL <- AA <- CC <- NULL                                                         # initialize loadings, angles, and angle change results
  for (d in 1:ND) {                                                              # for each level of dimensionality
    cat("\n Processing data for dimensionality =", d)                            # > update the user
    D <- as.integer(DD[d])                                                       # > the current level of dimensionality
    N <- paste0(ifelse(d == 1, NA, DD[d - 1]), "→", D)                           # > label the change in dimensionality
    L <- XX[[d]]$loadings                                                        # > get the loadings from the results of the analysis
    if (D != 1) {                                                                # > if dimensionality is not 1
      A <- matrix(NA, nrow = NV, ncol = NV, dimnames = list(VV, VV))             # >> preallocate angles matrix
      for (i in 1:NV) {for (j in 1:NV) {                                         # >> for each pair of variables/items
        Theta   <- acos(cor(L[i, ], L[j, ]))                                     # >>> calculate the angle between their loading vectors
        Theta   <- 180 * Theta / pi                                              # >>> convert to degrees
        A[i, j] <- Theta                                                     }}} # >>> and store the results
    else {A <- matrix(0, nrow = NV, ncol = NV, dimnames = list(VV, VV))}         # > otherwise, all angles are 0
    if (d < 2) {C <- NULL} else {C <- list(abs(A - AA[[d - 1]])); names(C) <- N} # > if dimensionality is 1, NULL changes, otherwise, absolute difference
    L <- list(L); names(L) <- D                                                  # > put loadings in a list, and name it with the current dimensionality
    A <- list(A); names(A) <- D                                                  # > put angle changes in a list, and name it with the current dimensionality
    LL <- c(LL, L)                                                               # > append loadings
    AA <- c(AA, A)                                                               # > append angles
    CC <- c(CC, C)                                                             } # > append angle changes
  list(loadings = LL, angles = AA, changes = CC, rotate = RR)                    # final results
}
