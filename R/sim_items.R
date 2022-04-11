#' @name sim_items
#' @title Generate a simulated MIRT item design matrix (\code{gen_design}),
#'   multidimensional \strong{θ} (\code{gen_thetas}), 2PL MIRT item intercepts
#'   and multidimensional item slopes (\code{gen_item_pars}), and dichotomous
#'   item-score data (\code{gen_item_scores}).
#' @param nd Positive integer scalar giving the number of dimensions.
#' @param ni Positive integer scalar giving the number of items (must be greater
#'   than or equal to the number of dimensions).
#' @param np Positive integer scalar giving the number of persons.
#' @param m Numeric vector of length \code{nd} giving the means of the \code{nd}
#'   \code{θ} vectors to generate.
#' @param r An \code{nd × nd} correlation matrix.
#' @param design An \code{ni × nd} indicator (0/1) matrix showing the
#'   dimension(s) each item measures.
#' @param t An \code{np × nd} matrix of \code{θ} values.
#' @param a An \code{ni × nd} matrix of item slope values.
#' @param d A vector of \code{ni} item intercept values.
#' @param D Scaling factor (1 for a standard logistic curve, 1.702 for
#'   similarity to a normal ogive).
#' @return \code{gen_design} returns an \code{ni × nd} indicator (0/1) matrix
#'   identifying which dimension(s) each item measures.
#'   \cr\cr
#'   \code{gen_thetas} returns an \code{np × nd} matrix of \code{θ} values with
#'   mean and covariance defined by \code{mu} and \code{s}.
#'   \cr\cr
#'   \code{gen_item_pars} returns a two-element list with \code{$a} being an
#'   \code{ni × nd} numeric matrix of item slope parameters and \code{$d} being
#'   a numeric vector of length \code{ni} containing item intercept parameters.
#'   \cr\cr
#'   \code{gen_item_scores} returns an \code{np × ni} matrix of dichotomous item
#'   scores.
gen_design <- function(nd, ni) {
  y <- matrix(0, nrow = ni, ncol = nd)                                           # initialize the results matrix
  iLeft <- ni                                                                    # the number of items left to allocate to dimensions
  dLeft <- nd                                                                    # the number of dimensions left to allocate to items
  Start <- 1                                                                     # initialize the starting item of the first dimension
  for (d in 1:nd) {                                                              # for each dimension d
    N   <- ceiling(iLeft / dLeft)                                                # > calculate the number of items to allocate to the dimension
    End <- Start + N - 1                                                         # > calculate the index number of the dimension's last item
    y[Start:End, d] <- 1                                                         # > store an indicator in the d-th column for the dimension's item block
    Start <- End + 1                                                             # > the next dimension's first item index number
    iLeft <- iLeft - N                                                           # > subtract the number of items in dimension d's item block from the remaining item count
    dLeft <- dLeft - 1                                                           # > subtract 1 from the remaining dimension count
  }
  y
}

#' @rdname sim_items
#' @export
gen_thetas <- function(np, m, r = NA) {
  nd <- length(m)                                                                # number of dimensions
  Names <- names(m)                                                              # dimension names
  if (is.null(Names)) {Names <- paste0("theta.", 1:nd)}                          # make default names if needed
  if (length(r) == 1) {r <- matrix(r, nrow = nd, ncol = nd); diag(r) <- 1}       # inter-dimension correlations are all the same if [r] is a scalar
  matrix(data = MASS::mvrnorm(n = np, mu = m, Sigma = r),                        # generate thetas with the specified means and correlation matrix
         nrow = np, ncol = nd, dimnames = list(NULL, Names))
}

#' @rdname sim_items
#' @export
gen_item_pars <- function(design) {
  i    <- which(design == 1)                                                     # index the tagged elements of the design matrix
  na   <- length(i)                                                              # number of a-values to generate
  ni   <- nrow(design)                                                           # number of items
  d    <- rnorm(ni, 0, 1)                                                        # generate standard normal slope parameters
  a    <- design                                                                 # preallocate the slope matrix
  A    <- pmin(4, 0.5 + 0.5 * rlnorm(na, 0, 1))                                  # generate lognormal slopes with a min of 0.5, a max of 4.0, and half the variance of standard lognormal
  a[i] <- A                                                                      # store in the right locations
  list(a = a, d = d)
}

#' @rdname sim_items
#' @export
gen_item_scores <- function(t, a, d, D = 1.702) {
  i <- rep.int(1, nrow(t))                                                       # generate a vector of 1s (one per item)
  e <- -D * (t %*% t(a) + i %*% t(d))                                            # value of the exponent
  p <- 1 / (1 + exp(e))                                                          # convert to probability of correct response
  r <- matrix(runif(length(p)), nrow = nrow(p))                                  # uniform random numbers
  y <- (r < p) / 1                                                               # if random is less than probability, score as 1
  colnames(y) <- paste0("item.", 1:length(d))                                    # add column names, identifying them as items
  y
}
