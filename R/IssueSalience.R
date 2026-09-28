#' Issue salience, attention and valence
#'
#' \code{AttentionPunctuation}: kurtosis and L-kurtosis of pooled percentage
#' changes in issue attention (Jones and Baumgartner). \code{ValenceConvergence}:
#' Schofield's convergence coefficient and characteristic matrix for the
#' mean-voter equilibrium with valence. \code{StructureInducedEquilibrium}:
#' Shepsle's issue-by-issue weighted median. Identical to the Python arm
#' \code{morie.fn.salience}.
#'
#' @param series List of attention (or budget) series, one per issue.
#' @param ideals Voter ideal points (matrix, one row per voter).
#' @param valence Party valences.
#' @param beta Spatial coefficient.
#' @param weights Optional voter weights.
#' @return A list, or a numeric vector for \code{StructureInducedEquilibrium}.
#' @references Jones, B. D. and Baumgartner, F. R. (2005). The Politics of
#'   Attention. University of Chicago Press.
#'
#'   Hosking, J. R. M. (1990). L-moments. Journal of the Royal Statistical
#'   Society B 52, 105-124.
#'
#'   Schofield, N. (2007). The mean voter theorem: necessary and sufficient
#'   conditions for convergent equilibrium. Review of Economic Studies 74,
#'   965-980.
#'
#'   Shepsle, K. A. (1979). Institutional arrangements and equilibrium in
#'   multidimensional voting models. American Journal of Political Science 23,
#'   27-59.
#' @examples
#' AttentionPunctuation(list(c(10, 11, 10, 30, 29), c(5, 5, 6, 6, 2)))$l_kurtosis
#' StructureInducedEquilibrium(rbind(c(0, 3), c(2, 1), c(1, 0)))
#' @export
AttentionPunctuation <- function(series) {
  ch <- unlist(lapply(series, function(v) diff(v) / v[-length(v)]))
  x <- sort(ch)
  n <- length(x)
  m <- sum(x) / n
  m2 <- sum((x - m)^2) / n
  m4 <- sum((x - m)^4) / n
  i <- seq_len(n) - 1
  b <- vapply(0:3, function(r) {
    w <- rep(1, n)
    for (k in seq_len(r)) w <- w * (i - (k - 1)) / (n - k)
    sum(w * x) / n
  }, numeric(1))
  l2 <- 2 * b[2] - b[1]
  l3 <- 6 * b[3] - 6 * b[2] + b[1]
  l4 <- 20 * b[4] - 30 * b[3] + 12 * b[2] - b[1]
  list(changes = ch, kurtosis = m4 / m2^2, l_moments = c(b[1], l2, l3 / l2, l4 / l2), l_kurtosis = l4 / l2)
}

#' @rdname AttentionPunctuation
#' @export
ValenceConvergence <- function(ideals, valence, beta) {
  X <- as.matrix(ideals)
  n <- nrow(X)
  w <- ncol(X)
  Xc <- sweep(X, 2, colSums(X) / n)
  V <- crossprod(Xc) / n
  j <- which.min(valence)
  e <- exp(valence - max(valence))
  rho <- e / sum(e)
  A <- beta * (1 - 2 * rho[j])
  C <- 2 * A * V - diag(w)
  ev <- sort(eigen(C, symmetric = TRUE, only.values = TRUE)$values)
  cc <- 2 * A * sum(diag(V))
  list(c = cc, rho = rho, A = A, lowest = j - 1, characteristic_matrix = C, eigenvalues = ev,
       local_equilibrium = max(ev) < 0, necessary = cc < w)
}

#' @rdname AttentionPunctuation
#' @export
StructureInducedEquilibrium <- function(ideals, weights = NULL) {
  X <- as.matrix(ideals)
  n <- nrow(X)
  wt <- if (is.null(weights)) rep(1, n) else weights
  vapply(seq_len(ncol(X)), function(k) {
    o <- order(X[, k], seq_len(n))
    X[o, k][which(cumsum(wt[o]) >= sum(wt) / 2)[1]]
  }, numeric(1))
}
