# Support vector data description (SVDD).
# Source: Tax, D. M. J. and Duin, R. P. W. (2004), Support vector
# data description, Machine Learning 54, 45-66, Eqs. 6-14
# (fetched-wave3/Support Vector Data Description.pdf).
# Mirrors Python morie.fn.deepSVDD exactly: same SMO-style pairwise
# sweep order, same clipping, same convergence test.

.svdd_kernel <- function(a, b, kern, gamma) {
  if (kern == "linear") return(sum(a * b))
  exp(-gamma * sum((a - b)^2))
}

#' Support vector data description (Tax & Duin 2004)
#'
#' Solves the dual of Eq. 6: maximise sum_i a_i K_ii - sum_ij a_i a_j
#' K_ij subject to sum_i a_i = 1 and 0 <= a_i <= C, by an SMO-style
#' pairwise sweep that keeps the sum constraint exact.  The centre is
#' a = sum_i a_i phi(x_i); the squared radius is the kernel-space
#' distance (Eq. 14) averaged over the boundary support vectors
#' (0 < a_i < C).  Objects with a_i = C fall outside the sphere.
#' \code{kkt_violation} reports the largest violation of the
#' Karush-Kuhn-Tucker conditions (Eqs. 11-13), so a correct solution
#' returns a value near zero.
#'
#' @param X Data matrix (n x d).
#' @param C Upper bound on each a_i; must be >= 1/n for feasibility.
#' @param kernel "linear" or "rbf".
#' @param gamma RBF width parameter.
#' @param tol Convergence tolerance on the largest alpha move.
#' @param max_sweeps Maximum full pairwise sweeps.
#' @return A list with elements \code{alpha}, \code{center},
#'   \code{radius2}, \code{support}, \code{outliers},
#'   \code{kkt_violation}, \code{kernel}, \code{C}, \code{method}.
#' @references Tax, D. M. J. and Duin, R. P. W. (2004). Support
#'   vector data description. Machine Learning, 54, 45-66.
#' @export
morie_deepSVDD <- function(X, C = 1, kernel = "linear", gamma = 1,
                           tol = 1e-10, max_sweeps = 500) {
  Xv <- as.matrix(X)
  n <- nrow(Xv)
  if (n < 2) stop("need at least two objects")
  C <- as.numeric(C)
  if (C < 1 / n) stop("need C >= 1/n for a feasible dual")
  kern <- tolower(as.character(kernel))
  if (!kern %in% c("linear", "rbf"))
    stop("kernel must be 'linear' or 'rbf'")
  K <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n))
    K[i, j] <- .svdd_kernel(Xv[i, ], Xv[j, ], kern, gamma)
  alpha <- rep(1 / n, n)
  grad <- function(i) K[i, i] - 2 * sum(alpha * K[i, ])
  for (sweep in seq_len(as.integer(max_sweeps))) {
    moved <- 0
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (j <= i) next
        s <- alpha[i] + alpha[j]
        lo <- max(0, s - C); hi <- min(C, s)
        if (hi - lo < 1e-15) next
        denom <- 2 * (K[i, i] - 2 * K[i, j] + K[j, j])
        gi <- grad(i); gj <- grad(j)
        new <- if (denom <= 1e-300) {
          if (gi - gj > 0) hi else lo
        } else alpha[i] + (gi - gj) / denom
        new <- min(max(new, lo), hi)
        delta <- new - alpha[i]
        if (abs(delta) > 1e-16) {
          alpha[i] <- new
          alpha[j] <- s - new
          moved <- max(moved, abs(delta))
        }
      }
    }
    if (moved < tol) break
  }
  sup <- which(alpha > 1e-8)
  boundary <- sup[alpha[sup] < C - 1e-8]
  outl <- which(alpha >= C - 1e-8)
  quad <- if (length(sup)) sum(outer(alpha[sup], alpha[sup]) *
                               K[sup, sup, drop = FALSE]) else 0
  dist2 <- function(i) K[i, i] - 2 * sum(alpha * K[i, ]) + quad
  if (length(boundary)) {
    r2s <- vapply(boundary, dist2, numeric(1))
    radius2 <- mean(r2s)
    r2_spread <- max(r2s) - min(r2s)
  } else {
    radius2 <- max(vapply(seq_len(n), dist2, numeric(1)))
    r2_spread <- 0
  }
  viol <- r2_spread
  for (i in seq_len(n)) {
    d2 <- dist2(i)
    if (alpha[i] < 1e-8 && d2 > radius2 + 1e-6)
      viol <- max(viol, d2 - radius2)
    if (alpha[i] >= C - 1e-8 && C < 1 && d2 < radius2 - 1e-6)
      viol <- max(viol, radius2 - d2)
  }
  center <- if (kern == "linear")
    as.numeric(colSums(alpha * Xv)) else NULL
  list(alpha = alpha, center = center, radius2 = radius2,
       support = as.integer(sup - 1L), outliers = as.integer(outl - 1L),
       kkt_violation = viol, kernel = kern, C = C,
       method = "SVDD (Tax & Duin 2004, Eqs. 6-14)")
}
