# Support vector data description.
# Source: Tax & Duin (2004), Machine Learning 54(1), 45-66,
# Eqs. 3-14 (fetched-wave3/Support Vector Data Description.pdf).
# Mirrors Python morie.fn.deepSVDD exactly (same deterministic
# pairwise coordinate-ascent sweep order).

.svdd_kernel <- function(A, kern, gamma) {
  if (kern == "linear") return(A %*% t(A))
  D2 <- as.matrix(stats::dist(A))^2
  exp(-gamma * D2)
}

#' Support vector data description (minimal enclosing hypersphere)
#'
#' Tax-Duin dual: maximize sum a_i K_ii - a' K a subject to
#' sum a = 1, 0 <= a <= C, by exact pairwise coordinate ascent.
#' Centre a = sum alpha_i x_i; R^2 from boundary support vectors;
#' KKT conditions (Eqs. 11-13) verified and reported.
#'
#' @param X Matrix (n x d) of training objects.
#' @param C Box constraint (>= 1/n).
#' @param kernel "linear" or "rbf".
#' @param gamma RBF width.
#' @param tol,max_sweeps Convergence controls.
#' @return A list with elements \code{alpha}, \code{center} (linear
#'   kernel), \code{radius2}, \code{support}, \code{outliers},
#'   \code{kkt_violation}, \code{kernel}, \code{C}, \code{method}.
#' @references Tax, D. M. J. and Duin, R. P. W. (2004). Support
#'   vector data description. Machine Learning, 54(1), 45-66.
#' @export
morie_svdd <- function(X, C = 1, kernel = "linear", gamma = 1,
                       tol = 1e-10, max_sweeps = 500) {
  X <- as.matrix(X)
  n <- nrow(X)
  if (n < 2) stop("need at least two objects")
  if (C < 1 / n) stop("need C >= 1/n for a feasible dual")
  kern <- tolower(kernel)
  if (!kern %in% c("linear", "rbf")) stop("kernel must be 'linear' or 'rbf'")
  K <- .svdd_kernel(X, kern, gamma)
  alpha <- rep(1 / n, n)
  grad <- function(i) K[i, i] - 2 * sum(alpha * K[i, ])
  for (sweep in seq_len(max_sweeps)) {
    moved <- 0
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        s <- alpha[i] + alpha[j]
        lo <- max(0, s - C)
        hi <- min(C, s)
        if (hi - lo < 1e-15) next
        denom <- 2 * (K[i, i] - 2 * K[i, j] + K[j, j])
        gi <- grad(i)
        gj <- grad(j)
        new <- if (denom <= 1e-300) {
          if (gi - gj > 0) hi else lo
        } else {
          alpha[i] + (gi - gj) / denom
        }
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
  out <- which(alpha >= C - 1e-8)
  dist2 <- function(i) {
    K[i, i] - 2 * sum(alpha * K[i, ]) +
      sum(outer(alpha[sup], alpha[sup]) * K[sup, sup, drop = FALSE])
  }
  if (length(boundary)) {
    r2s <- vapply(boundary, dist2, numeric(1))
    radius2 <- mean(r2s)
    spread <- max(r2s) - min(r2s)
  } else {
    radius2 <- max(vapply(seq_len(n), dist2, numeric(1)))
    spread <- 0
  }
  viol <- spread
  for (i in seq_len(n)) {
    d2 <- dist2(i)
    if (alpha[i] < 1e-8 && d2 > radius2 + 1e-6) {
      viol <- max(viol, d2 - radius2)
    }
    if (alpha[i] >= C - 1e-8 && C < 1 && d2 < radius2 - 1e-6) {
      viol <- max(viol, radius2 - d2)
    }
  }
  center <- if (kern == "linear") as.numeric(t(alpha) %*% X) else NULL
  list(alpha = alpha, center = center, radius2 = radius2,
       support = sup, outliers = out, kkt_violation = viol,
       kernel = kern, C = C,
       method = "SVDD (Tax & Duin 2004, Eqs. 6-14)")
}
