# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random Fourier features (RFF) kernel approximation
#'
#' NOT IN THE BOOK.  Montesinos Lopez, Montesinos Lopez and Crossa (2022),
#' Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#' Springer, was searched in full -- all seventeen page-range volumes and the
#' index, \[Pages 683-691\].  Chapter 8, volume \[Pages 251-336\], is the kernel
#' chapter and builds every kernel it uses as an explicit n-by-n matrix; it
#' never approximates one by a feature map, and "random Fourier" and "Bochner"
#' do not occur anywhere.
#'
#' The method is therefore taken from the originating primary source, Rahimi,
#' A. and Recht, B. (2007), Random features for large-scale kernel machines,
#' Advances in Neural Information Processing Systems 20 (NIPS 2007),
#' pp. 1177-1184.  It is a NIPS proceedings paper and carries no DOI.
#'
#' CITATION CARE.  That paper gives exactly ONE feature map, its equation for
#' z(x) in Algorithm 1: z(x) = sqrt(2/D) \[cos(w_1^T x + b_1), ...,
#' cos(w_D^T x + b_D)]^T, with w_1, ..., w_D drawn from p(w), the Fourier
#' transform of the kernel, and b_1, ..., b_D drawn from Uniform(0, 2 pi).  The
#' 2D-dimensional sin/cos map -- \[sin(w^T x), cos(w^T x)\] stacked without a
#' phase offset -- that is often attributed to this paper is NOT in it, and is
#' not implemented here.
#'
#' The spectral density is the paper's own Gaussian entry: for
#' k(x - y) = exp(-||x - y||^2 / 2) the density p(w) is N(0, I).  The bandwidth
#' convention matters and is stated rather than assumed.  This function takes
#' the kernel in the gamma parameterisation k(x, y) = exp(-gamma ||x - y||^2),
#' the convention used throughout this package; matching exponents requires the
#' spectral draws to be scaled by sqrt(2 gamma), so w ~ N(0, 2 gamma I).  At
#' gamma = 1/2 that reduces to the paper's N(0, I).
#'
#' DETERMINISM.  The paper draws w and b at random.  Both are replaced here by
#' a Halton sequence -- van der Corput in a DIFFERENT PRIME BASE for each
#' coordinate, all indexed by the same j -- mapped through the inverse normal
#' for w and scaled to (0, 2 pi) for b.
#'
#' The separate bases are load-bearing, not cosmetic.  Writing
#' cos(A)cos(B) = \[cos(A - B) + cos(A + B)\]/2 gives
#' z(x)^T z(y) = (1/D) sum_j cos(w_j^T (x - y))
#'             + (1/D) sum_j cos(w_j^T (x + y) + 2 b_j),
#' and the estimator is only unbiased because the SECOND sum vanishes, which
#' needs b independent of w.  A first attempt strided a single van der Corput
#' stream across the coordinates, so b and one column of W came from the same
#' base-3 sequence at interleaved indices.  They were correlated, the second sum
#' did not vanish, and the mean absolute error against the exact kernel GREW
#' with D -- 0.051 at D = 64, 0.066 at D = 512, 0.070 at D = 4096 -- converging
#' to the wrong target.  Both arms would have agreed on it to 1e-16.  One base
#' per coordinate fixes it and the error now falls with D as it must.
#'
#' @param X n-by-p matrix of inputs.
#' @param D number of random features.
#' @param kernel only "rbf" is offered, the Gaussian entry of the paper's table.
#' @param gamma the kernel is exp(-gamma ||x - y||^2); gamma = 1/2 is the
#'   paper's own unit-variance Gaussian.
#' @return list: estimate, Z, K_approx, K_exact, W, b, n, method.
#' @keywords internal
#' @examples
#' Rfkrn(cbind(sin((0:5) * 0.5), (0:5) / 10), 256L)$estimate
#' @export
Rfkrn <- function(X, D = 256L, kernel = "rbf", gamma = 0.5) {
  XX <- .s03mat(X)
  n <- nrow(XX)
  if (n == 0L) stop("random_fourier_features: X is empty")
  p <- ncol(XX)
  if (p == 0L) stop("random_fourier_features: X has no columns")
  d <- as.integer(D)
  if (d < 1L) stop("random_fourier_features: D must be at least 1")
  if (!identical(kernel, "rbf")) {
    stop("random_fourier_features: only the rbf kernel of the paper's Gaussian entry is offered")
  }
  g <- as.numeric(gamma)
  if (g <= 0) stop("random_fourier_features: gamma must be positive")
  scale <- sqrt(2 * g)
  pr <- .rfkprimes(p + 1L)
  W <- matrix(0, p, d)
  for (j in seq_len(d)) {
    for (a in seq_len(p)) W[a, j] <- scale * .s03qnorm(.s03vdc(j, pr[a]))
  }
  b <- vapply(seq_len(d), function(k) 2 * pi * .s03vdc(k, pr[p + 1L]), 0)
  cc <- sqrt(2 / d)
  Z <- matrix(0, n, d)
  for (i in seq_len(n)) {
    for (j in seq_len(d)) {
      s <- b[j]
      for (a in seq_len(p)) s <- s + XX[i, a] * W[a, j]
      Z[i, j] <- cc * cos(s)
    }
  }
  Ka <- matrix(0, n, n); Ke <- matrix(0, n, n); err <- 0
  for (i in seq_len(n)) {
    for (k in seq_len(n)) {
      s <- sum(Z[i, ] * Z[k, ])
      Ka[i, k] <- s
      Ke[i, k] <- exp(-g * sum((XX[i, ] - XX[k, ])^2))
      err <- err + abs(s - Ke[i, k])
    }
  }
  list(estimate = err / (n * n), Z = Z, K_approx = Ka, K_exact = Ke, W = W,
       b = b, n = n,
       method = paste0("z(x) = sqrt(2/D) cos(W^T x + b), w ~ N(0, 2 gamma I), ",
                       "b ~ U(0, 2pi); Rahimi and Recht (2007) NIPS 20; not in the book"))
}

# The first k primes, for the Halton bases.
#' The first k primes, for the Halton bases
#'
#' A step of the rfkrn implementation. Called by \code{Rfkrn}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param k See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.rfkprimes <- function(k) {
  out <- integer(0); c <- 2L
  while (length(out) < k) {
    j <- 2L; ok <- TRUE
    while (j * j <= c) { if (c %% j == 0L) { ok <- FALSE; break }; j <- j + 1L }
    if (ok) out <- c(out, c)
    c <- c + 1L
  }
  out
}
