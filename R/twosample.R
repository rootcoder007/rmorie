# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: 1-Wasserstein distance between two empirical measures on the
# line. On R the p = 1 transport cost is integral |F - G|, so sorting
# solves the problem and no linear program is needed.
#' Internal: 1-Wasserstein distance between two empirical measures on
#' the
#'
#' line. On R the p = 1 transport cost is integral |F - G|, so sorting
#' solves the problem and no linear program is needed.
#'
#' @param a Numeric; passed to \code{sort}.
#' @param b Numeric; passed to \code{sort}.
#' @return A numeric value.
#' @export
.w1_distance <- function(a, b) {
  xs <- sort(a)
  ys <- sort(b)
  grid <- sort(c(xs, ys))
  width <- diff(grid)
  if (length(width) == 0L) return(0)
  left <- grid[-length(grid)]
  F <- findInterval(left, xs) / length(xs)
  G <- findInterval(left, ys) / length(ys)
  sum(abs(F - G) * width)
}

#' Two-sample permutation test on the 1-Wasserstein distance
#'
#' Uses \eqn{W_1} as the statistic, which on the line is
#' \eqn{W_1(F, G) = \int |F(x) - G(x)| dx}, the area between the two
#' empirical distribution functions. That closed form is why the test
#' needs no optimal-transport solver: the one-dimensional problem is
#' solved by sorting.
#'
#' \eqn{W_1} has no tractable null distribution, so the p-value comes
#' from permuting the group labels and ranking the observed distance
#' among the permuted ones,
#' \eqn{p = (1 + \#\{W_1^{(b)} \ge W_1^{obs}\}) / (1 + B)}.
#'
#' The test responds to any difference in distribution, not only in
#' location, because \eqn{W_1} integrates the whole gap between the CDFs
#' rather than comparing summaries.
#'
#' Mirrors \code{morie.fn.otprm} on the Python side.
#'
#' @param x,y Numeric vectors, the two samples. Matrices are rejected
#'   rather than flattened: the closed form above holds only on the line.
#' @param B Number of label permutations. Default 999.
#' @param cdf Optional function giving the null CDF of the statistic,
#'   replacing the permutation null.
#' @return Named list with \code{statistic}, \code{p_value}, \code{B},
#'   \code{m}, \code{n}, \code{null_statistics}, \code{method}.
#' @references Villani C (2009). \emph{Optimal Transport: Old and New}.
#'   Springer, Berlin. Theorem 2.18 gives the one-dimensional form.
#'
#'   Ramdas A, Garcia Trillos N & Cuturi M (2017). On Wasserstein
#'   two-sample testing and related families of nonparametric tests.
#'   \emph{Entropy}, 19(2), 47.
#' @examples
#' set.seed(1)
#' morie_wasserstein_test(rnorm(40), rnorm(40, 1), B = 99)$p_value
#' @export
morie_wasserstein_test <- function(x, y, B = 999L, cdf = NULL) {
  for (nm in c("x", "y")) {
    v <- get(nm)
    if (!is.null(dim(v))) {
      stop(nm, " must be a plain numeric vector; the closed-form W_1 used ",
           "here holds only on the line.", call. = FALSE)
    }
    if (length(v) < 2L) {
      stop(nm, " needs at least 2 observations, got ", length(v), ".", call. = FALSE)
    }
    if (!all(is.finite(v))) stop(nm, " must be finite.", call. = FALSE)
  }
  a <- as.numeric(x)
  b <- as.numeric(y)
  m <- length(a)
  n <- length(b)
  observed <- .w1_distance(a, b)

  if (!is.null(cdf)) {
    return(list(statistic = observed, p_value = 1 - cdf(observed),
                B = 0L, m = m, n = n,
                method = "W_1 two-sample test against a supplied null CDF"))
  }

  B <- as.integer(B)
  if (B < 1L) stop("B must be at least 1, got ", B, ".", call. = FALSE)
  pool <- c(a, b)
  null <- numeric(B)
  for (i in seq_len(B)) {
    perm <- sample(pool)
    null[i] <- .w1_distance(perm[1:m], perm[(m + 1L):(m + n)])
  }

  list(
    statistic = observed,
    p_value = (1 + sum(null >= observed)) / (1 + B),
    B = B, m = m, n = n,
    null_statistics = null,
    method = "W_1 two-sample permutation test"
  )
}

# Internal: Gram matrix for the three supported kernels.
#' Internal: Gram matrix for the three supported kernels
#'
#' A step of the twosample implementation. Called by \code{morie_mmd_test}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Z A matrix; passed to \code{tcrossprod}.
#' @param kernel See Usage.
#' @param gamma Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.mmd_gram <- function(Z, kernel, gamma) {
  if (identical(kernel, "linear")) return(tcrossprod(Z))
  d2 <- as.matrix(stats::dist(Z))^2
  if (identical(kernel, "rbf")) exp(-gamma * d2) else exp(-gamma * sqrt(d2))
}

#' Kernel two-sample test using the maximum mean discrepancy
#'
#' The maximum mean discrepancy is the largest difference in expectation
#' over the unit ball of a reproducing-kernel Hilbert space. Its biased
#' empirical estimate keeps the diagonal terms,
#'
#' \deqn{MMD_b^2 = \frac{1}{m^2}\sum_{i,j} k(x_i,x_j)
#'                + \frac{1}{n^2}\sum_{i,j} k(y_i,y_j)
#'                - \frac{2}{mn}\sum_{i,j} k(x_i,y_j)}
#'
#' while the unbiased estimate of Gretton et al.'s equation (3) drops
#' them, dividing the within-sample sums by \eqn{m(m-1)} and
#' \eqn{n(n-1)} over \eqn{i \ne j}.
#'
#' The null has no simple closed form, so the p-value comes from
#' permuting the group labels. Any bias is shared by the observed and
#' permuted statistics and cancels in the ranking, which is why the
#' biased form is a sound test statistic even though the unbiased one is
#' the better estimate of \eqn{MMD^2}.
#'
#' Mirrors \code{morie.fn.otmtest} on the Python side.
#'
#' @param x,y The two samples, matrices with matching column counts. A
#'   vector is read as a single column.
#' @param kernel One of "rbf", "laplacian", "linear". A characteristic
#'   kernel (rbf, laplacian) makes MMD zero only when the distributions
#'   are equal; "linear" compares means alone and is blind to any
#'   difference that leaves the mean unchanged.
#' @param B Number of label permutations. Default 999.
#' @param cdf Optional function giving the null CDF of the statistic.
#' @param gamma Kernel bandwidth. Defaults to the median heuristic on the
#'   pooled sample, 1 / median squared pairwise distance. Ignored by the
#'   linear kernel.
#' @param unbiased Use equation (3) rather than the biased estimate.
#' @return Named list with \code{statistic}, \code{p_value},
#'   \code{kernel}, \code{gamma}, \code{B}, \code{m}, \code{n},
#'   \code{unbiased}, \code{null_statistics}, \code{method}.
#' @references Gretton A, Borgwardt KM, Rasch MJ, Scholkopf B & Smola A
#'   (2012). A kernel two-sample test. \emph{Journal of Machine Learning
#'   Research}, 13, 723-773.
#' @examples
#' set.seed(1)
#' morie_mmd_test(rnorm(30), rnorm(30, 2), B = 99)$p_value
#' @export
morie_mmd_test <- function(x, y, kernel = "rbf", B = 999L, cdf = NULL,
                           gamma = NULL, unbiased = FALSE) {
  as2d <- function(v, nm) {
    A <- if (is.null(dim(v))) matrix(as.numeric(v), ncol = 1L) else as.matrix(v)
    if (!all(is.finite(A))) stop(nm, " must be finite.", call. = FALSE)
    A
  }
  A <- as2d(x, "x")
  Bm <- as2d(y, "y")
  if (ncol(A) != ncol(Bm)) {
    stop("x and y must share a feature dimension; got ", ncol(A), " and ",
         ncol(Bm), ".", call. = FALSE)
  }
  m <- nrow(A)
  n <- nrow(Bm)
  if (m < 2L || n < 2L) {
    stop("Both samples need at least 2 observations, got m=", m, ", n=", n, ".",
         call. = FALSE)
  }
  if (!kernel %in% c("rbf", "laplacian", "linear")) {
    stop("kernel must be one of rbf, laplacian, linear; got ", kernel, ".",
         call. = FALSE)
  }

  Z <- rbind(A, Bm)
  if (is.null(gamma)) {
    d2 <- as.matrix(stats::dist(Z))^2
    med <- stats::median(d2[upper.tri(d2)])
    gamma <- if (is.finite(med) && med > 0) 1 / med else 1
  }
  gamma <- as.numeric(gamma)
  if (gamma <= 0) stop("gamma must be positive, got ", gamma, ".", call. = FALSE)
  K <- .mmd_gram(Z, kernel, gamma)

  mmd2 <- function(ix, iy) {
    Kxx <- K[ix, ix, drop = FALSE]
    Kyy <- K[iy, iy, drop = FALSE]
    Kxy <- K[ix, iy, drop = FALSE]
    mm <- length(ix)
    nn <- length(iy)
    if (unbiased) {
      sxx <- (sum(Kxx) - sum(diag(Kxx))) / (mm * (mm - 1))
      syy <- (sum(Kyy) - sum(diag(Kyy))) / (nn * (nn - 1))
    } else {
      sxx <- sum(Kxx) / mm^2
      syy <- sum(Kyy) / nn^2
    }
    sxx + syy - 2 * sum(Kxy) / (mm * nn)
  }

  ix <- seq_len(m)
  iy <- m + seq_len(n)
  observed <- mmd2(ix, iy)

  if (!is.null(cdf)) {
    return(list(statistic = observed, p_value = 1 - cdf(observed),
                kernel = kernel, gamma = gamma, B = 0L, m = m, n = n,
                unbiased = unbiased,
                method = "MMD two-sample test against a supplied null CDF"))
  }

  B <- as.integer(B)
  if (B < 1L) stop("B must be at least 1, got ", B, ".", call. = FALSE)
  null <- numeric(B)
  for (i in seq_len(B)) {
    perm <- sample.int(m + n)
    null[i] <- mmd2(perm[ix], perm[iy])
  }

  list(
    statistic = observed,
    p_value = (1 + sum(null >= observed)) / (1 + B),
    kernel = kernel, gamma = gamma, B = B, m = m, n = n,
    unbiased = unbiased,
    null_statistics = null,
    method = "MMD two-sample permutation test (Gretton et al. 2012)"
  )
}
