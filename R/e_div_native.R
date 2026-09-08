# E-divisive: hierarchical change-point estimation by energy distance.
# Source: Matteson, D. S. and James, N. A. (2014), A nonparametric
# approach for multiple change point analysis of multivariate data,
# Journal of the American Statistical Association 109(505), 334-345.
#
# Their eq. (5) is the empirical divergence between two samples,
#   E(X, Y; alpha) = (2/mn) sum |X_i - Y_j|^a
#                  - C(n,2)^{-1} sum_{i<k} |X_i - X_k|^a
#                  - C(m,2)^{-1} sum_{j<k} |Y_j - Y_k|^a,
# scaled in their eq. (6) to Q(X, Y) = (mn/(m+n)) E; eq. (7) locates a
# change point by maximising Q over both the split and the right-hand
# window.  Because E is zero if and only if the two distributions
# coincide (for 0 < alpha < 2), the method detects ANY distributional
# change, not only a change in mean.  Significance comes from the
# within-segment permutation test of their Sec. 3.
#
# Native implementation mirroring Python morie.fn.e_div exactly: the
# same 2-D prefix sums, the same row-major (tau, kappa) scan with a
# strict ">" so ties resolve identically, the same Fisher-Yates
# within-cluster shuffle consuming the shared Philox stream in the
# same order, and the same (count + 0) / (R + 1) p-value.

#' .mor_ed_dist
#'
#' A step of the e_div_native implementation. Called by \code{morie_e_div}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Passed to \code{dist}.
#' @param alpha Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .mor_ed_dist(z = y, alpha = 0.5)
#' res
.mor_ed_dist <- function(z, alpha) as.matrix(dist(z))^alpha

# P[i + 1, j + 1] = sum of D[1..i, 1..j]
#' P\[i + 1, j + 1\] = sum of D\[1..i, 1..j\]
#'
#' A step of the e_div_native implementation. Called by \code{morie_e_div}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param D A matrix; indexed by row and column.
#' @return The value of \code{P}, as built in the body.
#' @export
.mor_ed_prefix <- function(D) {
  n <- nrow(D)
  P <- matrix(0, n + 1L, n + 1L)
  for (i in seq_len(n))
    P[i + 1L, -1L] <- P[i, -1L] + cumsum(D[i, ])
  P
}

# sum of D over the 0-based half-open block [a1, b1) x [a2, b2)
#' Sum of D over the 0-based half-open block [a1, b1) x [a2, b2)
#'
#' A step of the e_div_native implementation. Called by \code{.mor_ed_qhat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param P A matrix; indexed by row and column.
#' @param a1 Numeric; combined arithmetically in the body.
#' @param b1 Numeric; combined arithmetically in the body.
#' @param a2 Numeric; combined arithmetically in the body.
#' @param b2 Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mor_ed_block <- function(P, a1, b1, a2, b2)
  P[b1 + 1L, b2 + 1L] - P[a1 + 1L, b2 + 1L] - P[b1 + 1L, a2 + 1L] +
    P[a1 + 1L, a2 + 1L]

#' .mor_ed_qhat
#'
#' A step of the e_div_native implementation. Called by \code{.mor_ed_best_split}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param P Passed to \code{.mor_ed_block}.
#' @param a Numeric; combined arithmetically in the body.
#' @param tau Numeric; combined arithmetically in the body.
#' @param kappa Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mor_ed_qhat <- function(P, a, tau, kappa) {
  n1 <- tau - a
  m1 <- kappa - tau
  between <- .mor_ed_block(P, a, tau, tau, kappa)
  withinX <- .mor_ed_block(P, a, tau, a, tau) / 2
  withinY <- .mor_ed_block(P, tau, kappa, tau, kappa) / 2
  e <- (2 / (n1 * m1)) * between -
    withinX / (n1 * (n1 - 1) / 2) -
    withinY / (m1 * (m1 - 1) / 2)
  (n1 * m1 / (n1 + m1)) * e
}

#' .mor_ed_best_split
#'
#' A step of the e_div_native implementation. Called by \code{morie_e_div}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param P Passed to \code{.mor_ed_qhat}.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param min_size Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.mor_ed_best_split <- function(P, a, b, min_size) {
  bestq <- -Inf
  bt <- -1L
  bk <- -1L
  lo <- a + min_size
  hi <- b - min_size
  if (hi >= lo) for (tau in seq.int(lo, hi)) {
    k0 <- tau + min_size
    if (k0 <= b) for (kappa in seq.int(k0, b)) {
      q <- .mor_ed_qhat(P, a, tau, kappa)
      if (q > bestq) { bestq <- q
      bt <- tau
      bk <- kappa }
    }
  }
  c(bestq, bt, bk)
}

# Fisher-Yates within each cluster, consuming us[pos ...] in the same
# order as the Python arm.
#' Fisher-Yates within each cluster, consuming us\[pos ...\] in the same
#'
#' order as the Python arm.
#'
#' @param order A vector; indexed elementwise.
#' @param clusters A matrix; indexed by row and column.
#' @param us A vector; indexed elementwise.
#' @param pos Numeric; combined arithmetically in the body.
#' @return A list with \code{order}, \code{pos}.
#' @export
.mor_ed_shuffle <- function(order, clusters, us, pos) {
  for (ci in seq_len(nrow(clusters))) {
    a <- clusters[ci, 1L]
    b <- clusters[ci, 2L]
    L <- b - a
    if (L > 1L) for (i in seq.int(L - 1L, 1L)) {
      j <- as.integer(us[pos] * (i + 1))
      if (j > i) j <- i
      pos <- pos + 1L
      t1 <- order[a + i + 1L]
      order[a + i + 1L] <- order[a + j + 1L]
      order[a + j + 1L] <- t1
    }
  }
  list(order = order, pos = pos)
}

#' E-divisive multiple change-point analysis
#'
#' Repeatedly splits the series at the point maximising the energy
#' statistic of Matteson and James (2014), eqs. (5)-(7), stopping when
#' a within-segment permutation test no longer rejects at level
#' \code{sig}.  Detects changes in the full distribution rather than
#' only the mean, and needs no parametric model.
#'
#' @param x Numeric vector, or a matrix with one observation per row.
#' @param sig Significance level of the permutation stopping rule.
#' @param R Number of permutations per test.
#' @param alpha Energy exponent, in \code{(0, 2)}.
#' @param min_size Minimum segment length.
#' @param max_cp Optional cap on the number of change points.
#' @param seed Seed of the counter-based stream shared with the Python
#'   arm, so the permutation test is reproducible across languages.
#' @return A list with \code{changepoints} (in discovery order),
#'   \code{changepoints_sorted}, \code{p_values}, \code{q_stats},
#'   \code{n_changepoints}, \code{estimate}, \code{n}, \code{method}.
#' @references Matteson, D. S. and James, N. A. (2014). A
#'   nonparametric approach for multiple change point analysis of
#'   multivariate data. JASA, 109(505), 334-345.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_e_div(V)
morie_e_div <- function(x, sig = 0.05, R = 199L, alpha = 1, min_size = 2L,
                        max_cp = NULL, seed = 20260809) {
  z <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  storage.mode(z) <- "double"
  n <- nrow(z)
  min_size <- as.integer(min_size)
  if (n < 2L * min_size) stop("series too short")
  if (!(alpha > 0 && alpha < 2)) stop("alpha must be in (0, 2)")
  R <- as.integer(R)
  cps <- integer(0)
  pvals <- numeric(0)
  qstats <- numeric(0)
  clusters_of <- function(taus) {
    s <- sort(taus)
    cbind(c(0L, s), c(s, n))
  }
  P <- .mor_ed_prefix(.mor_ed_dist(z, alpha))
  repeat {
    if (!is.null(max_cp) && length(cps) >= max_cp) break
    clusters <- clusters_of(cps)
    bestq <- -Inf
    tau_hat <- -1L
    for (ci in seq_len(nrow(clusters))) {
      a <- clusters[ci, 1L]
      b <- clusters[ci, 2L]
      if (b - a >= 2L * min_size) {
        r_ <- .mor_ed_best_split(P, a, b, min_size)
        if (r_[1] > bestq) { bestq <- r_[1]
        tau_hat <- as.integer(r_[2]) }
      }
    }
    if (tau_hat < 0L) break
    needed <- sum(vapply(seq_len(nrow(clusters)), function(ci)
      max(clusters[ci, 2L] - clusters[ci, 1L] - 1L, 0L), numeric(1)))
    count_ge <- 0L
    for (r in seq_len(R)) {
      us <- .morie_random_uniform(needed, seed = seed, stream = r)
      sh <- .mor_ed_shuffle(seq_len(n) - 1L, clusters, us, 1L)
      zp <- z[sh$order + 1L, , drop = FALSE]
      Pp <- .mor_ed_prefix(.mor_ed_dist(zp, alpha))
      bq <- -Inf
      for (ci in seq_len(nrow(clusters))) {
        a <- clusters[ci, 1L]
        b <- clusters[ci, 2L]
        if (b - a >= 2L * min_size) {
          q <- .mor_ed_best_split(Pp, a, b, min_size)[1]
          if (q > bq) bq <- q
        }
      }
      if (bq >= bestq) count_ge <- count_ge + 1L
    }
    p <- count_ge / (R + 1)
    pvals <- c(pvals, p)
    qstats <- c(qstats, bestq)
    if (p > sig) break
    cps <- c(cps, tau_hat)
  }
  list(changepoints = as.numeric(cps),
       changepoints_sorted = as.numeric(sort(cps)),
       p_values = pvals, q_stats = qstats,
       n_changepoints = length(cps), estimate = as.numeric(sort(cps)),
       n = n, method = "E-divisive (Matteson-James 2014)")
}
