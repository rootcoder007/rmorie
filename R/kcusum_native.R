# Kernel change-point analysis by the kernel Fisher discriminant ratio.
# Source: Harchaoui, Z., Moulines, E. and Bach, F. (2008), Kernel
# change-point analysis, NIPS 21.  Their KFDR statistic compares the
# two candidate segments in the reproducing-kernel Hilbert space:
#
#   KFDR_k = [k(n-k)/n] (mu2 - mu1)' (Sw + gamma I)^{-1} (mu2 - mu1),
#
# with Sw the pooled within-segment covariance in feature space and
# gamma the regularisation of their Sec. 2.  It is then centred and
# scaled by the first two moments d1 = tr[(Sw+gamma I)^{-1} Sw] and
# d2 = tr[((Sw+gamma I)^{-1} Sw)^2] to give the normalised running
# maximum T_k = (KFDR_k - d1) / sqrt(2 d2) of their Sec. 3, whose
# argmax estimates the change point.
#
# Native implementation mirroring Python morie.fn.kcusum exactly: the
# same Gram matrix, the same median heuristic for the Gaussian
# bandwidth (median of pairwise DISTANCES, not squared distances),
# the same eigenvalue tolerance, and the same strict ">" scan so the
# EARLIEST maximising k wins ties.

#' .mor_kc_gram
#'
#' A step of the kcusum_native implementation. Called by \code{morie_kcusum}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z A matrix; passed to \code{nrow}.
#' @param kernel One of \code{"gaussian"}, \code{"linear"}.
#' @param bandwidth Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A list with \code{K}, \code{bw}.
#' @export
.mor_kc_gram <- function(z, kernel, bandwidth) {
  n <- nrow(z)
  if (kernel == "linear") return(list(K = z %*% t(z), bw = NULL))
  if (kernel != "gaussian") stop("kernel must be 'linear' or 'gaussian'")
  d2 <- as.matrix(dist(z))^2
  if (is.null(bandwidth)) {
    dists <- sqrt(d2[upper.tri(d2)])
    ds <- sort(dists)
    m <- length(ds)
    bandwidth <- if (m %% 2L == 1L) ds[(m %/% 2L) + 1L] else
      0.5 * (ds[m %/% 2L] + ds[(m %/% 2L) + 1L])
    if (bandwidth <= 0) bandwidth <- 1
  }
  K <- exp(-d2 / (2 * bandwidth * bandwidth))
  diag(K) <- 1
  list(K = K, bw = as.numeric(bandwidth))
}

#' Kernel change-point analysis (KFDR scan)
#'
#' Scans every split point and reports the one maximising the
#' normalised kernel Fisher discriminant ratio of Harchaoui, Moulines
#' and Bach (2008).  Because the comparison happens in feature space,
#' the method detects changes in the whole distribution -- variance,
#' shape, dependence -- not only in the mean, which is what separates
#' it from \code{\link{morie_pelt}}.
#'
#' @param x Numeric vector, or a matrix with one observation per row.
#' @param kernel \code{"gaussian"} (default) or \code{"linear"}.  Both
#'   routes of the paper are available; the linear kernel reduces the
#'   statistic to a classical mean-change test.
#' @param threshold Optional decision threshold; when supplied the
#'   result carries \code{detected}.
#' @param gamma Regularisation added to the pooled covariance.
#' @param bandwidth Gaussian bandwidth; \code{NULL} uses the median
#'   pairwise distance.
#' @param kmin,kmax Scan range; \code{kmax} defaults to \code{n - 2}.
#' @return A list with \code{estimate} (the estimated split index),
#'   \code{statistic}, \code{kfdr}, \code{d1}, \code{d2}, \code{T}
#'   (the whole scan), \code{kmin}, \code{kmax}, \code{gamma},
#'   \code{bandwidth}, \code{n}, \code{method}, and when a threshold
#'   is given \code{threshold} and \code{detected}.
#' @references Harchaoui, Z., Moulines, E. and Bach, F. (2008).
#'   Kernel change-point analysis. Advances in Neural Information
#'   Processing Systems, 21.
#' @export
morie_kcusum <- function(x, kernel = "gaussian", threshold = NULL,
                         gamma = 0.1, bandwidth = NULL, kmin = 2L,
                         kmax = NULL) {
  z <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  storage.mode(z) <- "double"
  n <- nrow(z)
  if (n < 4L) stop("need n >= 4")
  if (is.null(kmax)) kmax <- n - 2L
  kmin <- as.integer(kmin); kmax <- as.integer(kmax)
  if (!(kmin > 1L && kmin <= kmax && kmax < n))
    stop("need 1 < kmin <= kmax < n")
  gr <- .mor_kc_gram(z, kernel, bandwidth)
  K <- gr$K
  ei <- eigen(K, symmetric = TRUE)
  lam <- rev(ei$values)                    # ascending, as numpy eigh
  U <- ei$vectors[, rev(seq_len(n)), drop = FALSE]
  lmax <- max(abs(lam))
  tol <- 1e-12 * (if (lmax > 0) lmax else 1)
  keep <- which(lam > tol)
  r <- length(keep)
  # coordinates C = diag(sqrt(lambda)) U' on the retained directions
  C <- matrix(0, r, n)
  for (a in seq_len(r)) C[a, ] <- sqrt(lam[keep[a]]) * U[, keep[a]]
  Ts <- numeric(kmax - kmin + 1L)
  kf <- numeric(length(Ts)); d1v <- numeric(length(Ts))
  d2v <- numeric(length(Ts))
  I_r <- diag(r)
  for (idx in seq_along(Ts)) {
    k <- kmin + idx - 1L
    C1 <- C[, seq_len(k), drop = FALSE]
    C2 <- C[, seq.int(k + 1L, n), drop = FALSE]
    mu1 <- rowMeans(C1); mu2 <- rowMeans(C2)
    delta <- mu2 - mu1
    A1 <- C1 - mu1
    A2 <- C2 - mu2
    S1 <- (A1 %*% t(A1)) / k
    S2 <- (A2 %*% t(A2)) / (n - k)
    Sw <- (k * S1 + (n - k) * S2) / n
    M <- Sw + gamma * I_r
    sol <- solve(M, delta)
    kfdr <- (k * (n - k) / n) * sum(delta * sol)
    d1 <- sum(diag(solve(M, Sw)))
    d2 <- sum(diag(solve(M, solve(M, Sw %*% Sw))))
    Ts[idx] <- (kfdr - d1) / sqrt(2 * d2)
    kf[idx] <- kfdr; d1v[idx] <- d1; d2v[idx] <- d2
  }
  ib <- 1L
  for (i in seq_along(Ts)) if (Ts[i] > Ts[ib]) ib <- i
  out <- list(estimate = kmin + ib - 1L, statistic = Ts[ib],
              kfdr = kf[ib], d1 = d1v[ib], d2 = d2v[ib], T = Ts,
              kmin = kmin, kmax = kmax, gamma = as.numeric(gamma),
              bandwidth = gr$bw, n = n,
              method = paste("Kernel change-point analysis",
                             "(Harchaoui-Moulines-Bach 2008)"))
  if (!is.null(threshold)) {
    out$threshold <- as.numeric(threshold)
    out$detected <- Ts[ib] > as.numeric(threshold)
  }
  out
}
