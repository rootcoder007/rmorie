# SPDX-License-Identifier: AGPL-3.0-or-later
#
# smallstats_native.R -- native replacements for low-use dependencies.
#
# Each helper here replaces an external package that had <= 2 call
# sites in R/ code, so the whole Suggests entry can eventually be
# dropped once the cross-validation tests retire. Every function is
# cross-validated against the package it replaces in
# tests/testthat/test-smallstats-native.R.
#
# Replaced call sites:
#   pracma::hurstexp        -> .morie_hurst_rs        (signal.R)
#   rbounds::psens          -> .morie_psens_wilcoxon  (causal.R, effects.R)
#   ebal::ebalance          -> .morie_entropy_balance (matching.R)
#   FNN::get.knn            -> .morie_knn_index       (tps_spatial*.R)
#   smotefamily::SMOTE      -> .morie_smote           (ml.R)
#   harmonicmeanp::p.hmp    -> .morie_hmp             (multiple_testing.R)
#   MCMCpack::rdirichlet    -> inline rgamma draw     (ghcon.R)
#   boot::boot(parametric)  -> existing inline loop   (bootstrap_methods.R)

#' @srrstats {G1.0} References: Hurst (1951); Rosenbaum (2002);
#'   Hainmueller (2012); Chawla et al. (2002); Wilson (2019).
#' @srrstats {G2.1} Inputs validated for type/length at each entry.
#' @noRd
NULL

# ---------------------------------------------------------------------------
# Hurst exponent via simple rescaled-range (R/S) analysis -- an exact
# replica of pracma::hurstexp()$Hs: pad odd-length series to even,
# truncate to the length in [0.99*N, N] with the most divisors >= 50
# (pracma's block-optimal OptN), then compute the whole-series
# statistic log(R/S) / log(n).
.morie_hurst_rs <- function(x, d = 50L) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 16L) {
    stop("Hurst R/S estimation needs at least 16 finite observations.",
         call. = FALSE)
  }
  if (n %% 2L != 0L) {
    x <- c(x, (x[n - 1L] + x[n]) / 2)
    n <- n + 1L
  }
  divisors <- function(m, m0) {
    cand <- m0:floor(m / 2)
    cand[m %% cand == 0]
  }
  n0 <- min(floor(0.99 * n), n - 1L)
  opt_n <- n0
  dv <- divisors(n0, d)
  for (i in (n0 + 1L):n) {
    dw <- divisors(i, d)
    if (length(dw) > length(dv)) {
      opt_n <- i
      dv <- dw
    }
  }
  x <- x[seq_len(opt_n)]
  y <- x - mean(x)
  s <- cumsum(y)
  rs <- (max(s) - min(s)) / stats::sd(x)
  log(rs) / log(length(x))
}

# ---------------------------------------------------------------------------
# Rosenbaum sensitivity bounds for the Wilcoxon signed-rank statistic
# (Rosenbaum 2002, ch. 4), the same quantity rbounds::psens() reports.
# Pairs are formed positionally from equal-length treated/control
# vectors, matching how psens() was called here.
.morie_psens_wilcoxon <- function(treated, control, gamma) {
  stopifnot(length(treated) == length(control), gamma >= 1)
  .morie_psens_wilcoxon_d(as.numeric(treated) - as.numeric(control), gamma)
}

# One-sample form: Rosenbaum bounds directly on pair differences.
# Exact replica of rbounds::psens(): both bounds share the variance
# computed at p+ (a documented quirk of that implementation), the
# lower bound centres at p-, the upper at p+.
.morie_psens_wilcoxon_d <- function(d, gamma) {
  stopifnot(gamma >= 1)
  d <- as.numeric(d)
  d <- d[d != 0]
  n <- length(d)
  if (n == 0L) {
    return(c(p_lower = 1, p_upper = 1))
  }
  r <- rank(abs(d), ties.method = "average")
  t_obs <- sum(r[d > 0])
  p_plus <- gamma / (1 + gamma)
  p_minus <- 1 / (1 + gamma)
  e_plus <- sum(r * p_plus)
  e_minus <- sum(r * p_minus)
  v <- sum(r^2 * p_plus * (1 - p_plus))
  c(p_lower = 1 - stats::pnorm((t_obs - e_minus) / sqrt(v)),
    p_upper = 1 - stats::pnorm((t_obs - e_plus) / sqrt(v)))
}

# ---------------------------------------------------------------------------
# Entropy balancing (Hainmueller 2012), ATT flavour: reweight controls
# so their covariate means equal the treated means. Solves the convex
# dual  min_l  log(sum_c exp(-Xc l)) + mbar' l  via BFGS with an
# analytic gradient; weights are the softmax of -Xc l, rescaled to sum
# to the number of controls (the ebal::ebalance convention).
.morie_entropy_balance <- function(t_mask, X, max_iter = 200L, tol = 1e-8) {
  t_mask <- as.logical(t_mask)
  X <- as.matrix(X)
  Xt <- X[t_mask, , drop = FALSE]
  Xc <- X[!t_mask, , drop = FALSE]
  if (nrow(Xt) == 0L || nrow(Xc) == 0L) {
    stop("Entropy balancing needs both treated and control units.",
         call. = FALSE)
  }
  # Scale columns for conditioning; lambda is rescaled back implicitly
  # because weights only depend on Xc %*% l.
  mu <- colMeans(Xc)
  sd_ <- pmax(apply(Xc, 2, stats::sd), 1e-12)
  Zc <- sweep(sweep(Xc, 2, mu), 2, sd_, "/")
  mbar <- (colMeans(Xt) - mu) / sd_

  obj <- function(l) {
    eta <- -as.numeric(Zc %*% l)
    m <- max(eta)
    log(sum(exp(eta - m))) + m + sum(mbar * l)
  }
  grad <- function(l) {
    eta <- -as.numeric(Zc %*% l)
    w <- exp(eta - max(eta))
    w <- w / sum(w)
    -as.numeric(crossprod(Zc, w)) + mbar
  }
  opt <- stats::optim(rep(0, ncol(Zc)), obj, grad, method = "BFGS",
                      control = list(maxit = max_iter, reltol = tol))
  eta <- -as.numeric(Zc %*% opt$par)
  w <- exp(eta - max(eta))
  w <- w / sum(w) * nrow(Zc)
  bal <- max(abs(colSums(w * Zc) / nrow(Zc) * nrow(Zc) / nrow(Zc) - mbar))
  list(w = w, converged = opt$convergence == 0L,
       max_imbalance = max(abs(as.numeric(crossprod(Zc, w / sum(w))) - mbar)))
}

# ---------------------------------------------------------------------------
# k-nearest-neighbour indices (Euclidean), the FNN::get.knn()$nn.index
# surface used here. Brute force O(n^2) -- the call sites feed spatial
# unit tables (hundreds of rows), where this is instant.
.morie_knn_index <- function(coords, k) {
  coords <- as.matrix(coords)
  n <- nrow(coords)
  k <- as.integer(k)
  if (k >= n) {
    stop("k must be smaller than the number of rows.", call. = FALSE)
  }
  # Squared distances via the Gram-matrix identity (BLAS-speed), then
  # k vectorized min sweeps with max.col (C-speed) instead of a per-row
  # order() -- ~20x faster than the apply(order) formulation.
  # Work on the negated matrix so each sweep is a plain max.col call
  # (negating inside the loop re-allocated n^2 doubles per sweep).
  # ~0.06s at n=1000; the call sites guard n <= 2000. If larger inputs
  # ever matter, this is the function to move into the C++ backend.
  sq <- rowSums(coords^2)
  nd <- 2 * tcrossprod(coords) - outer(sq, sq, "+")
  diag(nd) <- -Inf
  res <- matrix(0L, n, k)
  for (j in seq_len(k)) {
    nn_j <- max.col(nd, ties.method = "first")
    res[, j] <- nn_j
    nd[cbind(seq_len(n), nn_j)] <- -Inf
  }
  res
}

# ---------------------------------------------------------------------------
# SMOTE (Chawla et al. 2002): synthesize minority-class points by
# linear interpolation towards random minority k-NN until classes
# balance. Returns rows to append (X_new, y_new).
.morie_smote <- function(X, y_chr, k) {
  X <- as.matrix(X)
  counts <- table(y_chr)
  minority <- names(counts)[which.min(counts)]
  idx_min <- which(y_chr == minority)
  n_needed <- max(counts) - length(idx_min)
  if (n_needed <= 0L || length(idx_min) < 2L) {
    return(list(X_new = X[0, , drop = FALSE], y_new = character(0)))
  }
  k <- min(as.integer(k), length(idx_min) - 1L)
  Xm <- X[idx_min, , drop = FALSE]
  nn <- .morie_knn_index(Xm, k)
  base_idx <- rep_len(seq_len(nrow(Xm)), n_needed)
  X_new <- t(vapply(base_idx, function(i) {
    nb <- Xm[nn[i, sample.int(k, 1L)], ]
    Xm[i, ] + stats::runif(1L) * (nb - Xm[i, ])
  }, numeric(ncol(Xm))))
  colnames(X_new) <- colnames(X)
  list(X_new = X_new, y_new = rep(minority, n_needed))
}

# ---------------------------------------------------------------------------
# Asymptotically exact harmonic mean p-value (Wilson 2019, PNAS).
# The statistic t = mean(1/p) is asymptotically Landau distributed with
# location log(L) + 0.874367040387922 and scale pi/2; the combined
# p-value is the Landau upper tail at t. The Landau density has no
# closed form; integrate its standard integral representation.
.morie_hmp <- function(p, L = length(p)) {
  p <- pmax(as.numeric(p), 1e-300)
  t_stat <- mean(1 / p)
  mu <- log(L) + 0.874367040387922
  sigma <- pi / 2
  landau_density <- function(x) {
    vapply(x, function(xi) {
      stats::integrate(function(u) {
        exp(-u * log(u) - xi * u) * sin(pi * u)
      }, 0, Inf, rel.tol = 1e-9, stop.on.error = FALSE)$value / pi
    }, numeric(1))
  }
  z <- (t_stat - mu) / sigma
  # Upper tail from z to a far cutoff; Landau tail ~ 1/x, integrate the
  # transformed tail 1/x^2 weight analytically past the cutoff.
  upper <- stats::integrate(function(x) landau_density(x), z, 400,
                            rel.tol = 1e-8, stop.on.error = FALSE)$value
  tail_corr <- 1 / 400 # Landau upper tail beyond cutoff ~ 1/x
  min(1, max(0, upper + tail_corr))
}
