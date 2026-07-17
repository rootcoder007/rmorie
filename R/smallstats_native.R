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
# Hurst exponent via rescaled-range (R/S) analysis. Simple R/S slope
# estimator equivalent to pracma::hurstexp()$Hs: split the series into
# non-overlapping blocks at a ladder of sizes, compute the rescaled
# range per block, and regress log(mean R/S) on log(block size).
.morie_hurst_rs <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 16L) {
    stop("Hurst R/S estimation needs at least 16 finite observations.",
         call. = FALSE)
  }
  # Block-size ladder: halve from n down to 8.
  sizes <- unique(floor(n / 2^(0:floor(log2(n / 8)))))
  sizes <- sizes[sizes >= 8L]
  rs_mean <- vapply(sizes, function(m) {
    k <- floor(n / m)
    rs <- vapply(seq_len(k), function(i) {
      seg <- x[((i - 1L) * m + 1L):(i * m)]
      dev <- cumsum(seg - mean(seg))
      r <- max(dev) - min(dev)
      s <- stats::sd(seg)
      if (s <= 0) return(NA_real_)
      r / s
    }, numeric(1))
    mean(rs, na.rm = TRUE)
  }, numeric(1))
  ok <- is.finite(rs_mean) & rs_mean > 0
  fit <- stats::lm.fit(cbind(1, log(sizes[ok])), log(rs_mean[ok]))
  as.numeric(fit$coefficients[2L])
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

# One-sample form: Rosenbaum bounds directly on pair differences,
# matching rbounds::psens(x) with a single vector argument.
.morie_psens_wilcoxon_d <- function(d, gamma) {
  stopifnot(gamma >= 1)
  d <- as.numeric(d)
  d <- d[d != 0]
  n <- length(d)
  if (n == 0L) {
    return(c(p_lower = 1, p_upper = 1))
  }
  r <- rank(abs(d))
  t_obs <- sum(r[d > 0])
  s1 <- sum(r)
  s2 <- sum(r^2)
  p_hi <- gamma / (1 + gamma)
  p_lo <- 1 / (1 + gamma)
  z <- function(p_edge) {
    mu <- p_edge * s1
    v <- p_edge * (1 - p_edge) * s2
    stats::pnorm((t_obs - mu) / sqrt(v), lower.tail = FALSE)
  }
  # Upper bound uses the least-favourable p (p_lo shifts mass down, so
  # the observed statistic looks larger -> smaller p); psens reports
  # Lower = z(p_hi), Upper = z(p_lo).
  c(p_lower = z(p_hi), p_upper = z(p_lo))
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
  d2 <- as.matrix(stats::dist(coords))
  diag(d2) <- Inf
  t(apply(d2, 1L, function(row) order(row)[seq_len(k)]))
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
