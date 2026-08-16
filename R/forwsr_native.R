# SPDX-License-Identifier: AGPL-3.0-or-later
# morie.fn -- function file (rootcoder007/morie)
#
# Sources:
#   Atkinson, A. C. and Riani, M. (2000) Robust Diagnostic Regression
#   Analysis, Springer, ISBN 978-0-387-95017-5,
#   doi:10.1007/978-1-4612-1160-0. The forward search: the
#   least-median-of-squares start, the residual ordering that defines
#   each subset, and the monitoring of the deletion residuals.
#   Riani, M., Atkinson, A. C. and Cerioli, A. (2009) "Finding an
#   unknown number of multivariate outliers", Journal of the Royal
#   Statistical Society Series B 71(2), 447-454 -- equation (12), the
#   consistency factor for a scale estimated from a truncated sample,
#   doi:10.1111/j.1467-9868.2008.00692.x.
#
# Native implementation mirroring Python morie.fn.forwsr exactly: the
# same Gauss-Jordan solve with partial pivoting, the same SplitMix64
# draws for the LMS start, the same residual ordering, and the same
# consistency correction. Row indices are 0-based here, as they are in
# the Python arm; R subscripts add one at the point of use.

#' .forwsr_prep
#'
#' Part of the forwsr_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @return A list with \code{M}, \code{y}, \code{n}, \code{p}.
#' @export
.forwsr_prep <- function(X, y) {
  M <- as.matrix(X); storage.mode(M) <- "double"
  yy <- as.numeric(y)
  n <- nrow(M)
  if (n != length(yy))
    stop(sprintf("forwsr: %d rows of X but %d responses", n, length(yy)))
  if (n < 4L) stop("forwsr: need at least four observations")
  p <- ncol(M)
  if (p == 0L) stop("forwsr: the design is ragged or empty")
  if (n <= p)
    stop(sprintf("forwsr: %d observations cannot support %d coefficients",
                 n, p))
  list(M = M, y = yy, n = n, p = p)
}

#' CPython\'s builtin sum() switched to Neumaier compensated summation
#'
#' for floats in 3.12. R\'s sum() accumulates in long double, and a
#' plain loop accumulates in double; neither reproduces it. The forward
#' search orders residuals that differ in their last digits, so the
#' summation algorithm decides which row enters next and the two arms
#' part company about a dozen steps in. Do what CPython does.
#'
#' @param v See Usage.
#' @return A numeric value.
#' @export
.forwsr_dsum <- function(v) {
  # CPython's builtin sum() switched to Neumaier compensated summation
  # for floats in 3.12. R's sum() accumulates in long double, and a
  # plain loop accumulates in double; neither reproduces it. The
  # forward search orders residuals that differ in their last digits,
  # so the summation algorithm decides which row enters next and the
  # two arms part company about a dozen steps in. Do what CPython does.
  s <- 0.0
  cs <- 0.0
  for (i in seq_along(v)) {
    x <- v[i]
    t <- s + x
    if (abs(s) >= abs(x)) cs <- cs + ((s - t) + x)
    else cs <- cs + ((x - t) + s)
    s <- t
  }
  s + cs
}

#' .forwsr_solve
#'
#' Part of the forwsr_native implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @param b See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.forwsr_solve <- function(A, b) {
  p <- length(b)
  Ab <- cbind(A, b)
  for (c in seq_len(p)) {
    piv <- (c:p)[which.max(abs(Ab[c:p, c]))]
    if (abs(Ab[piv, c]) < 1e-12)
      stop("forwsr: the subset is rank deficient; its design has collinear columns")
    if (piv != c) {
      tmp <- Ab[c, ]; Ab[c, ] <- Ab[piv, ]; Ab[piv, ] <- tmp
    }
    for (r in seq_len(p)) {
      if (r == c) next
      f <- Ab[r, c] / Ab[c, c]
      for (k in c:(p + 1L)) Ab[r, k] <- Ab[r, k] - f * Ab[c, k]
    }
  }
  vapply(seq_len(p), function(i) Ab[i, p + 1L] / Ab[i, i], numeric(1))
}

#' R\'s qnorm IS Wichura AS 241 (PPND16); the Python arm implements the
#'
#' same rational approximation with the same coefficients, and the two
#' agree bit for bit. Bisecting on erf/pnorm instead lands on different
#' sides of the root in the last ulp, which is enough to move the
#' consistency factor and, through it, the deletion residual.
#'
#' @param p See Usage.
#' @return The value of \code{qnorm}.
#' @export
.forwsr_norm_ppf <- function(p) {
  # R's qnorm IS Wichura AS 241 (PPND16); the Python arm implements the
  # same rational approximation with the same coefficients, and the two
  # agree bit for bit. Bisecting on erf/pnorm instead lands on
  # different sides of the root in the last ulp, which is enough to
  # move the consistency factor and, through it, the deletion residual.
  if (!(p > 0 && p < 1))
    stop(sprintf("forwsr: a probability must lie in (0, 1), got %s",
                 format(p)))
  qnorm(p)
}

#' Consistency factor for a scale estimated from a truncated sample
#'
#' The subset of size \code{m} holds the \code{m} SMALLEST squared
#' residuals, so \code{s^2} estimates a truncated normal variance and is
#' biased low; on clean data the raw scale climbs sixteenfold across the
#' search as that bias unwinds. Riani, Atkinson and Cerioli's equation
#' (12) gives the inflation factor
#' \code{c_FS(m) = (m/n) / P(chi2_(v+2) < chi2_(v,m/n))}; for regression
#' residuals \code{v = 1}, and with \code{psi = Phi^-1{(n+m)/2n}} the
#' denominator is \code{m/n - 2 psi phi(psi)}, so the reciprocal is
#' exactly \code{1 - (2n/m) psi phi(psi)}. Multiply \code{s^2} by it.
#'
#' @param m Subset size.
#' @param n Sample size.
#' @return The multiplier, 1 when \code{m >= n}.
#' @references Riani, M., Atkinson, A. C. and Cerioli, A. (2009)
#'   Journal of the Royal Statistical Society Series B 71(2), 447-466,
#'   doi:10.1111/j.1467-9868.2008.00692.x, equation (12).
#' @export
morie_forwsr_consistency_factor <- function(m, n) {
  m <- as.integer(m); n <- as.integer(n)
  if (m >= n) return(1.0)
  if (m <= 0L) stop("forwsr: the subset cannot be empty")
  psi <- .forwsr_norm_ppf((n + m) / (2.0 * n))
  phi <- exp(-0.5 * psi * psi) / sqrt(2.0 * pi)
  cc <- 1.0 - (2.0 * n / m) * psi * phi
  if (cc > 0.0) cc else 1.0
}

#' Least squares on a subset
#'
#' @param X Design matrix; supply your own intercept column.
#' @param y Response.
#' @param subset 0-based row indices, all rows when \code{NULL}.
#' @return List with \code{beta}, \code{residuals} (all rows),
#'   \code{s2}, \code{sigma}, \code{subset}, \code{df}.
#' @export
morie_forwsr_ols_fit <- function(X, y, subset = NULL) {
  pr <- .forwsr_prep(X, y)
  M <- pr$M; yy <- pr$y; n <- pr$n; p <- pr$p
  idx <- if (is.null(subset)) seq_len(n) - 1L else as.integer(subset)
  if (length(idx) < p)
    stop(sprintf("forwsr: a subset of %d cannot fit %d coefficients",
                 length(idx), p))
  ridx <- idx + 1L
  # Accumulate in the Python arm's order rather than through BLAS.
  # The step-p fit interpolates exactly, so the residuals it leaves are
  # at the 1e-16 level and their ORDER decides which row joins next;
  # a different summation order there picks a different subset.
  A <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p))
    A[a, b] <- .forwsr_dsum(M[ridx, a] * M[ridx, b])
  v <- vapply(seq_len(p), function(a) .forwsr_dsum(M[ridx, a] * yy[ridx]),
              numeric(1))
  beta <- .forwsr_solve(A, v)
  resid <- vapply(seq_len(n), function(i)
    yy[i] - .forwsr_dsum(M[i, ] * beta), numeric(1))
  df <- length(idx) - p
  s2 <- if (df > 0L) .forwsr_dsum(resid[ridx]^2) / df else 0.0
  list(beta = beta, residuals = resid, s2 = s2, sigma = sqrt(s2),
       subset = idx, df = df)
}

#' Least median of squares start
#'
#' The p-subset whose fit has the smallest median squared residual: a
#' starting point unlikely to contain an outlier.
#'
#' @param X Design matrix.
#' @param y Response.
#' @param n_draw Random p-subsets to try.
#' @param seed SplitMix64 seed, matching the Python arm.
#' @return List with \code{subset} (0-based, sorted) and
#'   \code{median_sq_residual}.
#' @export
morie_forwsr_lms_start <- function(X, y, n_draw = 500L, seed = 1L) {
  pr <- .forwsr_prep(X, y)
  n <- pr$n; p <- pr$p
  e <- .ghc_rng(as.numeric(seed))
  best <- NULL; best_med <- Inf
  for (d in seq_len(as.integer(n_draw))) {
    idx <- integer(0)
    for (k in seq_len(p)) {
      j <- as.integer(.ghc_unif(e, 1L) * n) %% n
      while (j %in% idx) j <- as.integer(.ghc_unif(e, 1L) * n) %% n
      idx <- c(idx, j)
    }
    f <- tryCatch(morie_forwsr_ols_fit(X, y, idx), error = function(e) NULL)
    if (is.null(f)) next
    sq <- sort(f$residuals^2)
    med <- sq[length(sq) %/% 2L + 1L]
    if (med < best_med) { best_med <- med; best <- sort(idx) }
  }
  if (is.null(best))
    stop("forwsr: every sampled subset was rank deficient; is the design collinear?")
  list(subset = best, median_sq_residual = best_med)
}

#' Run the forward search, monitoring as it goes
#'
#' @param X Design matrix.
#' @param y Response.
#' @param start Starting subset (0-based); the LMS subset when NULL.
#' @param n_draw Draws for the LMS start.
#' @param seed SplitMix64 seed.
#' @return List of steps, each with \code{m}, \code{beta}, \code{sigma},
#'   \code{s2}, \code{consistency_factor}, \code{sigma_corrected},
#'   \code{min_deletion_residual} and \code{subset}.
#' @export
morie_forwsr_forward_search <- function(X, y, start = NULL,
                                        n_draw = 500L, seed = 1L) {
  pr <- .forwsr_prep(X, y)
  n <- pr$n; p <- pr$p
  cur <- if (is.null(start)) {
    morie_forwsr_lms_start(X, y, n_draw, seed)$subset
  } else {
    s0 <- sort(as.integer(start))
    if (length(s0) < p)
      stop(sprintf("forwsr: the starting subset must hold at least %d observations", p))
    s0
  }
  steps <- list()
  repeat {
    f <- morie_forwsr_ols_fit(X, y, cur)
    outside <- setdiff(seq_len(n) - 1L, cur)
    cfac <- morie_forwsr_consistency_factor(length(cur), n)
    if (length(outside) > 0L && f$sigma > 0) {
      sig <- f$sigma / sqrt(cfac)
      mdr <- if (sig > 0) min(abs(f$residuals[outside + 1L])) / sig else NA_real_
    } else {
      mdr <- NA_real_
    }
    steps[[length(steps) + 1L]] <- list(
      m = length(cur), beta = f$beta, sigma = f$sigma, s2 = f$s2,
      consistency_factor = cfac,
      sigma_corrected = f$sigma / sqrt(cfac),
      min_deletion_residual = mdr, subset = cur)
    if (length(cur) >= n) break
    ord <- order(abs(f$residuals)) - 1L
    cur <- sort(ord[seq_len(length(cur) + 1L)])
  }
  steps
}

#' The monitored series along the search
#'
#' @param steps Output of \code{morie_forwsr_forward_search}.
#' @param key Which monitored quantity to return.
#' @return List with \code{m} and the requested series.
#' @export
morie_forwsr_forward_plot <- function(steps,
                                      key = "min_deletion_residual") {
  if (length(steps) == 0L) stop("forwsr: no steps to monitor")
  avail <- setdiff(names(steps[[1L]]), "subset")
  if (!(key %in% names(steps[[1L]])))
    stop(sprintf("forwsr: '%s' is not monitored; available: %s",
                 key, paste(sort(avail), collapse = ", ")))
  out <- list(m = vapply(steps, function(s) as.numeric(s$m), numeric(1)))
  out[[key]] <- vapply(steps, function(s) as.numeric(s[[key]]), numeric(1))
  out
}

#' Forward search regression: run the search and report where it jumps
#'
#' \code{threshold} is on the minimum deletion residual; the units
#' flagged are those entering after the first exceedance -- candidates,
#' not verdicts. \code{min_df} holds the rule back until the subset has
#' that many residual degrees of freedom.
#'
#' @param X Design matrix; supply your own intercept column.
#' @param y Response.
#' @param start Starting subset (0-based) or NULL for the LMS start.
#' @param n_draw Draws for the LMS start.
#' @param seed SplitMix64 seed.
#' @param threshold Deletion-residual threshold.
#' @param min_df Residual degrees of freedom before the rule applies.
#' @return List with \code{estimate}, \code{coefficients}, \code{steps},
#'   \code{n}, \code{n_flagged}, \code{flagged}, \code{jump_at},
#'   \code{method}.
#' @references Atkinson and Riani (2000); Riani, Atkinson and Cerioli
#'   (2009) doi:10.1111/j.1467-9868.2008.00692.x.
#' @export
morie_forwsr <- function(X, y, start = NULL, n_draw = 500L, seed = 1L,
                         threshold = 3.0, min_df = 5L) {
  pr <- .forwsr_prep(X, y)
  p <- pr$p
  steps <- morie_forwsr_forward_search(X, y, start, n_draw, seed)
  n <- steps[[length(steps)]]$m
  jump <- NULL
  for (s in steps) {
    if (s$m - p < as.integer(min_df)) next
    v <- s$min_deletion_residual
    if (!is.na(v) && v > as.numeric(threshold)) { jump <- s$m; break }
  }
  flagged <- integer(0)
  if (length(steps) > 1L) {
    for (i in seq_len(length(steps) - 1L)) {
      a <- steps[[i]]$subset; b <- steps[[i + 1L]]$subset
      new <- setdiff(b, a)
      if (!is.null(jump) && steps[[i + 1L]]$m > jump && length(new) > 0L)
        flagged <- c(flagged, new[1L])
    }
  }
  entry_order <- list()
  if (length(steps) > 1L) {
    for (i in seq_len(length(steps) - 1L)) {
      new <- setdiff(steps[[i + 1L]]$subset, steps[[i]]$subset)
      entry_order[[length(entry_order) + 1L]] <-
        list(m = steps[[i + 1L]]$m,
             entered = if (length(new) > 0L) new[1L] else NA_integer_)
    }
  }
  full <- morie_forwsr_ols_fit(X, y)
  list(estimate = full$beta, coefficients = full$beta, steps = steps,
       n = n, n_flagged = length(flagged), flagged = flagged,
       jump_at_m = if (is.null(jump)) NA_real_ else as.numeric(jump),
       threshold = as.numeric(threshold), min_df = as.integer(min_df),
       monitored_from_m = p + as.integer(min_df),
       entry_order = entry_order,
       sigma_trajectory = vapply(steps, function(s) s$sigma, numeric(1)),
       mdr_trajectory = vapply(steps, function(s) s$min_deletion_residual,
                               numeric(1)),
       method = paste0("forward search (Atkinson & Riani 2000) from a ",
                       "least-median-of-squares start"))
}
