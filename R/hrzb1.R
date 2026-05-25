# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: Manski maximum-score objective (negative average concordance
# of the sign of the index with the +-1 response). Extracted from the
# hrzb1() optimiser closures so it can be unit-tested directly.
.hrzb1_score <- function(b, ys, X) {
  -mean(ys * (X %*% b > 0))
}

#' Manski (1975) maximum-score estimator
#'
#' @param x Numeric covariate vector or design matrix.
#' @param y Numeric binary response (0/1).
#' @return Named list with estimate, se, score, n, method.
#' @keywords internal
#' @export
hrzb1 <- function(x, y) {
  y <- as.numeric(y)
  X <- if (is.null(dim(x))) matrix(x, ncol = 1) else as.matrix(x)
  n <- nrow(X)
  p <- ncol(X)
  if (n < max(10, 2 * p)) {
    return(list(
      estimate = rep(NA_real_, p), se = rep(NA_real_, p),
      n = n, method = "maximum-score (insufficient data)"
    ))
  }
  ys <- 2 * y - 1
  score <- function(b) .hrzb1_score(b, ys, X)
  beta0 <- as.numeric(stats::coef(stats::lm.fit(X, ys)))
  nrm <- sqrt(sum(beta0^2))
  if (nrm > 1e-12) beta0 <- beta0 / nrm
  if (beta0[1] < 0) beta0 <- -beta0
  best <- beta0
  best_l <- score(best)
  # Pick an optimiser appropriate to p: Brent (1-D bracketing) for the
  # univariate case, Nelder-Mead simplex for p >= 2. Nelder-Mead on 1-D
  # emits a "use Brent or optimize() directly" warning on every call,
  # which is correct -- it really is the wrong tool for 1-D.
  .hrzb1_optim <- function(start, fn, maxit) {
    if (length(start) == 1L) {
      r <- stats::optim(start, fn, method = "Brent",
                         lower = -1, upper = 1,
                         control = list(maxit = maxit, reltol = 1e-4))
    } else {
      r <- stats::optim(start, fn, method = "Nelder-Mead",
                         control = list(maxit = maxit, reltol = 1e-4))
    }
    r
  }
  set.seed(0)
  for (k in 1:8) {
    s <- stats::rnorm(p)
    s <- s / sqrt(sum(s^2))
    r <- .hrzb1_optim(s, score, maxit = 300)
    b <- r$par / max(sqrt(sum(r$par^2)), 1e-12)
    if (b[1] < 0) b <- -b
    l <- score(b)
    if (l < best_l) {
      best_l <- l
      best <- b
    }
  }
  # Subsample SE (cube-root rescale)
  set.seed(42)
  B <- 30
  m <- max(20L, n %/% 2L)
  boot <- matrix(0, B, p)
  for (b_idx in 1:B) {
    idx <- sample.int(n, m, replace = FALSE)
    Xb <- X[idx, , drop = FALSE]
    yb <- ys[idx]
    sc <- function(b) .hrzb1_score(b, yb, Xb)
    r <- .hrzb1_optim(best + 0.05 * stats::rnorm(p), sc, maxit = 150)
    bb <- r$par / max(sqrt(sum(r$par^2)), 1e-12)
    if (bb[1] < 0) bb <- -bb
    boot[b_idx, ] <- bb
  }
  se <- apply(boot, 2, stats::sd) * (m / n)^(1 / 3)
  list(
    estimate = best, se = se, score = -best_l, n = n,
    method = "Manski (1975) maximum-score (binary response)",
    warnings = list("Cube-root asymptotics: subsample-rescaled SEs.")
  )
}

# canonical full-name alias (Py<->R API parity)
#' @rdname hrzb1
#' @keywords internal
#' @export
morie_horowitz_binary_response <- hrzb1
