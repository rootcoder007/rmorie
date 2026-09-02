# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for the marginal hazard ratio when proportional hazards fails
#'
#' Under non-proportional hazards there is no single Cox coefficient to
#' report, so the target is built from the two marginal survival curves:
#' the counterfactual survivals at the last observed event time are
#' targeted separately and the contrast is the ratio of the marginal
#' cumulative hazards, \code{Lambda_a(t0) = -log S_a(t0)}.
#'
#' The nuisance step is a pooled logistic hazard on the person-period
#' expansion (one row per subject per at-risk grid point, design
#' \code{\[1, t, A, W\]}).  The targeting step fluctuates the hazard on the
#' logit scale along
#' \code{H_a(t) = -I(A = a)/g_a(W) * S_a(t0|W)/S_a(t-|W)}, solving the
#' pooled score for a single \code{eps} by fixed-iteration Newton, and
#' \code{psi_a = mean_i S*_a(t0|W_i)}.
#'
#' @param time Observed follow-up time.
#' @param event 1 if the event was observed, 0 if right censored.
#' @param D Binary treatment.
#' @param X Baseline covariates.
#' @return List with \code{estimate}, \code{se}, \code{s1}, \code{s0},
#'   \code{eps}, \code{t0}, \code{n}.
#' @references Moore, K. L. & van der Laan, M. J. (2009). Statistics in
#'   Medicine 28(1):39-64; van der Laan, M. J. & Rubin, D. (2006). IJB
#'   2(1):11.
#' @export
#' @examples
#' Tmlhrz(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), D = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlhrz <- function(time, event, D, X) {
  tv <- as.numeric(time)
  ev <- as.numeric(event)
  Dv <- as.numeric(D)
  n <- length(tv)
  if (n == 0L || length(ev) != n || length(Dv) != n)
    stop("Tmlhrz: time, event and D must share one length")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlhrz: X must have one row per subject")
  W <- cbind(1, Xm)
  grid <- sort(unique(tv[ev > 0.5]))
  if (length(grid) == 0L) stop("Tmlhrz: no observed events")
  K <- length(grid)
  t0 <- grid[K]
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)

  ii <- integer(0)
  kk <- integer(0)
  ybin <- numeric(0)
  for (i in seq_len(n)) for (k in seq_len(K)) if (grid[k] <= tv[i]) {
    ii <- c(ii, i)
    kk <- c(kk, k)
    ybin <- c(ybin, if (ev[i] > 0.5 && grid[k] == tv[i]) 1 else 0)
  }
  rows <- cbind(1, grid[kk], Dv[ii], Xm[ii, , drop = FALSE])
  hb <- .s4_glmbin(rows, ybin)

  haz <- function(i, k, a) {
    z <- sum(c(1, grid[k], a, Xm[i, ]) * hb)
    .s4_clip(.s4_expit(z), 1e-6, 1 - 1e-6)
  }
  h0 <- matrix(0, n, K)
  h1 <- matrix(0, n, K)
  for (i in seq_len(n)) for (k in seq_len(K)) {
    h0[i, k] <- haz(i, k, 0)
    h1[i, k] <- haz(i, k, 1)
  }
  curves <- function(sh0, sh1) {
    s0 <- matrix(0, n, K)
    s1 <- matrix(0, n, K)
    for (i in seq_len(n)) {
      p0 <- 1
      p1 <- 1
      for (k in seq_len(K)) {
        a0 <- h0[i, k]
        a1 <- h1[i, k]
        if (!is.null(sh0)) {
          a0 <- .s4_clip(.s4_expit(.s4_logit(a0) + sh0[i, k]), 1e-12, 1 - 1e-12)
          a1 <- .s4_clip(.s4_expit(.s4_logit(a1) + sh1[i, k]), 1e-12, 1 - 1e-12)
        }
        p0 <- p0 * (1 - a0)
        p1 <- p1 * (1 - a1)
        s0[i, k] <- p0
        s1[i, k] <- p1
      }
    }
    list(s0 = s0, s1 = s1)
  }
  S0 <- curves(NULL, NULL)
  H0 <- matrix(0, n, K)
  H1 <- matrix(0, n, K)
  for (i in seq_len(n)) for (k in seq_len(K)) {
    pv0 <- if (k > 1L) S0$s0[i, k - 1L] else 1
    pv1 <- if (k > 1L) S0$s1[i, k - 1L] else 1
    hit0 <- if (abs(Dv[i] - 0) < 0.5) 1 else 0
    hit1 <- if (abs(Dv[i] - 1) < 0.5) 1 else 0
    H0[i, k] <- -hit0 / (1 - g[i]) * S0$s0[i, K] / pv0
    H1[i, k] <- -hit1 / g[i] * S0$s1[i, K] / pv1
  }
  Hobs <- ifelse(Dv[ii] > 0.5, H1[cbind(ii, kk)], H0[cbind(ii, kk)])
  hobs <- ifelse(Dv[ii] > 0.5, h1[cbind(ii, kk)], h0[cbind(ii, kk)])
  eps <- 0
  for (it in seq_len(30L)) {
    p <- .s4_clip(.s4_expit(.s4_logit(hobs) + eps * Hobs), 1e-12, 1 - 1e-12)
    score <- sum(Hobs * (ybin - p))
    info <- sum(Hobs * Hobs * p * (1 - p))
    if (info < 1e-14) break
    step <- score / info
    eps <- eps + step
    if (abs(step) < 1e-13) break
  }
  Sst <- curves(eps * H0, eps * H1)
  psi0 <- sum(Sst$s0[, K]) / n
  psi1 <- sum(Sst$s1[, K]) / n
  if (psi0 <= 0 || psi0 >= 1 || psi1 <= 0 || psi1 >= 1)
    stop("Tmlhrz: targeted survival left (0, 1); grid too coarse")
  L0 <- -log(psi0)
  L1 <- -log(psi1)
  est <- L1 / L0
  p <- .s4_clip(.s4_expit(.s4_logit(hobs) + eps * Hobs), 1e-12, 1 - 1e-12)
  term <- Hobs * (ybin - p)
  ic0 <- numeric(n)
  ic1 <- numeric(n)
  for (r in seq_along(ii)) {
    if (Dv[ii[r]] > 0.5) ic1[ii[r]] <- ic1[ii[r]] + term[r]
    else ic0[ii[r]] <- ic0[ii[r]] + term[r]
  }
  ic0 <- ic0 + Sst$s0[, K] - psi0
  ic1 <- ic1 + Sst$s1[, K] - psi1
  d1 <- -1 / (psi1 * L0)
  d0 <- L1 / (psi0 * L0 * L0)
  ic <- d1 * ic1 + d0 * ic0
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = est, se = se, s1 = psi1, s0 = psi0, eps = eps,
             t0 = t0, n = n,
             method = "TMLE for the marginal hazard ratio under non-proportional hazards")
}
