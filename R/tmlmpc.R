# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for a multi-state cause-specific cumulative hazard contrast
#'
#' In a multi-state model each transition has its own hazard and the
#' competing transitions remove people from the risk set, so a marginal
#' contrast must be built transition by transition.  The target is the
#' counterfactual cumulative hazard of the FIRST transition label,
#' \code{Lambda_k^a(t0) = E_W\[sum_{j <= t0} h_k(j | a, W)\]}, and the
#' estimate is \code{Lambda_k^1(t0) - Lambda_k^0(t0)}.
#'
#' Writing the parameter as a mean over W of a sum of conditional hazards
#' gives its influence function directly: the score for \code{h_k(j)} is
#' \code{R(j)(N_k(j) - h_k(j))}, so the clever covariate is
#' \code{H_k(j) = I(A = a)/g_a(W)/P(R(j) = 1 | a, W)} with the at-risk
#' probability the product of surviving every transition and not being
#' censored, both from pooled logistic hazards.
#'
#' @param time Time of the observed transition or of censoring.
#' @param state Label of the state entered at \code{time}; 0 = censored.
#' @param D Binary treatment.
#' @param X Baseline covariates.
#' @return List with \code{estimate}, \code{se}, \code{lam1},
#'   \code{lam0}, \code{eps1}, \code{eps0}, \code{t0}, \code{n}.
#' @references Rytgaard, H. C., Gerds, T. A. & van der Laan, M. J.
#'   (2022). Annals of Statistics 50(5).
#' @export
#' @examples
#' Tmlmpc(time = c(1, 2, 3, 4, 5, 6, 7, 8), state = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1,
#' 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlmpc <- function(time, state, D, X) {
  tv <- as.numeric(time)
  sv <- as.numeric(state)
  Dv <- as.numeric(D)
  n <- length(tv)
  if (n == 0L || length(sv) != n || length(Dv) != n)
    stop("Tmlmpc: time, state and D must share one length")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlmpc: X must have one row per subject")
  causes <- sort(unique(sv[sv > 0.5]))
  if (length(causes) == 0L) stop("Tmlmpc: no observed transitions")
  grid <- sort(unique(tv[sv > 0.5]))
  K <- length(grid)
  t0 <- grid[K]
  target <- causes[1L]
  W <- cbind(1, Xm)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  ii <- integer(0)
  kk <- integer(0)
  for (i in seq_len(n)) for (k in seq_len(K)) if (grid[k] <= tv[i]) {
    ii <- c(ii, i)
    kk <- c(kk, k)
  }
  rows <- cbind(1, grid[kk], Dv[ii], Xm[ii, , drop = FALSE])
  hb <- list()
  for (ci in seq_along(causes)) {
    c_ <- causes[ci]
    hb[[ci]] <- .s4_glmbin(rows, ifelse(abs(sv[ii] - c_) < 1e-9 & grid[kk] == tv[ii], 1, 0))
  }
  cb <- .s4_glmbin(rows, ifelse(sv[ii] < 0.5 & grid[kk] == tv[ii], 1, 0))
  hz <- function(b, a) {
    M <- matrix(0, n, K)
    for (i in seq_len(n)) for (k in seq_len(K))
      M[i, k] <- .s4_clip(.s4_expit(sum(c(1, grid[k], a, Xm[i, ]) * b)), 1e-8, 1 - 1e-8)
    M
  }
  H1 <- lapply(seq_along(causes), function(ci) hz(hb[[ci]], 1))
  H0 <- lapply(seq_along(causes), function(ci) hz(hb[[ci]], 0))
  Cz1 <- hz(cb, 1)
  Cz0 <- hz(cb, 0)
  atrisk <- function(HL, Cz) {
    R <- matrix(0, n, K)
    for (i in seq_len(n)) {
      p <- 1
      for (k in seq_len(K)) {
        R[i, k] <- p
        tot <- 0
        for (ci in seq_along(causes)) tot <- tot + HL[[ci]][i, k]
        p <- p * (1 - .s4_clip(tot, 1e-8, 1 - 1e-8)) * (1 - Cz[i, k])
      }
    }
    R
  }
  R1 <- atrisk(H1, Cz1)
  R0 <- atrisk(H0, Cz0)
  ti <- which(causes == target)
  hobs <- matrix(0, n, K)
  for (i in seq_len(n)) for (k in seq_len(K))
    hobs[i, k] <- if (Dv[i] > 0.5) H1[[ti]][i, k] else H0[[ti]][i, k]
  ybin <- ifelse(abs(sv[ii] - target) < 1e-9 & grid[kk] == tv[ii], 1, 0)
  arm <- function(a, Rp, HA) {
    ga <- if (a > 0.5) g else 1 - g
    hit <- ifelse(abs(Dv - a) < 0.5, 1, 0)
    Hf <- matrix(0, n, K)
    for (i in seq_len(n)) for (k in seq_len(K))
      Hf[i, k] <- hit[i] / ga[i] / max(Rp[i, k], 1e-8)
    hv <- Hf[cbind(ii, kk)]
    ho <- hobs[cbind(ii, kk)]
    eps <- 0
    for (it in seq_len(30L)) {
      p <- .s4_clip(.s4_expit(.s4_logit(ho) + eps * hv), 1e-12, 1 - 1e-12)
      score <- sum(hv * (ybin - p))
      info <- sum(hv * hv * p * (1 - p))
      if (info < 1e-14) break
      step <- score / info
      eps <- eps + step
      if (abs(step) < 1e-13) break
    }
    lam <- numeric(n)
    for (i in seq_len(n)) for (k in seq_len(K))
      lam[i] <- lam[i] + .s4_clip(.s4_expit(.s4_logit(HA[[ti]][i, k]) + eps * Hf[i, k]),
                                  1e-12, 1 - 1e-12)
    psi <- sum(lam) / n
    p <- .s4_clip(.s4_expit(.s4_logit(ho) + eps * hv), 1e-12, 1 - 1e-12)
    term <- hv * (ybin - p)
    ic <- numeric(n)
    for (r in seq_along(ii)) ic[ii[r]] <- ic[ii[r]] + term[r]
    list(psi = psi, eps = eps, ic = ic + lam - psi)
  }
  a1 <- arm(1, R1, H1)
  a0 <- arm(0, R0, H0)
  est <- a1$psi - a0$psi
  ic <- a1$ic - a0$ic
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = est, se = se, lam1 = a1$psi, lam0 = a0$psi,
             eps1 = a1$eps, eps0 = a0$eps, t0 = t0, n = n,
             method = "TMLE for a cause-specific cumulative hazard contrast")
}
