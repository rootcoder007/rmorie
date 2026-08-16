# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cheng-Wei-Ying (1995) two-stage estimator for linear transformation
# models with censored data. Bit-identical mirror of
# src/morie/fn/twostg.py. The generated stub documented a hazard
# factorisation lambda_0(t) f(beta'X) g(gamma'Z) which does not appear
# in the cited paper; the real method implemented here is eq. (2.3).
# Printed anchor: Freireich data, PH error -> beta -1.74, se 0.41.

#' .morie_km_censoring
#'
#' A step of the twostg_native implementation. Called by \code{Twostg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param delta A vector; indexed elementwise.
#' @return The value of \code{function}.
#' @export
.morie_km_censoring <- function(x, delta) {
  n <- length(x)
  ord <- order(x, delta)
  xs <- x[ord]; ds <- delta[ord]
  G <- 1; at_risk <- n
  times <- numeric(0); gvals <- numeric(0)
  i <- 1L
  while (i <= n) {
    t0 <- xs[i]; j <- i; d_cens <- 0L
    while (j <= n && xs[j] == t0) {
      if (ds[j] == 0) d_cens <- d_cens + 1L
      j <- j + 1L
    }
    if (d_cens > 0L) G <- G * (1 - d_cens / at_risk)
    times <- c(times, t0); gvals <- c(gvals, G)
    at_risk <- at_risk - (j - i)
    i <- j
  }
  function(t, before = FALSE) {
    g <- 1
    for (k in seq_along(times)) {
      if (times[k] < t || (times[k] == t && !before)) g <- gvals[k]
      else break
    }
    g
  }
}

#' .morie_xi_ph
#'
#' A step of the twostg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s See Usage.
#' @return A numeric value.
#' @export
.morie_xi_ph <- function(s) 1 / (1 + exp(pmax(pmin(s, 30), -30)))
#' .morie_dxi_ph
#'
#' A step of the twostg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s See Usage.
#' @return A numeric value.
#' @export
.morie_dxi_ph <- function(s) {
  e <- exp(pmax(pmin(s, 30), -30)); -e / (1 + e)^2
}
#' .morie_xi_po
#'
#' A step of the twostg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_xi_po <- function(s) {
  a <- -40; b <- 40; m <- 4000L; h <- (b - a) / m
  k <- 0:m; t <- a + k * h
  Ft <- 1 / (1 + exp(-t)); ft <- Ft * (1 - Ft)
  Fts <- 1 / (1 + exp(-(t + s)))
  w <- ifelse(k == 0 | k == m, 1, ifelse(k %% 2 == 1, 4, 2))
  sum(w * (1 - Fts) * ft) * h / 3
}
#' .morie_dxi_po
#'
#' A step of the twostg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_dxi_po <- function(s) {
  a <- -40; b <- 40; m <- 4000L; h <- (b - a) / m
  k <- 0:m; t <- a + k * h
  Ft <- 1 / (1 + exp(-t)); ft <- Ft * (1 - Ft)
  Fts <- 1 / (1 + exp(-(t + s))); fts <- Fts * (1 - Fts)
  w <- ifelse(k == 0 | k == m, 1, ifelse(k %% 2 == 1, 4, 2))
  sum(w * (-fts) * ft) * h / 3
}

#' Cheng-Wei-Ying two-stage transformation-model estimator
#'
#' Linear transformation model g{S(t | Z)} = h(t) + Z'beta with censored
#' data: stage 1 estimates the censoring survival G by Kaplan-Meier
#' (left limit, validated against the paper's printed Freireich anchor),
#' stage 2 solves estimating equation (2.3) with unit weights. Sandwich
#' variance of Section 2 / Appendix 1.
#'
#' @param time Observed times.
#' @param event Event indicator (1 = event, 0 = censored).
#' @param X Covariate matrix.
#' @param Z Optional additional covariates, concatenated to X.
#' @param error "ph" (extreme-value, proportional hazards) or "po"
#'   (logistic, proportional odds).
#' @param max_iter,tol Newton controls.
#' @return List with \code{estimate}, \code{se}, \code{cov},
#'   \code{n_iter}, \code{error}, \code{method}.
#' @references Cheng, S. C., Wei, L. J. and Ying, Z. (1995), Biometrika
#'   82(4), 835-845.
#' @export
Twostg <- function(time, event, X, Z = NULL, error = "ph",
                   max_iter = 50L, tol = 1e-10) {
  t <- as.numeric(time); d <- as.numeric(event)
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(t)) Xm <- t(Xm)
  if (!is.null(Z)) {
    Zm <- as.matrix(Z); storage.mode(Zm) <- "double"
    if (nrow(Zm) != length(t)) Zm <- t(Zm)
    Xm <- cbind(Xm, Zm)
  }
  n <- nrow(Xm); p <- ncol(Xm)
  if (error == "ph") { xi <- .morie_xi_ph; dxi <- .morie_dxi_ph }
  else if (error == "po") { xi <- .morie_xi_po; dxi <- .morie_dxi_po }
  else stop("error must be 'ph' or 'po'", call. = FALSE)
  geval <- .morie_km_censoring(t, d)
  G2 <- vapply(seq_len(n), function(j) geval(t[j], before = TRUE)^2, 0)
  beta <- rep(0, p); it <- 0L
  for (it in seq_len(max_iter)) {
    U <- rep(0, p); J <- matrix(0, p, p)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) next
        zij <- Xm[i, ] - Xm[j, ]
        s <- sum(zij * beta)
        e_obs <- if (d[j] == 1 && t[i] >= t[j]) 1 / G2[j] else 0
        U <- U + zij * (e_obs - xi(s))
        J <- J + tcrossprod(zij) * (-dxi(s))
      }
    }
    step <- solve(J, U)
    beta <- beta - step
    if (max(abs(step)) < tol) break
  }
  ehat <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      s <- sum((Xm[i, ] - Xm[j, ]) * beta)
      base <- -xi(s)
      if (d[j] == 1 && t[i] >= t[j]) base <- base + 1 / G2[j]
      ehat[i, j] <- base
    }
  }
  lam_inv <- matrix(0, p, p)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      zij <- Xm[i, ] - Xm[j, ]
      lam_inv <- lam_inv + tcrossprod(zij) * (-dxi(sum(zij * beta)))
    }
  }
  lam <- solve(lam_inv / (n * n))
  gam <- matrix(0, p, p)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j == i) next
      zij <- Xm[i, ] - Xm[j, ]
      aij <- ehat[i, j] - ehat[j, i]
      for (k in seq_len(n)) {
        if (k == j || k == i) next
        zik <- Xm[i, ] - Xm[k, ]
        aik <- ehat[i, k] - ehat[k, i]
        gam <- gam + aij * aik * (zij %o% zik)
      }
    }
  }
  gam <- gam / n^3
  corr <- matrix(0, p, p)
  for (l in seq_len(n)) {
    if (d[l] == 1) next
    atrisk <- sum(t >= t[l])
    v <- rep(0, p)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j || d[j] != 1) next
        if (t[i] >= t[j] && t[j] >= t[l]) {
          v <- v + (Xm[i, ] - Xm[j, ]) * (1 / G2[j])
        }
      }
    }
    corr <- corr + (v %o% v) / atrisk^2
  }
  gam <- gam - 4 * corr / n^3
  cov <- (lam %*% gam %*% lam) / n
  list(estimate = beta, se = sqrt(abs(diag(cov))), cov = cov,
       n_iter = it, error = error,
       method = "Cheng-Wei-Ying (1995) two-stage transformation-model estimator, eq. (2.3)")
}
