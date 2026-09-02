# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Log-normal frailty by penalized partial likelihood with REML variance
# (Lnfrm). Bit-identical mirror of src/morie/fn/lnfrm.py. Anchored
# against survival::coxph(frailty.gaussian): beta and sigma2 agree to
# 6 significant digits on the 4-cluster test set.

#' Log-normal frailty Cox model (penalized partial likelihood + REML)
#'
#' Hazard lambda_ij(t | b_i) = lambda_0(t) exp(beta' x_ij + b_i) with
#' b_i ~ N(0, sigma^2) per cluster. For fixed sigma^2 the estimates
#' maximise the penalized Breslow partial likelihood
#' l(beta, b) - b'b / (2 sigma^2); sigma^2 is updated by McGilchrist's
#' REML formula (b'b + trace of the frailty block of the inverse
#' penalized information) / q, and the steps alternate.
#'
#' @param time Observed times.
#' @param event Event indicator (1 = event, 0 = censored).
#' @param X Covariate matrix.
#' @param cluster Cluster labels.
#' @param max_outer,tol Outer-loop controls.
#' @return List with \code{estimate}, \code{se}, \code{frailty},
#'   \code{sigma2}, \code{loglik_penalized}, \code{n_outer},
#'   \code{n_newton}, \code{method}.
#' @references McGilchrist, C. A. (1993), Biometrics 49(1), 221-225;
#'   McGilchrist and Aisbett (1991), Biometrics 47(2), 461-466;
#'   Therneau, Grambsch and Pankratz (2003), Journal of Computational
#'   and Graphical Statistics 12(1), 156-175.
#' @export
#' @examples
#' Lnfrm(time = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5), event = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8), cluster = c(1, 2, 3, 4, 5, 6, 7, 8))
Lnfrm <- function(time, event, X, cluster, max_outer = 50L, tol = 1e-7) {
  t <- as.numeric(time)
  e <- as.numeric(event)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(t)) Xm <- t(Xm)
  n <- length(t)
  p <- ncol(Xm)
  cl <- as.vector(cluster)
  ks <- sort(unique(cl))
  q <- length(ks)
  if (q < 2L) stop("need at least 2 clusters", call. = FALSE)
  Dm <- matrix(0, n, q)
  for (i in seq_len(n)) Dm[i, match(cl[i], ks)] <- 1
  Z <- cbind(Xm, Dm)
  etimes <- sort(unique(t[e == 1]))

  newton <- function(sigma2, max_iter = 50L, ntol = 1e-10) {
    theta <- rep(0, p + q)
    pen <- diag(c(rep(0, p), rep(1 / sigma2, q)), p + q)
    info <- NULL
    ll <- 0
    it <- 0L
    for (it in seq_len(max_iter)) {
      eta <- pmax(pmin(as.vector(Z %*% theta), 500), -500)
      w <- exp(eta)
      U <- rep(0, p + q)
      info <- matrix(0, p + q, p + q)
      ll <- 0
      for (tk in etimes) {
        Dk <- which(t == tk & e == 1)
        R <- which(t >= tk)
        S0 <- sum(w[R])
        S1 <- as.vector(crossprod(Z[R, , drop = FALSE], w[R]))
        S2 <- crossprod(Z[R, , drop = FALSE] * w[R], Z[R, , drop = FALSE])
        d <- length(Dk)
        xbar <- S1 / S0
        ll <- ll + sum(eta[Dk]) - d * log(S0)
        U <- U + colSums(Z[Dk, , drop = FALSE]) - d * xbar
        info <- info + d * (S2 / S0 - tcrossprod(xbar))
      }
      b <- theta[(p + 1):(p + q)]
      ll_pen <- ll - sum(b * b) / (2 * sigma2)
      U <- U - pen %*% theta
      info <- info + pen
      step <- solve(info, U)
      theta <- theta + as.vector(step)
      if (max(abs(step)) < ntol) break
    }
    b <- theta[(p + 1):(p + q)]
    list(theta = theta, info = info,
         ll_pen = ll - sum(b * b) / (2 * sigma2), n_newton = it)
  }

  sigma2 <- 0.5
  fit <- NULL
  outer <- 0L
  for (outer in seq_len(max_outer)) {
    fit <- newton(sigma2)
    b <- fit$theta[(p + 1):(p + q)]
    cov <- solve(fit$info)
    tr <- sum(diag(cov)[(p + 1):(p + q)])
    sigma2_new <- (sum(b * b) + tr) / q
    if (abs(sigma2_new - sigma2) < tol * max(1, sigma2)) {
      sigma2 <- sigma2_new
      break
    }
    sigma2 <- sigma2_new
  }
  fit <- newton(sigma2)
  cov <- solve(fit$info)
  dg <- diag(cov)[1:p]
  if (any(!is.finite(dg)) || any(dg <= 0)) {
    stop("penalized information is singular", call. = FALSE)
  }
  b <- fit$theta[(p + 1):(p + q)]
  list(estimate = fit$theta[1:p], se = sqrt(dg),
       frailty = stats::setNames(as.list(b), as.character(ks)),
       sigma2 = sigma2, loglik_penalized = fit$ll_pen,
       n_outer = outer, n_newton = fit$n_newton,
       method = "McGilchrist (1993) log-normal frailty, penalized PL + REML variance")
}
