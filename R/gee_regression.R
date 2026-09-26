# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalized estimating equations with a robust covariance
#'
#' Liang & Zeger (1986): solve sum_i D_i' V_i^-1 (y_i - mu_i) = 0 with V_i =
#' phi A_i^1/2 R_i(alpha) A_i^1/2, alternating a Fisher-scoring step in beta
#' with moment estimates from the Pearson residuals e: phi = sum e^2 / N; for
#' "exchangeable" alpha = sum_i sum_(j<k) e_ij e_ik / (phi sum_i n_i (n_i - 1)
#' / 2); for "ar1" the least-squares fit of e_ij e_ik / phi on alpha^|j - k|
#' over all within-cluster pairs (Prentice 1988), as geepack's geeglm computes
#' them. Rows within a cluster are taken in input order. The covariance is the
#' sandwich B^-1 M B^-1. The fit starts from the independence GLM.
#'
#' @param y Response vector.
#' @param X Covariate matrix (or vector).
#' @param clusters Cluster identifier of each row.
#' @param family "gaussian", "binomial" (logit) or "poisson" (log).
#' @param corr_structure "independence", "exchangeable" or "ar1".
#' @param add_intercept Prepend an intercept column.
#' @param max_iter Maximum scoring iterations.
#' @param tol Convergence tolerance on the scoring step.
#' @return Named list: coefficients, se, p_values, alpha, scale, n_clusters,
#'   iterations, fitted, residuals.
#' @references Liang, K.-Y. & Zeger, S. L. (1986). Biometrika 73, 13-22.
#'   Prentice, R. L. (1988). Biometrics 44, 1033-1048. Halekoh, U., Hojsgaard,
#'   S. & Yan, J. (2006). JSS 15(2).
#' @examples
#' x <- sin(1:20)
#' morie_gee_regression(x + rep(c(0.2, -0.1, 0.3, 0), 5), x, rep(1:5, each = 4))$coefficients
#' @export
morie_gee_regression <- function(y, X, clusters, family = "gaussian",
                                 corr_structure = "exchangeable", add_intercept = TRUE,
                                 max_iter = 50L, tol = 1e-6) {
  if (!family %in% c("gaussian", "binomial", "poisson")) {
    stop("family must be 'gaussian', 'binomial' or 'poisson'", call. = FALSE)
  }
  if (!corr_structure %in% c("independence", "exchangeable", "ar1")) {
    stop("corr_structure must be 'independence', 'exchangeable' or 'ar1'", call. = FALSE)
  }
  y <- as.numeric(y)
  X <- as.matrix(X)
  p_raw <- ncol(X)
  if (add_intercept) X <- cbind(1, X)
  n <- nrow(X)
  k <- ncol(X)
  groups <- split(seq_len(n), factor(clusters, levels = unique(clusters)))
  inv_link <- switch(family, binomial = stats::plogis, poisson = exp, gaussian = identity)
  dmu <- switch(family, binomial = function(m) m * (1 - m), poisson = identity,
                gaussian = function(m) rep(1, length(m)))
  vfun <- if (family == "gaussian") function(m) rep(1, length(m)) else dmu
  beta <- rep(0, k)
  if (family == "poisson" && add_intercept) beta[1L] <- log(max(mean(y), 1e-8))
  for (it0 in 1:100) {
    eta <- drop(X %*% beta)
    mu <- inv_link(eta)
    w <- dmu(mu)^2 / vfun(mu)
    z <- eta + (y - mu) / dmu(mu)
    new <- drop(solve(crossprod(X, w * X), crossprod(X, w * z)))
    done <- max(abs(new - beta)) < 1e-12
    beta <- new
    if (done) break
  }
  moments <- function(beta) {
    mu <- inv_link(drop(X %*% beta))
    e <- (y - mu) / sqrt(vfun(mu))
    phi <- sum(e^2) / n
    alpha <- 0
    if (corr_structure == "exchangeable") {
      s <- 0
      np <- 0
      for (g in groups) {
        m <- length(g)
        if (m > 1) {
          eg <- e[g]
          s <- s + (sum(eg)^2 - sum(eg^2)) / 2
          np <- np + m * (m - 1) / 2
        }
      }
      if (np > 0) alpha <- s / (np * phi)
    } else if (corr_structure == "ar1") {
      zz <- numeric(0)
      dd <- numeric(0)
      for (g in groups) {
        m <- length(g)
        if (m > 1) for (a in 1:(m - 1)) for (b in (a + 1):m) {
          zz <- c(zz, e[g[a]] * e[g[b]] / phi)
          dd <- c(dd, b - a)
        }
      }
      if (length(zz)) {
        obj <- function(a) sum((zz - a^dd)^2)
        lo <- -0.999999
        hi <- 0.999999
        gr <- (sqrt(5) - 1) / 2
        c1 <- hi - gr * (hi - lo)
        d1 <- lo + gr * (hi - lo)
        fc <- obj(c1)
        fd <- obj(d1)
        while (hi - lo > 1e-15) {
          if (fc < fd) {
            hi <- d1
            d1 <- c1
            fd <- fc
            c1 <- hi - gr * (hi - lo)
            fc <- obj(c1)
          } else {
            lo <- c1
            c1 <- d1
            fc <- fd
            d1 <- lo + gr * (hi - lo)
            fd <- obj(d1)
          }
        }
        alpha <- (lo + hi) / 2
      }
    }
    list(phi = phi, alpha = alpha)
  }
  pieces <- function(beta, phi, alpha) {
    mu <- inv_link(drop(X %*% beta))
    bm <- matrix(0, k, k)
    u <- numeric(k)
    mm <- matrix(0, k, k)
    for (g in groups) {
      m <- length(g)
      rr <- if (corr_structure == "exchangeable") {
        matrix(alpha, m, m) + diag(1 - alpha, m)
      } else if (corr_structure == "ar1") {
        alpha^abs(outer(seq_len(m), seq_len(m), "-"))
      } else {
        diag(m)
      }
      sd <- sqrt(vfun(mu[g]))
      v <- phi * outer(sd, sd) * rr
      dm <- dmu(mu[g]) * X[g, , drop = FALSE]
      r <- y[g] - mu[g]
      vd <- solve(v, dm)
      vr <- solve(v, r)
      bm <- bm + crossprod(dm, vd)
      s <- drop(crossprod(dm, vr))
      u <- u + s
      mm <- mm + tcrossprod(s)
    }
    list(B = bm, U = u, M = mm)
  }
  it <- 0L
  for (it in seq_len(max_iter)) {
    mo <- moments(beta)
    pc <- pieces(beta, mo$phi, mo$alpha)
    step <- drop(solve(pc$B, pc$U))
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  mo <- moments(beta)
  pc <- pieces(beta, mo$phi, mo$alpha)
  bi <- solve(pc$B)
  se <- sqrt(pmax(diag(bi %*% pc$M %*% bi), 0))
  nm <- c(if (add_intercept) "(Intercept)", paste0("x", seq_len(p_raw) - 1L))
  mu <- inv_link(drop(X %*% beta))
  list(coefficients = stats::setNames(beta, nm), se = stats::setNames(se, nm),
       p_values = stats::setNames(2 * stats::pnorm(abs(beta / se), lower.tail = FALSE), nm),
       alpha = mo$alpha, scale = mo$phi, n_clusters = length(groups), iterations = it,
       fitted = mu, residuals = y - mu)
}
