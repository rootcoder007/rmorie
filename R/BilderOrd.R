# SPDX-License-Identifier: AGPL-3.0-or-later

#' Proportional odds model by maximum likelihood
#'
#' logit P(Y <= j) = beta_j0 + x'beta fitted by Newton-Raphson with the analytic
#' Hessian and step halving. MASS::polr reports zeta = beta_j0 and -beta.
#'
#' @param y Ordered categories coded 0, ..., J - 1 (all present).
#' @param X Matrix of explanatory variables without intercept.
#' @param max_iter,tol Newton controls.
#' @return list(intercepts, beta, se_intercepts, se_beta, loglik, iterations).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Eqs (3.11), (3.15).
#' @examples
#' PolrFit(c(0, 0, 1, 0, 1, 2, 1, 2, 2, 1), matrix(0:9))$beta
#' @export
PolrFit <- function(y, X, max_iter = 200, tol = 1e-12) {
  X <- as.matrix(X)
  n <- length(y)
  J <- max(y) + 1
  if (J < 2 || !identical(sort(unique(y)), seq_len(J) - 1) || nrow(X) != n) stop("y must use every category 0..J-1 and match X", call. = FALSE)
  k <- J - 1
  p <- ncol(X)
  cum <- vapply(seq_len(k) - 1, function(j) mean(y <= j), numeric(1))
  par <- c(stats::qlogis(cum), rep(0, p))
  cuts <- function(par) {
    eta <- drop(X %*% par[k + seq_len(p)])
    th <- c(-Inf, par[seq_len(k)], Inf)
    list(hi = th[y + 2] + eta, lo = th[y + 1] + eta)
  }
  loglik <- function(par) {
    if (k > 1 && any(diff(par[seq_len(k)]) <= 0)) return(-Inf)
    cc <- cuts(par)
    pr <- plogis(cc$hi) - plogis(cc$lo)
    if (any(pr <= 0)) return(-Inf)
    sum(log(pr))
  }
  derivs <- function(par) {
    cc <- cuts(par)
    Fa <- plogis(cc$hi)
    Fb <- plogis(cc$lo)
    P <- Fa - Fb
    fa <- Fa * (1 - Fa)
    fb <- Fb * (1 - Fb)
    faa <- ifelse(is.finite(cc$hi), fa * (1 - 2 * Fa) / P - fa^2 / P^2, 0)
    fbb <- ifelse(is.finite(cc$lo), -fb * (1 - 2 * Fb) / P - fb^2 / P^2, 0)
    fab <- fa * fb / P^2
    Va <- matrix(0, n, k + p)
    Vb <- matrix(0, n, k + p)
    up <- y < k
    dn <- y > 0
    Va[cbind(which(up), y[up] + 1)] <- 1
    Vb[cbind(which(dn), y[dn])] <- 1
    Va[up, k + seq_len(p)] <- X[up, , drop = FALSE]
    Vb[dn, k + seq_len(p)] <- X[dn, , drop = FALSE]
    g <- colSums((fa / P) * Va - (fb / P) * Vb)
    H <- crossprod(Va, faa * Va) + crossprod(Vb, fbb * Vb) + crossprod(Va, fab * Vb) + crossprod(Vb, fab * Va)
    list(g = g, H = H)
  }
  ll <- loglik(par)
  it <- 0L
  for (it in seq_len(max_iter)) {
    d <- derivs(par)
    step <- solve(-d$H, d$g)
    t <- 1
    repeat {
      cand <- par + t * step
      lc <- loglik(cand)
      if (lc >= ll || t < 1e-12) break
      t <- t / 2
    }
    done <- max(abs(t * step)) < tol * (1 + max(abs(par)))
    par <- cand
    ll <- lc
    if (done) break
  }
  se <- sqrt(diag(solve(-derivs(par)$H)))
  list(intercepts = par[seq_len(k)], beta = par[k + seq_len(p)], se_intercepts = se[seq_len(k)], se_beta = se[k + seq_len(p)],
       loglik = ll, iterations = it)
}

#' Rao-Scott corrections and the Thomas-Rao F test
#'
#' @param statistic Pearson or LRT statistic.
#' @param deltas Generalized design effects.
#' @param kappa Degrees of freedom of the variance estimate (for the F test).
#' @return list(rs1, rs1_df, rs1_p, rs2, rs2_df, rs2_p, f_tr, f_df, f_p, d_bar, c2).
#' @references Bilder, C. R. & Loughin, T. M. (2025). Analysis of Categorical
#'   Data with R, 2nd ed. Sec 6.3, eq (6.12).
#' @examples
#' RaoScott(10, c(2, 2))$rs1
#' @export
RaoScott <- function(statistic, deltas, kappa = NULL) {
  nu <- length(deltas)
  if (nu < 1 || min(deltas) <= 0 || statistic < 0) stop("need positive design effects and a non-negative statistic", call. = FALSE)
  db <- mean(deltas)
  c2 <- sum((deltas - db)^2) / (nu * db^2)
  rs1 <- statistic / db
  rs2 <- statistic / (db * (1 + c2))
  df2 <- nu / (1 + c2)
  ftr <- statistic / (nu * db)
  fdf <- if (!is.null(kappa)) c(df2, kappa * nu / (1 + c2)) else NULL
  list(rs1 = rs1, rs1_df = nu, rs1_p = pchisq(rs1, nu, lower.tail = FALSE), rs2 = rs2, rs2_df = df2,
       rs2_p = pchisq(rs2, df2, lower.tail = FALSE), f_tr = ftr, f_df = fdf,
       f_p = if (!is.null(fdf)) pf(ftr, fdf[1], fdf[2], lower.tail = FALSE) else NULL, d_bar = db, c2 = c2)
}
