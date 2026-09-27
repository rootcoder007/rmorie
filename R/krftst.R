# SPDX-License-Identifier: AGPL-3.0-or-later
.kr_inv <- function(a) {
  ev <- abs(eigen(a, symmetric = TRUE, only.values = TRUE)$values)
  if (min(ev) > 1e-10) return(solve(a))
  s <- svd(a)
  keep <- s$d > max(dim(a)) * max(s$d) * .Machine$double.eps
  s$v[, keep, drop = FALSE] %*% (t(s$u[, keep, drop = FALSE]) / s$d[keep])
}

#' Kenward-Roger adjusted F test for fixed effects
#'
#' Kenward & Roger (1997), as written in Schabenberger & Gotway (2005) eqs
#' (6.53)-(6.54): with Phi = (X' Sigma^-1 X)^-1 at the REML estimate and
#' Sigma_i the derivatives of Sigma, Phi_A = Phi + 2 Phi S Phi with S = sum_ij W_ij (Q_ij -
#' P_i Phi P_j - R_ij / 4), W twice the inverse REML expected
#' information, and F* = lambda (L beta - l0)' (L Phi_A L')^-1 (L beta - l0) /
#' q on q and m degrees of freedom (lambda and m as in pbkrtest). The R_ij
#' second-derivative terms vanish when Sigma is linear in theta, which is the
#' case pbkrtest's KRmodcomp handles; pass `d2Sigma` to include them for
#' spatial covariance functions.
#'
#' @param X Design matrix.
#' @param y Response vector.
#' @param Sigma Covariance matrix at the REML estimate.
#' @param dSigma List of derivative matrices dSigma / dtheta_i.
#' @param L Contrast matrix (q x p).
#' @param l0 Hypothesised value, default zero.
#' @param d2Sigma Optional named list of second derivatives, names "i,j"
#'   with i <= j (1-based).
#' @return Named list: beta, Phi, Phi_adjusted, F, ndf, ddf, scaling,
#'   p_value, F_unadjusted, p_value_unadjusted, W, A1, A2.
#' @references Kenward, M. G. & Roger, J. H. (1997). Biometrics 53,
#'   983-997. Halekoh, U. & Hojsgaard, S. (2014). JSS 59(9).
#'   Schabenberger & Gotway (2005), eqs (6.53)-(6.54), p. 343.
#' @examples
#' g <- rep(1:4, each = 3)
#' x <- seq_len(12)
#' zz <- outer(g, g, "==") * 1
#' krftst(cbind(1, x), x + g, 0.5 * zz + diag(12), list(zz, diag(12)), matrix(c(0, 1), 1))$F
#' @export
krftst <- function(X, y, Sigma, dSigma, L, l0 = NULL, d2Sigma = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  L <- matrix(L, ncol = ncol(X))
  ng <- length(dSigma)
  si <- solve(Sigma)
  tt <- si %*% X
  phi <- solve(crossprod(X, tt))
  beta <- drop(phi %*% crossprod(tt, y))
  h <- lapply(dSigma, function(g) g %*% si)
  o <- lapply(h, function(m) m %*% X)
  p <- lapply(o, function(m) {
    pm <- -crossprod(m, tt)
    (pm + t(pm)) / 2
  })
  q2 <- function(i, j) crossprod(o[[i]], si %*% o[[j]])
  ie2 <- matrix(0, ng, ng)
  for (i in seq_len(ng)) for (j in seq_len(ng)) {
    ie2[i, j] <- sum(diag(h[[i]] %*% h[[j]])) - 2 * sum(phi * q2(i, j)) +
      sum(diag(phi %*% p[[i]] %*% phi %*% p[[j]]))
  }
  w <- 2 * .kr_inv(ie2)
  u <- matrix(0, ncol(X), ncol(X))
  for (i in seq_len(ng)) for (j in seq_len(ng)) {
    term <- q2(i, j) - p[[i]] %*% phi %*% p[[j]]
    key <- paste(min(i, j), max(i, j), sep = ",")
    if (!is.null(d2Sigma) && !is.null(d2Sigma[[key]])) {
      term <- term - 0.25 * crossprod(tt, d2Sigma[[key]] %*% tt)
    }
    u <- u + w[i, j] * term
  }
  u <- (u + t(u)) / 2
  phia <- phi + 2 * phi %*% u %*% phi
  q <- qr(L)$rank
  theta <- t(L) %*% solve(L %*% phi %*% t(L), L)
  a1 <- 0
  a2 <- 0
  for (i in seq_len(ng)) {
    ui <- theta %*% phi %*% p[[i]] %*% phi
    for (j in seq_len(ng)) {
      uj <- theta %*% phi %*% p[[j]] %*% phi
      a1 <- a1 + w[i, j] * sum(diag(ui)) * sum(diag(uj))
      a2 <- a2 + w[i, j] * sum(diag(ui %*% uj))
    }
  }
  b <- (a1 + 6 * a2) / (2 * q)
  g <- ((q + 1) * a1 - (q + 4) * a2) / ((q + 2) * a2)
  den <- 3 * q + 2 * (1 - g)
  c1 <- g / den
  c2 <- (q - g) / den
  c3 <- (q + 2 - g) / den
  v0 <- 1 + c1 * b
  if (abs(v0) < 1e-10) v0 <- 0
  rho <- (1 / q) * ((1 - a2 / q) / (1 - c2 * b))^2 * v0 / (1 - c3 * b)
  m <- 4 + (q + 2) / (q * rho - 1)
  lam <- if (abs(m - 2) < 0.01) 1 else m * (1 - a2 / q) / (m - 2)
  if (is.null(l0)) l0 <- rep(0, q)
  d <- drop(L %*% beta) - l0
  fu <- drop(crossprod(d, solve(L %*% phia %*% t(L), d))) / q
  f <- lam * fu
  list(beta = beta, Phi = phi, Phi_adjusted = phia, F = f, ndf = q, ddf = m, scaling = lam,
       p_value = stats::pf(f, q, m, lower.tail = FALSE), F_unadjusted = fu,
       p_value_unadjusted = stats::pf(fu, q, m, lower.tail = FALSE), W = w, A1 = a1, A2 = a2)
}
