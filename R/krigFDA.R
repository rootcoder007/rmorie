# R arm of morie/fn/krigFDA.py -- universal kriging in explicit GLS form.
#
# The Python body was a placeholder: it averaged `coords` and used neither
# `values` nor `new_coords`. There was no R arm at all.
#
# Universal kriging is usually solved as one augmented (n+p) system with
# Lagrange multipliers. This module states it the other way, as the
# module's own specification asks:
#
#   beta_hat = (X' Sigma^-1 X)^-1 X' Sigma^-1 Z
#   lambda   = Sigma^-1 c0
#   Z*(s0)   = x0' beta_hat + lambda' (Z - X beta_hat)
#
# GLS trend, then simple kriging of the GLS residuals. The two
# formulations are algebraically identical; this one exposes beta_hat,
# which the augmented system hides. The reported variance is still the
# universal kriging variance from the augmented system -- the simple
# kriging variance of the residual step would ignore the uncertainty in
# beta_hat and understate the error.
#
# Conventions for model/nugget/sill/range_ are ukrig()'s, whose answer
# this must reproduce exactly.
#
# Cressie (1993) secs. 3.4.2 and 3.4.5; Schabenberger & Gotway (2005) ch. 5.

#' @noRd
KrigFDA <- function(coords, values, new_coords, model = "exponential",
                          nugget = 0, sill = 1, range_ = 1, trend_order = 1) {
  z <- as.numeric(values)
  n <- length(z)
  s <- if (is.matrix(coords)) coords else matrix(as.numeric(unlist(coords)), nrow = n)
  tg <- if (is.matrix(new_coords)) new_coords else
    matrix(as.numeric(unlist(new_coords)), ncol = ncol(s))
  if (nrow(s) != n) stop("coords rows must match values", call. = FALSE)
  if (ncol(tg) != ncol(s)) stop("new_coords dim must match coords dim", call. = FALSE)
  c0 <- nugget
  c1 <- sill - nugget
  if (c1 < 0) stop("sill must be >= nugget", call. = FALSE)
  a <- range_

  cov_fn <- function(h) {
    switch(model,
      exponential = c1 * exp(-h / a) + ifelse(h == 0, c0, 0),
      gaussian = c1 * exp(-(h^2) / (a^2)) + ifelse(h == 0, c0, 0),
      spherical = ifelse(h <= a, c1 * (1 - 1.5 * h / a + 0.5 * (h / a)^3), 0) +
        ifelse(h == 0, c0, 0),
      stop("unknown model", call. = FALSE)
    )
  }
  trend_design <- function(C) {
    C <- if (is.matrix(C)) C else matrix(C, ncol = ncol(s))
    ones <- matrix(1, nrow(C), 1)
    if (trend_order == 0) return(ones)
    if (trend_order == 1) return(cbind(ones, C))
    if (trend_order == 2) {
      sq <- C^2
      cross <- if (ncol(C) >= 2) C[, 1] * C[, 2] else NULL
      return(cbind(ones, C, sq, cross))
    }
    stop("trend_order must be 0, 1, or 2", call. = FALSE)
  }
  cross_d <- function(A, B) {
    matrix(vapply(seq_len(nrow(B)),
                  function(j) sqrt(rowSums((A - matrix(B[j, ], nrow(A), ncol(A),
                                                       byrow = TRUE))^2)),
                  numeric(nrow(A))), nrow = nrow(A))
  }

  Sig <- cov_fn(cross_d(s, s))
  X <- trend_design(s)
  p <- ncol(X)
  Si <- solve(Sig)
  XtSi <- t(X) %*% Si
  beta <- as.numeric(solve(XtSi %*% X, XtSi %*% z))
  resid <- as.numeric(z - X %*% beta)

  total_var <- c0 + c1
  K <- matrix(0, n + p, n + p)
  K[seq_len(n), seq_len(n)] <- Sig
  K[seq_len(n), n + seq_len(p)] <- X
  K[n + seq_len(p), seq_len(n)] <- t(X)

  est <- numeric(nrow(tg))
  se <- numeric(nrow(tg))
  wts <- vector("list", nrow(tg))
  for (m in seq_len(nrow(tg))) {
    row <- tg[m, , drop = FALSE]
    cv <- as.numeric(cov_fn(cross_d(row, s)))
    x0 <- as.numeric(trend_design(row))
    lam <- as.numeric(Si %*% cv)
    est[m] <- sum(x0 * beta) + sum(lam * resid)
    rhs <- c(cv, x0)
    sol <- as.numeric(solve(K, rhs))
    se[m] <- sqrt(max(total_var - sum(sol * rhs), 0))
    wts[[m]] <- lam
  }

  list(estimate = est, se = se, beta = beta, residuals = resid, weights = wts,
       n = n, p = p,
       method = "Universal kriging in GLS form, Z* = x0'beta + lambda'(Z - X beta)")
}

#' @noRd
