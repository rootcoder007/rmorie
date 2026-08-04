# SPDX-License-Identifier: AGPL-3.0-or-later
#' Meta-regression on centred moderators
#'
#' Centring each moderator leaves the slopes unchanged and makes the intercept
#' the predicted effect at the average study.  Fitted by weighted least squares
#' with the moment estimate of the residual tau^2.  Source consulted:
#' Borenstein, Hedges, Higgins and Rothstein (2009), chapter 20.
#'
#' @param yi,vi study effects and their within-study variances.
#' @param mods moderator matrix or vector.
#' @param weighted centre at the inverse-variance weighted mean.
#' @return list: estimate, coefficients, se, centers, tau2_resid, tau2_total,
#'   QE, centered, n, method.
#' @keywords internal
#' @examples
#' mac3(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08), 1:4)$centers
#' @export
mac3 <- function(yi, vi, mods, weighted = TRUE) {
  y <- as.numeric(yi); v <- as.numeric(vi); k <- length(y)
  m <- as.matrix(mods)
  if (nrow(m) != k) m <- t(m)
  w0 <- 1 / v
  ctr <- if (weighted) apply(m, 2, function(col) sum(w0 * col) / sum(w0)) else colMeans(m)
  mc <- sweep(m, 2, ctr, "-")
  x <- cbind(1, mc); dimnames(x) <- NULL
  p <- ncol(x)
  xtw <- t(x) * rep(w0, each = p)
  xtwxi <- solve(xtw %*% x)
  beta <- as.numeric(xtwxi %*% (xtw %*% y))
  resid <- as.numeric(y - x %*% beta)
  qe <- sum(w0 * resid * resid)
  trp <- sum(w0) - sum(diag(xtwxi %*% ((t(x) * rep(w0 * w0, each = p)) %*% x)))
  tau2r <- if (trp > 0) max(0, (qe - (k - p)) / trp) else 0
  ws <- 1 / (v + tau2r)
  xtws <- t(x) * rep(ws, each = p)
  vb <- solve(xtws %*% x)
  betar <- as.numeric(vb %*% (xtws %*% y))
  list(estimate = betar[1], coefficients = betar, se = sqrt(diag(vb)),
       centers = as.numeric(ctr), tau2_resid = tau2r,
       tau2_total = k02dl(y, v)$tau2, QE = qe, centered = mc, n = k,
       method = "Meta-regression on centred moderators (Borenstein et al. 2009, ch. 20)")
}

# CANONICAL TEST
# r <- mac3(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04), 1:6)
# stopifnot(abs(r$QE - 5.88183457856643) < 1e-10)

#' @rdname mac3
#' @keywords internal
#' @export
morie_mac3 <- mac3
