# SPDX-License-Identifier: AGPL-3.0-or-later
#' Meta-regression R^2: percent of tau^2 explained by moderators
#'
#' R^2 = 100 max(0, (tau2_total - tau2_res)/tau2_total) with
#' tau2_res = max(0, (QE - (k-p)) / trace(P)), P = W - W X (X'WX)^-1 X' W and
#' W = diag(1/v).  Source consulted: Borenstein, Hedges, Higgins and Rothstein
#' (2009), Introduction to Meta-Analysis, chapter 20.  Verified against
#' metafor::rma(mods = ...).
#'
#' @param yi,vi study effects and their within-study variances.
#' @param mods moderator matrix or vector; an intercept column is added.
#' @return list: estimate, tau2_total, tau2_resid, QE, df_resid,
#'   coefficients, se, n, method.
#' @keywords internal
#' @examples
#' marpct(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08), 1:4)$QE
#' @export
marpct <- function(yi, vi, mods) {
  y <- as.numeric(yi); v <- as.numeric(vi); k <- length(y)
  m <- as.matrix(mods)
  if (nrow(m) != k) m <- t(m)
  x <- cbind(1, m)
  dimnames(x) <- NULL
  p <- ncol(x)
  w <- 1 / v
  xtw <- t(x) * rep(w, each = p)
  xtwxi <- solve(xtw %*% x)
  beta <- as.numeric(xtwxi %*% (xtw %*% y))
  resid <- as.numeric(y - x %*% beta)
  qe <- sum(w * resid * resid)
  trp <- sum(w) - sum(diag(xtwxi %*% ((t(x) * rep(w * w, each = p)) %*% x)))
  tau2r <- if (trp > 0) max(0, (qe - (k - p)) / trp) else 0
  tau2t <- k02dl(y, v)$tau2
  ws <- 1 / (v + tau2r)
  xtws <- t(x) * rep(ws, each = p)
  vb <- solve(xtws %*% x)
  betar <- as.numeric(vb %*% (xtws %*% y))
  list(estimate = if (tau2t > 0) 100 * max(0, (tau2t - tau2r) / tau2t) else 0,
       tau2_total = tau2t, tau2_resid = tau2r, QE = qe,
       df_resid = as.integer(k - p), coefficients = betar,
       se = sqrt(diag(vb)), n = k,
       method = "Meta-regression R^2, proportion of tau^2 explained (Borenstein et al. 2009, ch. 20)")
}

# CANONICAL TEST
# r <- marpct(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04), 1:6)
# stopifnot(abs(r$QE - 5.88183457856643) < 1e-10)

#' @rdname marpct
#' @keywords internal
#' @export
morie_marpct <- marpct
