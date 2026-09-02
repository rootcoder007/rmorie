# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pooled TMLE across sites
#'
#' van der Laan and Rubin (2006), The International Journal of
#' Biostatistics 2(1), art. 11, for the targeting step; Rothman, Greenland
#' and Lash (2008), Modern Epidemiology, 3rd ed., chapter 15, for
#' fixed-effects pooling, psi = sum w_s psi_s / sum w_s with w_s = 1/se_s^2
#' and variance 1/sum w_s; Higgins and Thompson (2002), Statistics in
#' Medicine 21(11), 1539-1558, for Q = sum w_s (psi_s - psi)^2 and I2 =
#' max(0, (Q - (S-1))/Q).  None was retrievable here as a full text; the
#' three expressions are quoted in their standard published form.
#' Fixed-effects pooling assumes a common effect; I2 is returned precisely
#' so that assumption can be checked rather than asserted.
#'
#' @param y,D outcome and treatment.
#' @param X covariates.
#' @param site site identifier.
#' @param alpha interval level.
#' @return list: estimate, se, ci_lo, ci_hi, site_psi, site_se, site_n, Q,
#'   I2, df, n, method.
#' @keywords internal
#' @examples
#' Tmlepool(c(1, 0, 1, 1, 0, 1), c(1, 0, 1, 0, 1, 0),
#'          site = c(1, 1, 1, 2, 2, 2))$estimate
#' @export
Tmlepool <- function(y, D, X = NULL, site = NULL, alpha = 0.05) {
  yv <- .s03vec(y)
  d <- .s03vec(D)
  n <- length(yv)
  Xr <- if (!is.null(X)) .s03mat(X) else NULL
  lab <- as.character(if (!is.null(site)) site else rep(0, n))
  ids <- character(0)
  for (s in lab) if (!(s %in% ids)) ids <- c(ids, s)
  psis <- numeric(length(ids))
  ses <- numeric(length(ids))
  ns <- integer(length(ids))
  for (si in seq_along(ids)) {
    idx <- which(lab == ids[si])
    ys <- yv[idx]
    ds <- d[idx]
    xs <- if (!is.null(Xr)) Xr[idx, , drop = FALSE] else NULL
    ns[si] <- length(idx)
    if (length(idx) < 3L || sum(ds) <= 0 || sum(ds) >= length(ds)) {
      psis[si] <- NaN
      ses[si] <- NaN
      next
    }
    f <- .s03tmle(ys, ds, xs)
    psis[si] <- f$psi
    ses[si] <- f$se
  }
  num <- 0
  den <- 0
  for (i in seq_along(ids)) {
    if (!is.nan(psis[i]) && !is.nan(ses[i]) && ses[i] > 0) {
      w <- 1 / (ses[i] * ses[i])
      num <- num + w * psis[i]
      den <- den + w
    }
  }
  pool <- if (den > 0) num / den else NaN
  sep <- if (den > 0) sqrt(1 / den) else NaN
  Q <- 0
  S <- 0L
  for (i in seq_along(ids)) {
    if (!is.nan(psis[i]) && !is.nan(ses[i]) && ses[i] > 0) {
      w <- 1 / (ses[i] * ses[i])
      Q <- Q + w * (psis[i] - pool)^2
      S <- S + 1L
    }
  }
  df <- S - 1L
  i2 <- if (Q > 0) max(0, (Q - df) / Q) else 0
  z <- qnorm(1 - as.numeric(alpha) / 2)
  list(estimate = pool, se = sep, ci_lo = pool - z * sep, ci_hi = pool + z * sep,
       site_psi = psis, site_se = ses, site_n = ns, Q = Q, I2 = i2, df = df,
       n = n,
       method = "Site-stratified TMLE with inverse-variance (fixed-effects) pooling; Q and I^2 reported")
}
