# SPDX-License-Identifier: AGPL-3.0-or-later

#' Generalized likelihood ratio test of a parametric mean regression
#'
#' SOURCE.  Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, states on page 3 that "The subject of specification
#' testing, which has received much attention recently, is also not
#' treated."  The book therefore contains no likelihood-ratio-type test
#' and no locator for one.  The primary source consulted instead is
#' Fan, J., Zhang, C. and Zhang, J. (2001), "Generalized Likelihood
#' Ratio Statistics and Wilks Phenomenon", Annals of Statistics 29(1),
#' 153-193, Section 4.1, page 170:
#'
#'   lambda_n = (n/2) log(RSS0 / RSS1),
#'   r_K lambda_n ~ chi^2( r_K c_K abs(Omega) / h )            (4.1)
#'
#' with c_K = K(0) - 2^-1 ||K||_2^2 (page 170) and r_K given in
#' Theorem 5 (page 165).  Remark 4.1 (page 171) extends (4.1) to a
#' parametric null other than the linear model, applied to the
#' residuals of the fitted null.
#'
#' Two things the stub this replaces got wrong: the statistic is
#' r_K lambda_n, not 2 lambda_n; and the degrees of freedom
#' r_K c_K abs(Omega) / h grow as h shrinks, so there is no fixed
#' integer df.
#'
#' Table 2 (page 170) prints r_K = 2.5375 and c_K = 0.7737 for the
#' Gaussian.  The r_K value reproduces exactly from the Theorem 5
#' formula for the STANDARD normal kernel; c_K = 0.7737 does not (the
#' standard normal gives K(0) - ||K||^2/2 = 0.2578954) but equals three
#' times it, which is c_K for a normal kernel rescaled by 3 -- c_K,
#' unlike r_K, is not scale invariant.  This function computes c_K in
#' closed form for the kernel it actually uses; the tabulated pair is
#' available via kernel = "table".
#'
#' @param x Numeric scalar covariate vector.
#' @param y Numeric outcome vector.
#' @param fitted Optional numeric fitted values under the parametric
#'   null; default the least-squares polynomial of the given degree, so
#'   the default null is linearity.
#' @param h Numeric bandwidth of the local linear alternative; default
#'   n^(-1/5).
#' @param degree Integer degree of the default polynomial null.
#' @param kernel Either "closed" (constants computed for the standard
#'   Gaussian kernel used here) or "table" (the Gaussian row of Table 2).
#' @return Named list with statistic, p_value, n, method, lambdan, rk,
#'   ck, df, rss0, rss1, bandwidth, support.
#' @keywords internal
#' @examples
#' xv <- seq(0, 1, length.out = 200)
#' Splrtest(xv, 3 * sin(8 * xv), h = 0.05)$p_value
#' @export
Splrtest <- function(x, y, fitted = NULL, h = NULL, degree = 1L,
                     kernel = "closed") {
  xv <- as.numeric(x)
  yv <- as.numeric(y)
  n <- length(xv)
  if (length(yv) != n) {
    stop("x and y must have the same length.", call. = FALSE)
  }
  if (n < 5L) stop("need at least five observations.", call. = FALSE)
  hh <- if (is.null(h)) n^(-0.2) else as.numeric(h)
  if (hh <= 0) stop("bandwidth must be positive.", call. = FALSE)

  if (is.null(fitted)) {
    P <- outer(xv, 0:as.integer(degree), "^")
    f0 <- as.numeric(P %*% qr.solve(P, yv))
  } else {
    f0 <- as.numeric(fitted)
    if (length(f0) != n) {
      stop("fitted must have one entry per observation.", call. = FALSE)
    }
  }

  u <- outer(xv, xv, "-") / hh
  W <- .hrz2_gk(u)
  dx <- -outer(xv, xv, "-")
  s0 <- rowSums(W)
  s1 <- rowSums(W * dx)
  s2 <- rowSums(W * dx * dx)
  t0 <- rowSums(W * rep(yv, each = n))
  t1 <- rowSums(W * dx * rep(yv, each = n))
  det <- s0 * s2 - s1 * s1
  det <- ifelse(abs(det) > 1e-300, det, 1e-300)
  f1 <- (s2 * t0 - s1 * t1) / det

  rss0 <- sum((yv - f0)^2)
  rss1 <- max(sum((yv - f1)^2), 1e-300)
  lam <- 0.5 * n * log(max(rss0, 1e-300) / rss1)

  if (identical(kernel, "table")) {
    rk <- 2.5375
    ck <- 0.7737
  } else {
    rk <- 2.5374999999999996
    ck <- 1 / sqrt(2 * pi) - 0.5 / (2 * sqrt(pi))
  }
  support <- max(xv) - min(xv)
  df <- rk * ck * support / hh
  statv <- rk * lam
  pval <- if (df > 0) 1 - stats::pchisq(statv, df) else NaN
  list(statistic = statv, p_value = min(max(pval, 0), 1), n = n,
       method = "Fan, Zhang and Zhang (2001) eq. (4.1) GLR / Wilks",
       lambdan = lam, rk = rk, ck = ck, df = df, rss0 = rss0, rss1 = rss1,
       bandwidth = hh, support = support)
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Splrtest
#' @keywords internal
#' @export
morie_horowitz_likelihood_ratio_test <- Splrtest
