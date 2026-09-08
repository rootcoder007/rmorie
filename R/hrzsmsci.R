# SPDX-License-Identifier: AGPL-3.0-or-later

#' Confidence intervals for the smoothed maximum-score estimator
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 4.3.3: equation (4.25) (page 109), Theorem 4.5
#' (page 113), Theorem 4.6 and equation (4.32) (pages 114-115).  The
#' smoothed estimator maximises
#'
#'   S_sms(b) = (1/n) sum_i (2 Y_i - 1) K(X_i'b / h)
#'
#' with K the integral of a kernel.  Theorem 4.5(b) gives
#' (n h)^(1/2)(bt_n - bt) -> N(-lambda^(1/2) Q^-1 A, Q^-1 D Q^-1), and
#' Theorem 4.6 supplies consistent D_n, Q_n and A_n so that
#' V_n = Q_n^-1 D_n Q_n^-1 (4.32), the bias-corrected estimator is
#' bhat_n = bt_n + (lambda/n)^(s/(2s+1)) Q_n^-1 A_n, and
#' t = (n h)^(1/2)(bhat_nj - bt_j) / V_nj^(1/2) is asymptotically
#' N(0,1) (page 115).
#'
#' Subsampling (Politis and Romano 1994; Delgado et al. 2001) is the
#' book's remedy on page 107 for the UNSMOOTHED estimator, whose limit
#' is nonstandard and for which the bootstrap fails (Abrevaya and Huang
#' 2005).  The smoothed estimator does not need it, and an analytic
#' interval is the only one compatible with this shelf's determinism
#' rule.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric binary 0/1 outcome vector.
#' @param h Numeric bandwidth in (4.25); default n^(-1/(2s+1)).
#' @param alpha Numeric two-sided level.
#' @param s Integer smoothness order; the rate is n^(-s/(2s+1)).
#' @param hstar Numeric separate bandwidth of Theorem 4.6 for A_n;
#'   default n^(-0.5/(2s+1)).
#' @param biascorrect Logical; centre the interval on bhat_n.
#' @param niter Integer sweeps of the coordinate search.
#' @param delta Numeric initial step of the coordinate search.
#' @param b0 Optional numeric starting value for betatilde.
#' @return Named list with estimate, biascorrected, se, lower, upper,
#'   tstat, vcov, bandwidth, hstar, zcrit, objective, n, method.
#' @keywords internal
#' @examples
#' n <- 200
#' x <- cbind(seq(-2, 2, length.out = n), cos(seq_len(n) * 0.8))
#' Smsciband(x, as.numeric(as.numeric(x %*% c(1, 0.6)) >= 0), h = 0.3)$se
#' @export
Smsciband <- function(x, y, h = NULL, alpha = 0.05, s = 2L, hstar = NULL,
                      biascorrect = TRUE, niter = 12L, delta = 1, b0 = NULL) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  if (nrow(X) != length(yv)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) {
    stop("need at least two covariates for a scale normalisation.",
         call. = FALSE)
  }
  if (any(!(yv %in% c(0, 1)))) {
    stop("y must be binary 0/1 for a binary-response model.", call. = FALSE)
  }
  if (!(alpha > 0 && alpha < 1)) {
    stop("alpha must lie strictly between 0 and 1.", call. = FALSE)
  }
  si <- as.integer(s)
  hh <- if (is.null(h)) n^(-1 / (2 * si + 1)) else as.numeric(h)
  hs <- if (is.null(hstar)) n^(-0.5 / (2 * si + 1)) else as.numeric(hstar)
  sgn <- 2 * yv - 1
  Xt <- X[, -1L, drop = FALSE]

  negS <- function(bt) {
    b <- c(1, as.numeric(bt))
    -sum(sgn * stats::pnorm(as.numeric(X %*% b) / hh)) / n
  }
  start <- if (is.null(b0)) {
    ols <- as.numeric(qr.solve(X, yv))
    if (abs(ols[1L]) > 1e-12) ols[-1L] / ols[1L] else rep(0, d - 1L)
  } else as.numeric(b0)
  cm <- .hrz_coord_min(negS, start, niter = as.integer(niter),
                       delta = as.numeric(delta))
  bt <- cm$par
  beta <- c(1, bt)
  z <- as.numeric(X %*% beta)

  kp <- .hrz2_gk(z / hh)
  Dn <- crossprod(Xt * (kp * kp), Xt) / (n * hh)

  eps <- 1e-5
  grad <- function(btv) {
    b <- c(1, as.numeric(btv))
    kpv <- .hrz2_gk(as.numeric(X %*% b) / hh)
    colSums(Xt * (sgn * kpv)) / (n * hh)
  }
  Qn <- matrix(0, d - 1L, d - 1L)
  for (j in seq_len(d - 1L)) {
    bp <- bt
    bp[j] <- bp[j] + eps
    bm <- bt
    bm[j] <- bm[j] - eps
    Qn[, j] <- (grad(bp) - grad(bm)) / (2 * eps)
  }
  Qn <- 0.5 * (Qn + t(Qn))
  An <- colSums(Xt * (sgn * .hrz2_gk(z / hs))) / (n * hs^(si + 1))

  Qi <- tryCatch(solve(Qn + diag(1e-12, d - 1L)),
                 error = function(e) .morie_ginv(Qn))
  Vn <- Qi %*% Dn %*% Qi
  se <- sqrt(pmax(diag(Vn), 0) / (n * hh))
  bhat <- bt + (1 / n)^(si / (2 * si + 1)) * as.numeric(Qi %*% An)
  centre <- if (isTRUE(biascorrect)) bhat else bt
  zc <- stats::qnorm(1 - alpha / 2)
  list(estimate = beta, biascorrected = c(1, bhat), se = se,
       lower = centre - zc * se, upper = centre + zc * se,
       tstat = ifelse(se > 0, centre / ifelse(se > 0, se, 1), NaN),
       vcov = Vn, bandwidth = hh, hstar = hs, zcrit = zc,
       objective = -cm$value, n = n,
       method = "Horowitz (2009) Theorem 4.6, eq. (4.32) analytic CI")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Smsciband
#' @keywords internal
#' @export
morie_horowitz_sms_confidence <- Smsciband
