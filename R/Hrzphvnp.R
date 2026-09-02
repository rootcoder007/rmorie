# SPDX-License-Identifier: AGPL-3.0-or-later

#' Proportional hazards model with unobserved heterogeneity and a
#' nonparametric frailty distribution
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 6.3.4, pages 223-226, implementing Horowitz
#' (1999).  The model lambda(y | x, v) = lambda_0(y) exp(-x'beta) v is
#' equivalent to
#'
#'   log Lambda_0(Y) = X'beta + V + U                           (6.68)
#'
#' with U independent of (X, V) and F(u) = 1 - exp(-exp(-u)).
#' Writing the transformation model as T(Y) = X'alpha + W (6.69)
#' relates the two by T(y) = log Lambda_0(y) / sigma and
#' W = (V + U)/sigma with sigma = |beta_1|, so (6.68) is a RESCALED
#' transformation model and everything reduces to estimating the single
#' scalar sigma.
#'
#' sigma is recovered from the small-y limit (6.75)-(6.76), whose
#' sample analogue carries an explicit leading minus:
#'
#'   sigma_n(y) = - int Gnz(y|z) pnZ(z)^2 dz
#'                / int Gn(y|z) pnZ(z)^2 dz                     (6.80)
#'
#' That minus sign was read off a RENDERED IMAGE of page 225, not an
#' extracted text layer, because pdftotext drops minus signs.  It is
#' load-bearing: G_z < 0 by (6.74), so without it sigma_n comes out
#' negative and every downstream quantity inverts.  Note also that
#' (6.80) uses the kernel argument (z - Z_nj)/h_n, the OPPOSITE
#' orientation to (6.61) in Section 6.3.1; each section is implemented
#' as printed, which is why the two derivative terms differ in sign
#' here and in [Hrztmod()].
#'
#' The plain estimator sigma_n(y_n) converges no faster than n^(-1/3).
#' Under E exp(-3V) < Inf (PHU3(ii)) the Schucany-Sommers bias
#' correction
#'
#'   sigma_n = \[sigma_n(y_n1) - n^(-q(1-delta)) sigma_n(y_n2)\]
#'             / \[1 - n^(-q(1-delta))\]                          (6.81)
#'
#' reaches a rate arbitrarily close to the n^(-2/5) that Ishwaran
#' (1996) shows is optimal, and that correction is applied.  The
#' admissible ranges 1/5 < q < 1/4 and 1/(2q) - 3/2 < delta < 1 are
#' enforced.
#'
#' y_n1, y_n2 are defined by Lambda_0(y_n1) = c n^(-q) and
#' Lambda_0(y_n2) = c n^(-delta q), which is circular since Lambda_0 is
#' what is being estimated.  For small y,
#' P(Y <= y) = 1 - int exp(-Lambda_0(y) exp(-v)) dF_V
#' is approximately Lambda_0(y) E exp(-V), so the CDF level is
#' proportional to Lambda_0; the two points are therefore taken as the
#' empirical quantiles of Y at levels n^(-q) and n^(-delta q).  This
#' resolution of the circularity is stated here because the text does
#' not give one -- p. 173 notes that "methods for choosing a_n and h_n
#' in applications are not yet available", and the same is true of
#' these sequences.
#'
#' Then Lambda_0 and lambda_0 follow from
#'
#'   Lambda_n0(y) = exp\[sigma_n Tn(y)\]                          (6.70)
#'   lambda_n0(y) = sigma_n Tn'(y) exp\[sigma_n Tn(y)\]           (6.71)
#'
#' and beta = sigma alpha.  Because Tn(y0) = 0 by construction,
#' Lambda_n0(y0) = 1 exactly, which is the location normalisation the
#' section requires.
#'
#' frailty_dist is the estimated CDF of W = (V + U)/sigma from (6.66),
#' which is what the data identify without a further deconvolution
#' step; recovering F_V itself would require deconvolving the known
#' extreme-value F_U out of it, and that is NOT done here.
#'
#' The stub docstring this replaced made three claims that Section
#' 6.3.4 contradicts, all checked against pages 223-225:
#'
#' * "identification via multiple spells".  The section identifies from
#'   a SINGLE spell: "Elbers and Ridder (1982) showed that model (6.68)
#'   is identified if Ee^\{-V\} < infinity" (p. 223).  The word "spell"
#'   does not occur in the section.  Multiple-spell identification is a
#'   different literature and is not used here.
#' * "V arbitrary with E\[V\] = 1".  The normalisation the section
#'   actually imposes is Lambda_0(y0) = 1 for some finite y0 > 0
#'   (p. 223), together with |alpha_1| = 1 on the index.  E\[V\] = 1 is
#'   never assumed; what is assumed is Ee^\{-3V\} < infinity (PHU3(ii)).
#' * h(t|X,V) = h_0(t) exp(X'beta) V.  Equation (6.72) writes the
#'   hazard as lambda(y|z,v) = lambda_0(y) exp\[-(sigma z + v)\], i.e.
#'   with a NEGATIVE index and an exp(-v) frailty.  The published
#'   parameterisation is the one implemented.
#'
#' The estimator below follows the source, not the stub.
#'
#' @param t Numeric vector of observed durations, strictly positive.
#' @param x Numeric vector or n by d matrix of covariates.  The first
#'   column carries the normalisation.
#' @param event Numeric vector of 0/1 censoring indicators, or NULL.
#'   Censored rows are dropped, since (6.73)-(6.80) are written for
#'   observed Y.
#' @param ny,nz Integer grid sizes for the transformation-model step.
#' @param nq Integer, quadrature points for the integrals in (6.80).
#' @param q Numeric rate constant, strictly inside (1/5, 1/4).
#' @param delta Numeric bias-correction constant in
#'   (1/(2q) - 3/2, 1).
#' @param bandwidth Numeric or NULL; common bandwidth.
#' @return Named list with beta_hat, alpha_hat, sigma, sigma_y1,
#'   sigma_y2, h0_hat, Lambda0_hat, frailty_dist, frailty_grid, T_hat,
#'   y_grid, y0, n, n_used, method.
#' @keywords internal
#' @examples
#' n <- 40
#' x <- cbind(sin(1.3 * seq_len(n)), cos(0.6 * seq_len(n)))
#' tt <- exp(x[, 1] + 0.5 * x[, 2])
#' Hrzphvnp(tt, x, ny = 7L, nz = 7L, nq = 7L)$sigma > 0
#' @export
Hrzphvnp <- function(t, x, event = NULL, ny = 21L, nz = 21L, nq = 21L,
                     q = 0.22, delta = 0.85, bandwidth = NULL) {
  tv <- as.numeric(t)
  n_all <- length(tv)
  X <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else as.matrix(x)
  if (nrow(X) != n_all) {
    stop(sprintf("x must have %d rows, got %d.", n_all, nrow(X)))
  }
  d <- ncol(X)
  bad <- which(!(tv > 0))
  if (length(bad)) {
    stop(sprintf("durations must be strictly positive; t[%d] = %g.",
                 bad[1L], tv[bad[1L]]))
  }
  if (is.null(event)) {
    keep <- seq_len(n_all)
  } else {
    ev <- as.numeric(event)
    if (length(ev) != n_all) {
      stop(sprintf("t has %d points but event has %d.", n_all, length(ev)))
    }
    keep <- which(ev != 0)
  }
  n <- length(keep)
  if (n < 10L) {
    stop(sprintf("need at least 10 uncensored observations, got %d.", n))
  }
  q <- as.numeric(q)
  delta <- as.numeric(delta)
  if (!(q > 0.2 && q < 0.25)) {
    stop(sprintf("q must lie strictly in (1/5, 1/4), got %g.", q))
  }
  lo_d <- 1 / (2 * q) - 1.5
  if (!(delta > lo_d && delta < 1)) {
    stop(sprintf("delta must lie in (%.6g, 1); got %g.", lo_d, delta))
  }
  nq <- as.integer(nq)
  if (nq < 3L) stop(sprintf("nq must be at least 3, got %d.", nq))

  yv <- tv[keep]
  Xk <- X[keep, , drop = FALSE]

  hb <- if (is.null(bandwidth)) {
    .hrz_silverman(Xk[, 1L])
  } else {
    as.numeric(bandwidth)
  }
  alpha <- .hrz3_index_dir(Xk, yv, hb)
  Z <- as.numeric(Xk %*% alpha)
  hz <- if (is.null(bandwidth)) .hrz_silverman(Z) else as.numeric(bandwidth)

  za <- .s03quantile7(Z, 0.05)
  zb <- .s03quantile7(Z, 0.95)
  if (!(zb > za)) stop("the index has no spread.")
  dz <- (zb - za) / (nq - 1L)
  zg <- za + (seq_len(nq) - 1L) * dz
  wq <- rep(dz, nq)
  wq[1L] <- dz / 2
  wq[nq] <- dz / 2

  # (6.80) with the kernel argument (z - Z_nj)/h as printed.
  sigma_at <- function(yy) {
    num <- 0
    den <- 0
    ind <- as.numeric(yv <= yy)
    for (k in seq_len(nq)) {
      uu <- (zg[k] - Z) / hz
      kv <- exp(-0.5 * uu * uu) / .hrz3_sqrt2pi
      dk <- -(uu / hz) * kv        # d/dz K((z - Z_i)/h)
      A <- sum(ind * kv)
      B <- sum(kv)
      Az <- sum(ind * dk)
      Bz <- sum(dk)
      if (B <= 1e-300) next
      pnz <- B / (n * hz)
      Gn <- A / B
      Gnz <- (Az * B - A * Bz) / (B * B)
      num <- num + wq[k] * Gnz * pnz * pnz
      den <- den + wq[k] * Gn * pnz * pnz
    }
    if (abs(den) < 1e-300) {
      stop(paste("the denominator of (6.80) vanished; y is too small for",
                 "the sample to identify sigma."))
    }
    -num / den
  }

  yn1 <- .s03quantile7(yv, min(n^(-q), 0.99))
  yn2 <- .s03quantile7(yv, min(n^(-delta * q), 0.99))
  s1 <- sigma_at(yn1)
  s2 <- sigma_at(yn2)
  fac <- n^(-q * (1 - delta))
  if (abs(1 - fac) < 1e-12) {
    stop("the bias-correction weight in (6.81) is degenerate.")
  }
  sigma <- (s1 - fac * s2) / (1 - fac)

  tf <- Hrztf(Xk, yv, ny = ny, nz = nz, bandwidth = bandwidth)
  T <- tf$T_hat
  yg <- tf$y_grid
  m <- length(yg)
  dv <- yg[2L] - yg[1L]
  Tp <- numeric(m)
  Tp[1L] <- (T[2L] - T[1L]) / dv
  Tp[m] <- (T[m] - T[m - 1L]) / dv
  if (m > 2L) {
    for (k in 2:(m - 1L)) Tp[k] <- (T[k + 1L] - T[k - 1L]) / (2 * dv)
  }

  Lam <- exp(sigma * T)                    # (6.70)
  lam <- sigma * Tp * exp(sigma * T)       # (6.71)
  beta <- sigma * alpha

  list(beta_hat = beta, alpha_hat = alpha, sigma = sigma, sigma_y1 = s1,
       sigma_y2 = s2, h0_hat = lam, Lambda0_hat = Lam,
       frailty_dist = tf$F_hat, frailty_grid = tf$u_grid, T_hat = T,
       y_grid = yg, y0 = tf$y0, n = n_all, n_used = n,
       method = "Horowitz (2009) eqs. (6.80)-(6.81), (6.70)-(6.71)")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzphvnp
#' @keywords internal
#' @export
morie_horowitz_ph_frailty_nonpar <- Hrzphvnp
