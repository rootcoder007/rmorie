# SPDX-License-Identifier: AGPL-3.0-or-later

#' Nonparametric IV model as a Fredholm equation of the first kind
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 5.3, pages 156-159.  For Y = g(X) + U with
#' E(U | W = w) = 0 and the support of (X, W) mapped into \[0, 1\]^2,
#'
#'   E(Y | W = w) f_W(w) = int_0^1 f_XW(x, w) g(x) dx           (5.40)
#'
#' Multiplying by f_XW(z, w) and integrating over w symmetrises the
#' problem into
#'
#'   r(z)      = int_0^1 tau(x, z) g(x) dx                      (5.41)
#'   r(z)      = int_0^1 E(Y|W=w) f_XW(z, w) f_W(w) dw          (5.42)
#'   tau(x, z) = int_0^1 f_XW(x, w) f_XW(z, w) dw               (5.43)
#'
#' that is r = T g (5.44), with T self-adjoint and positive
#' semi-definite by (5.45).
#'
#' Estimation is by SPECTRAL TRUNCATION, the regularisation the
#' section itself motivates: T has eigenvalues decaying to zero, so
#' T^-1 h = sum_j <h, phi_j> phi_j / lambda_j exists only formally, and
#' terms with tiny lambda_j amplify estimation noise without bound.
#' Truncating at lambda_j > tol * lambda_1 is what makes the inverse
#' usable.  The eigenvalues are returned because that decay IS the
#' ill-posedness rather than an incidental diagnostic.
#'
#' Singularity of T -- a zero eigenvalue -- is exactly the failure of
#' identification (pp. 158-159): if T h = 0 for some nonzero h then g
#' and g + h satisfy (5.40) equally well.
#'
#' @param x Numeric vector, the endogenous regressor.
#' @param y Numeric vector, the response.
#' @param w Numeric vector, the instrument.
#' @param bandwidth Numeric or NULL; kernel bandwidth on the \[0, 1\]
#'   scale, default `1.06 n^(-1/6)/sqrt(12)`.
#' @param grid Integer, number of quadrature points on \[0, 1\].
#' @param tol Numeric, relative eigenvalue cutoff for the truncated
#'   inverse.  This bounds the amplification factor lambda_1/lambda_j
#'   of the retained directions at 1/tol.  The default is not
#'   cosmetic: at tol = 1e-8 this estimator retains a seventh
#'   direction that amplifies by 6e7, and two correct implementations
#'   of the SAME formula then disagree in the seventh decimal of g_hat
#'   purely through rounding.  That is the ill-posedness of Section
#'   5.3 showing up as arithmetic, and the cutoff is what controls it.
#' @return Named list with g_hat, grid_points, r_hat, fW, raw_mass,
#'   eigenvalues, trace_T, n_terms, identified, bandwidth, n, m,
#'   method.
#' @keywords internal
#' @examples
#' n <- 30
#' x <- sin(1.3 * seq_len(n))
#' w <- cos(0.9 * seq_len(n))
#' Hrznpiv(x, 2 * x, w, grid = 11)$n_terms
#' @export
Hrznpiv <- function(x, y, w, bandwidth = NULL, grid = 25L, tol = 1e-5) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  w <- as.numeric(w)
  n <- length(x)
  if (length(y) != n || length(w) != n) {
    stop(sprintf("x, y, w must have the same length; got %d, %d, %d.",
                 n, length(y), length(w)))
  }
  if (n < 3L) stop(sprintf("need at least 3 observations, got %d.", n))
  h <- if (is.null(bandwidth)) .hrz3_bw01(n) else as.numeric(bandwidth)
  if (h <= 0) stop(sprintf("bandwidth must be positive, got %g.", h))
  tol <- as.numeric(tol)
  if (tol <= 0) stop(sprintf("tol must be positive, got %g.", tol))

  u <- .hrz3_u01(x)
  v <- .hrz3_u01(w)
  gw <- .hrz3_grid_w(grid)
  z <- gw$z
  wq <- gw$w
  m <- length(z)

  KW <- .hrz3_kmat(z, v, h)
  fg <- .hrz3_fxw_grid(u, v, z, wq, h)
  fxw <- fg$f
  raw_mass <- fg$mass

  # f_W(w) = int f_XW(x, w) dx.
  fW <- as.numeric(wq %*% fxw)

  # E(Y | W = w) by Nadaraya-Watson on the [0, 1] scale.
  den <- as.numeric(KW %*% rep(1, n))
  mW <- ifelse(den > 1e-300, as.numeric(KW %*% y) / den, 0)

  # r(z), eq. (5.42), and tau(x, z), eq. (5.43).
  r_hat <- as.numeric(fxw %*% (wq * mW * fW))
  tau <- fxw %*% (wq * t(fxw))

  # Symmetrise in the quadrature inner product: S = D^(1/2) tau D^(1/2),
  # so S's eigenpairs give T's eigenpairs with phi = D^(-1/2) s.
  rt <- sqrt(wq)
  S <- outer(rt, rt) * tau
  e <- eigen(S, symmetric = TRUE)
  lam_s <- e$values
  V <- e$vectors

  trace_T <- sum(wq * diag(tau))

  cut <- if (lam_s[1L] > 0) tol * lam_s[1L] else 0
  g_hat <- rep(0, m)
  n_terms <- 0L
  for (j in seq_len(m)) {
    if (lam_s[j] <= cut) next
    n_terms <- n_terms + 1L
    phi <- V[, j] / rt
    ip <- sum(wq * r_hat * phi)
    g_hat <- g_hat + (ip / lam_s[j]) * phi
  }

  list(g_hat = g_hat, grid_points = z, r_hat = r_hat, fW = fW,
       raw_mass = raw_mass, eigenvalues = lam_s, trace_T = trace_T,
       n_terms = n_terms, identified = lam_s[m] > cut,
       bandwidth = h, n = n, m = m,
       method = "Horowitz (2009) eqs. (5.41)-(5.44), spectral truncation of T")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrznpiv
#' @keywords internal
#' @export
morie_horowitz_npiv_model <- Hrznpiv
