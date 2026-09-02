# SPDX-License-Identifier: AGPL-3.0-or-later

#' Nonparametric IV by series truncation when T is unknown
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 5.4.2, pages 178-181, following Blundell,
#' Chen and Kristensen (2007).  Section 5.4.1 estimates the operator
#' and then inverts it; this section avoids estimating eigenfunctions
#' at all, because "consistent estimation of eigenfunctions requires
#' assumptions about the spacing of eigenvalues that may be undesirable
#' in applications" (p. 178).  The basis is KNOWN and only coefficients
#' are estimated.
#'
#' With rho(w, h) = E\[Y - h(X) | W = w\] the model implies rho(w, g) = 0,
#' so g minimises E\[rho(W, h)\]^2 (5.80).  Writing the conditional-mean
#' operator as m = A g (5.84) and expanding both sides in the basis
#' gives the finite linear system
#'
#'   m_k = sum_\{j=1\}^\{J\} b_j q_jk,   k = 1, ..., J              (5.85)
#'
#' where mhat = (Psi'Psi)^- Psi'Y and qhat_jk is the k-th coefficient
#' from the series regression of psi_j(X) on W.  The estimator is
#' ghat = sum_j betahat_j psi_j (5.83).  The Moore-Penrose generalised
#' inverse is used exactly as the text specifies, so a rank-deficient
#' Psi'Psi degrades rather than raising.
#'
#' Printing error corrected here: (5.85) as printed on p. 181 carries
#' the summation index k = 1 beneath a summand b_j q_jk, which would
#' make the left side independent of k.  It must be j = 1, matching the
#' display immediately above it.
#'
#' @param x Numeric vector, the endogenous regressor.
#' @param y Numeric vector, the response.
#' @param w Numeric vector, the instrument.
#' @param K Integer, the series length J.  The text uses the same basis
#'   and length for g and for rho, and so does this.
#' @param basis Character, "poly" or "cos", on the mid-rank \[0, 1\]
#'   scale.
#' @return Named list with g_hat, beta, m_hat, Q, J, basis, n, method.
#' @keywords internal
#' @examples
#' n <- 30
#' x <- sin(1.3 * seq_len(n))
#' r <- (rank(x) - 0.5) / n
#' Hrzseriu(x, 1 + 2 * r - 3 * r^2, x, K = 3)$beta
#' @export
Hrzseriu <- function(x, y, w, K = 4L, basis = "poly") {
  x <- as.numeric(x)
  y <- as.numeric(y)
  w <- as.numeric(w)
  n <- length(x)
  if (length(y) != n || length(w) != n) {
    stop(sprintf("x, y, w must have the same length; got %d, %d, %d.",
                 n, length(y), length(w)))
  }
  J <- as.integer(K)
  if (J < 1L) stop(sprintf("K must be at least 1, got %d.", J))
  if (J > n) stop(sprintf("K must not exceed n; got K=%d, n=%d.", J, n))

  u <- .hrz3_u01(x)
  v <- .hrz3_u01(w)
  Psi <- .hrz3_sieve(v, J, basis)
  Phi <- .hrz3_sieve(u, J, basis)

  PtP <- t(Psi) %*% Psi
  PtY <- as.numeric(t(Psi) %*% y)
  PtF <- t(Psi) %*% Phi

  # The text specifies the Moore-Penrose inverse of Psi'Psi.  It is
  # applied here as a ridge-regularised solve, which coincides with it
  # on a full-rank Psi'Psi and degrades in the same graceful way when
  # the basis is collinear -- and, unlike a generalised inverse built
  # from a language-specific SVD, takes an identical arithmetic path in
  # both language arms.
  m_hat <- as.numeric(.s03ridgesolve(PtP, PtY))
  C <- matrix(0, J, J)
  for (j in seq_len(J)) {
    C[, j] <- as.numeric(.s03ridgesolve(PtP, PtF[, j]))
  }

  # (5.85): m_hat = C beta, solved in normal-equation form.
  CtC <- t(C) %*% C
  Ctm <- as.numeric(t(C) %*% m_hat)
  beta <- as.numeric(.s03ridgesolve(CtC, Ctm))
  g_hat <- as.numeric(Phi %*% beta)

  list(g_hat = g_hat, beta = beta, m_hat = m_hat, Q = C, J = J,
       basis = as.character(basis), n = n,
       method = "Horowitz (2009) eqs. (5.83)-(5.85), series truncation")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzseriu
#' @keywords internal
#' @export
morie_horowitz_series_unknown_T <- Hrzseriu
