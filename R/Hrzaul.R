# SPDX-License-Identifier: AGPL-3.0-or-later

#' Nonparametric additive model with an unknown link, by penalised
#' least squares
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 3.3, pages 77-80, implementing the estimator
#' of Horowitz and Mammen (2007).  The model is
#'
#'   E(Y | X = x) = G[m_1(x^1) + ... + m_d(x^d)]
#'
#' with G and all m_j unknown.  This nests both the single-index model
#' and the additive model with identity link.
#'
#' Identification requires normalisations, because (3.1) is unchanged
#' if m_j -> m_j + a_j with G(nu) -> G(nu - sum a_j), and equally if
#' m_j -> c m_j with G(nu) -> G(nu / c).  The text uses mu = 0 and
#'
#'   int m_j(v) dv = 0,  j = 1, ..., d                          (3.25)
#'   sum_j int m_j^2(v) dv = 1                                  (3.26)
#'
#' (3.25) is imposed EXACTLY here, by expanding each m_j in centred
#' monomials v^k - 1/(k+1) which integrate to zero on [0, 1] by
#' construction; (3.26) is imposed exactly by rescaling against the
#' closed-form Gram matrix.  Neither is approached iteratively, so both
#' are identities of the returned fit rather than things that happen to
#' hold at convergence.
#'
#' Identification further requires at least two nonconstant additive
#' components (p. 78): with only one, (3.27) is satisfied by many
#' different pairs (G, m_1).  d < 2 therefore raises.
#'
#' The estimator solves
#'
#'   min (1/n) sum_i {Y_i - G[m_1(X_i^1) + ... + m_d(X_i^d)]}^2
#'       + lambda_n^2 J(G, m_1, ..., m_d)                       (3.28)
#'
#' computed, as the text describes, "by a backfitting algorithm that
#' alternates between two steps": with G held fixed the objective is
#' minimised over the m_j coefficients, and with the m_j held fixed it
#' is "an unconstrained quadratic programming problem that can be
#' solved analytically" for G.  The roughness penalty is the integrated
#' squared second derivative, in closed form for this basis.
#'
#' The PLS estimator has NO bandwidth: (3.28) is penalised, not
#' smoothed.  The bandwidth argument is retained for API stability and
#' is used as the penalty constant lambda_n, whose default
#' n^(-k/(2k+1)) is assumption PLS4 with k = 2.
#'
#' Theorem 3.9 gives rates but no asymptotic distribution, so no
#' standard errors are returned; p. 80 states plainly that "it is not
#' yet possible to carry out statistical inference with this
#' estimator".
#'
#' @param x Numeric n by d matrix of covariates, d >= 2.
#' @param y Numeric vector, the response.
#' @param bandwidth Numeric or NULL; the penalty constant lambda_n,
#'   default `n^(-0.4)`.
#' @param degree Integer, polynomial degree of each additive component.
#' @param link_degree Integer, polynomial degree of the link.
#' @param iters Integer, backfitting sweeps; a FIXED count with no
#'   tolerance-based exit.
#' @return Named list with G_hat, m_j_hats, index, link_coef, m_coef,
#'   loc_norm, scale_norm, lambda_n, rss, n, d, method.
#' @keywords internal
#' @examples
#' n <- 40
#' x <- cbind(sin(1.1 * seq_len(n)), cos(0.7 * seq_len(n)))
#' Hrzaul(x, x[, 1] + x[, 2], iters = 3L)$scale_norm
#' @export
Hrzaul <- function(x, y, bandwidth = NULL, degree = 3L, link_degree = 3L,
                   iters = 15L) {
  y <- as.numeric(y)
  n <- length(y)
  X <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else as.matrix(x)
  if (nrow(X) != n) stop(sprintf("x must have %d rows, got %d.", n, nrow(X)))
  d <- ncol(X)
  if (d < 2L) {
    stop(sprintf(paste("identification of G and the additive components",
                       "requires at least two nonconstant components",
                       "(p. 78); got d = %d."), d))
  }
  if (n < 6L) stop(sprintf("need at least 6 observations, got %d.", n))
  p <- as.integer(degree)
  pg <- as.integer(link_degree)
  if (p < 1L || pg < 1L) {
    stop(sprintf("degrees must be >= 1, got %d and %d.", p, pg))
  }
  lam <- if (is.null(bandwidth)) n^(-0.4) else as.numeric(bandwidth)
  if (lam < 0) stop(sprintf("lambda_n must be non-negative, got %g.", lam))
  iters <- as.integer(iters)
  if (iters < 1L) stop(sprintf("iters must be at least 1, got %d.", iters))

  kk <- seq_len(p)
  U <- lapply(seq_len(d), function(j) .hrz3_u01(X[, j]))
  # B[[j]][i, k] = phi_k(u_ij), phi_k(v) = v^k - 1/(k+1).
  B <- lapply(seq_len(d), function(j) {
    outer(U[[j]], kk, "^") - rep(1 / (kk + 1), each = n)
  })
  Om <- outer(kk, kk, function(a, b) 1 / (a + b + 1) - 1 / ((a + 1) * (b + 1)))
  Rm <- outer(kk, kk, function(a, b) {
    ifelse(a >= 2 & b >= 2, a * (a - 1) * b * (b - 1) / (a + b - 3), 0)
  })

  scale_norm <- function(cc) {
    s <- 0
    for (j in seq_len(d)) s <- s + sum(outer(cc[[j]], cc[[j]]) * Om)
    s
  }
  renorm <- function(cc) {
    s <- scale_norm(cc)
    if (s <= 0) {
      stop(paste("the additive components collapsed to zero, so the scale",
                 "normalisation (3.26) cannot be imposed."))
    }
    lapply(cc, function(v) v * s^-0.5)
  }
  cc <- renorm(lapply(seq_len(d), function(j) c(1, rep(0, p - 1))))

  index <- function(cc) {
    nu <- rep(0, n)
    for (j in seq_len(d)) nu <- nu + as.numeric(B[[j]] %*% cc[[j]])
    nu
  }

  a <- rep(0, pg + 1L)
  qq <- seq_len(pg + 1L) - 1L
  for (it in seq_len(iters)) {
    nu <- index(cc)
    # --- G step: unconstrained ridge on the link coefficients.
    D <- outer(nu, qq, "^")
    lo <- min(nu)
    hi <- max(nu)
    Rg <- outer(qq, qq, function(u, v) {
      e <- u + v - 3
      ifelse(u >= 2 & v >= 2,
             u * (u - 1) * v * (v - 1) * (hi^(e + 1) - lo^(e + 1)) / (e + 1),
             0)
    })
    A1 <- t(D) %*% D + lam * lam * Rg
    a <- as.numeric(.s03ridgesolve(A1, as.numeric(t(D) %*% y)))

    # --- m step: linearise G about the current index (Gauss-Newton).
    G0 <- as.numeric(D %*% a)
    Gp <- as.numeric(outer(nu, qq[-1] - 1L, "^") %*% (qq[-1] * a[-1]))
    tt <- y - G0 + Gp * nu
    A <- matrix(0, n, d * p)
    for (j in seq_len(d)) {
      A[, ((j - 1L) * p + 1L):(j * p)] <- B[[j]] * Gp
    }
    A2 <- t(A) %*% A
    for (j in seq_len(d)) {
      idx <- ((j - 1L) * p + 1L):(j * p)
      A2[idx, idx] <- A2[idx, idx] + lam * lam * Rm
    }
    sol <- as.numeric(.s03ridgesolve(A2, as.numeric(t(A) %*% tt)))
    cc <- renorm(lapply(seq_len(d),
                        function(j) sol[((j - 1L) * p + 1L):(j * p)]))
  }

  nu <- index(cc)
  G_hat <- as.numeric(outer(nu, qq, "^") %*% a)
  m_hats <- lapply(seq_len(d), function(j) as.numeric(B[[j]] %*% cc[[j]]))
  rss <- sum((y - G_hat)^2)

  list(G_hat = G_hat, m_j_hats = m_hats, index = nu, link_coef = a,
       m_coef = cc, loc_norm = 0, scale_norm = scale_norm(cc),
       lambda_n = lam, rss = rss, n = n, d = d,
       method = "Horowitz (2009) eq. (3.28), Horowitz-Mammen PLS")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzaul
#' @keywords internal
#' @export
morie_horowitz_additive_unknown_link <- Hrzaul
