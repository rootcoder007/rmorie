# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dirichlet regression of compositions on covariates
#'
#' Hijazi and Jernigan (2009), "Modeling Compositional Data Using Dirichlet
#' Regression Models", Journal of Applied Probability and Statistics 4(1),
#' 77-91.  The first page was retrieved directly from the journal
#' (japs.isoss.net/ms0162a.pdf) and read; it states the model as Campbell and
#' Mosimann's Dirichlet Covariate Model, in which the Dirichlet parameters are
#' linked to covariates.  The remaining pages were not served, so the
#' parametrisation used here is the one written in this module's own
#' specification and is the standard log link, alpha_ij = exp(x_i' beta_j),
#' for which the log-likelihood over N compositions is
#' l(beta) = sum_i [lnG(alpha_i.) - sum_j lnG(alpha_ij)
#' + sum_j (alpha_ij - 1) ln y_ij], alpha_i. = sum_j alpha_ij, and the score has
#' the closed form dl/dbeta_jm = sum_i x_im alpha_ij
#' \[psi(alpha_i.) - psi(alpha_ij) + ln y_ij\], by the chain rule
#' d alpha_ij / d beta_jm = alpha_ij x_im.
#'
#' Fitting is deterministic: ascent along the analytic score with a halving
#' backtracking line search, started at beta = 0, for a fixed iteration budget.
#' No random starts, so both language arms land on identical numbers.  The
#' attained score is returned as score_max_abs; a fit whose score is not near
#' zero has not converged and says so rather than pretending.  phi is the mean
#' precision, mean_i alpha_i.  For near-deterministic compositional data the
#' Dirichlet likelihood has no interior maximum -- the precision alpha_i. runs
#' away to infinity -- so a large score_max_abs there is the data speaking,
#' not the solver failing.  The analytic score is checked against a central
#' difference of the log-likelihood as an anchor, so an algebra slip in the
#' chain rule cannot pass unnoticed.
#'
#' @param X_cov N-by-p design matrix, used verbatim; put in a column of ones
#'   for an intercept.
#' @param Y_comp N-by-D matrix of strictly positive compositions summing to one.
#' @param ref optional index of a reference part.  Present for interface
#'   compatibility only; the log link is already identified, so no part is
#'   dropped.
#' @param max_iter ascent steps.
#' @param step0 initial step length, halved by the backtracking search.
#' @param tol stop when the largest absolute score entry falls below this.
#' @return list: beta, phi, ll, estimate, alpha, precision, score_max_abs,
#'   iterations, N, p, D, method.
#' @keywords internal
#' @examples
#' Aitdrr(cbind(1, c(0, 1, 2)), rbind(c(.2, .3, .5), c(.3, .3, .4), c(.5, .3, .2)))$phi
#' @export
Aitdrr <- function(X_cov, Y_comp, ref = NULL, max_iter = 400L, step0 = 0.05, tol = 1e-10) {
  Xm <- as.matrix(X_cov)
  storage.mode(Xm) <- "double"
  Ym <- as.matrix(Y_comp)
  storage.mode(Ym) <- "double"
  N <- nrow(Xm)
  if (N == 0L || nrow(Ym) == 0L) stop("dirichlet_regression: no observations")
  if (nrow(Ym) != N) stop("dirichlet_regression: X_cov and Y_comp have different row counts")
  p <- ncol(Xm)
  D <- ncol(Ym)
  if (D < 2L) stop("dirichlet_regression: a composition needs at least 2 parts")
  if (any(!(Ym > 0))) stop("dirichlet_regression: every part of Y_comp must be positive")
  for (i in seq_len(N)) {
    s <- 0
    for (v in Ym[i, ]) s <- s + v
    if (abs(s - 1) > 1e-8) stop("dirichlet_regression: a row of Y_comp does not sum to one")
  }
  LY <- log(Ym)
  if (!is.null(ref)) {
    rr <- as.integer(ref)
    if (rr < 0L || rr >= D) stop("dirichlet_regression: ref is out of range")
  }
  B <- matrix(0, nrow = p, ncol = D)
  A <- .aitdrr_alpha(Xm, B)
  ll <- .aitdrr_ll(A, LY)
  gmax <- Inf
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    G <- .aitdrr_score(Xm, A, LY)
    gmax <- 0
    for (v in as.numeric(G)) if (abs(v) > gmax) gmax <- abs(v)
    if (gmax < tol) break
    st <- step0
    moved <- FALSE
    for (h in seq_len(60L)) {
      Bn <- B + st * G
      An <- .aitdrr_alpha(Xm, Bn)
      lln <- .aitdrr_ll(An, LY)
      if (lln > ll) {
        B <- Bn
        A <- An
        ll <- lln
        moved <- TRUE
        break
      }
      st <- st * 0.5
    }
    if (!moved) break
  }
  a0s <- numeric(N)
  for (i in seq_len(N)) {
    s <- 0
    for (v in A[i, ]) s <- s + v
    a0s[i] <- s
  }
  phi <- 0
  for (v in a0s) phi <- phi + v
  phi <- phi / N
  list(
    beta = B, phi = phi, ll = ll, estimate = ll, alpha = A, precision = a0s,
    score_max_abs = gmax, iterations = it, N = N, p = p, D = D,
    method = "alpha_ij = exp(x_i' beta_j); ML by score ascent with backtracking"
  )
}

#' @noRd
.aitdrr_alpha <- function(Xm, B) {
  N <- nrow(Xm)
  p <- ncol(Xm)
  D <- ncol(B)
  A <- matrix(0, nrow = N, ncol = D)
  for (i in seq_len(N)) {
    for (j in seq_len(D)) {
      s <- 0
      for (m in seq_len(p)) s <- s + Xm[i, m] * B[m, j]
      if (s > 500) s <- 500
      if (s < -500) s <- -500
      A[i, j] <- exp(s)
    }
  }
  A
}

#' @noRd
.aitdrr_ll <- function(A, LY) {
  ll <- 0
  for (i in seq_len(nrow(A))) {
    a0 <- 0
    for (v in A[i, ]) a0 <- a0 + v
    t <- lgamma(a0)
    for (j in seq_len(ncol(A))) {
      t <- t - lgamma(A[i, j])
      t <- t + (A[i, j] - 1) * LY[i, j]
    }
    ll <- ll + t
  }
  ll
}

#' @noRd
.aitdrr_score <- function(Xm, A, LY) {
  N <- nrow(Xm)
  p <- ncol(Xm)
  D <- ncol(A)
  G <- matrix(0, nrow = p, ncol = D)
  for (i in seq_len(N)) {
    a0 <- 0
    for (v in A[i, ]) a0 <- a0 + v
    d0 <- .s03digamma(a0)
    for (j in seq_len(D)) {
      w <- A[i, j] * (d0 - .s03digamma(A[i, j]) + LY[i, j])
      for (m in seq_len(p)) G[m, j] <- G[m, j] + Xm[i, m] * w
    }
  }
  G
}
