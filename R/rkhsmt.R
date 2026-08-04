# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-trait Bayesian kernel regression with shared kernel matrix
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer, read
#' as rendered page images.
#'
#' Volume [Pages 251-336], Chapter 8, Section 8.9 "Multi-trait Bayesian
#' Kernel", p. 288, is where the index sends you, and it contains no equation
#' at all.  Its whole specification is the sentence "In BGLR, it is possible to
#' fit multi-trait Bayesian kernel BLUP methods, and the fitting process is
#' exactly the same as fitting multi-trait Bayesian GBLUP methods (see
#' Chap. 6).  The only difference is that instead of using a linear kernel, any
#' kernel can be used."  The model is therefore taken from where that sentence
#' points.
#'
#' Volume [Pages 171-208], Chapter 6, Section 6.8.1, equation (6.9), p. 191:
#' Y = 1_J mu^T + X B + Z_1 b_1 + E, with E ~ MN_{J x n_T}(0, I_J, R) and
#' b_1 ~ MN_{J x n_T}(0, G, Sigma_T), that is vec(b_1) ~ N(0, Sigma_T (x) G).
#' Section 8.9 replaces G by the kernel K.  The marginal
#' vec(Y) ~ MVN(0, Sigma_g (x) K + Sigma_e (x) I) is that model's marginal; it
#' is NOT printed anywhere in the book and no equation number should ever be
#' attached to it.
#'
#' The Gibbs sampler is the six numbered steps on p. 193: beta and mu at their
#' normal full conditionals; g = vec(b_1) ~ N(g~, G~) with
#' G~ = [(Sigma_T^-1 (x) G^-1) + (R^-1 (x) Z_1^T Z_1)]^-1 and
#' g~ = G~ (R^-1 (x) Z_1^T) vec(Y - 1_J mu^T - X B);
#' Sigma_T ~ IW(v_T + J, b_1^T G^-1 b_1 + S_T); and
#' R ~ IW(v_R + J, S_R + (Y - 1_J mu^T - X B - Z_1 b_1)^T (same)).
#'
#' BOOK ERRATUM, recorded.  Step 5 as printed writes the residual scale as
#' "S_T + ..." where it must be S_R; S_T is the trait genetic scale of step 4
#' and cannot also be the residual scale.  The same typo appears in the
#' Section 6.9 sampler on p. 196.  S_R is used here.
#'
#' DETERMINISM.  Nothing is sampled.  Every step is taken at the exact mean of
#' its own full conditional -- the normal steps at the means written above, and
#' the two inverse-Wishart steps at E[IW(v, S)] = S/(v - n_T - 1), the mean the
#' book's own exponent |Sigma|^{-(v+n_T+1)/2} implies.  Iterating those
#' conditional means is the EM fixed point of the same sampler, so both arms
#' land on identical numbers rather than on the same posterior.
#'
#' @param Y J-by-n_T matrix of phenotypes; row j is line j, column t trait t.
#' @param K J-by-J kernel (or genomic relationship) matrix, symmetric.
#' @param n_iter maximum number of conditional-mean sweeps.
#' @param X optional J-by-p fixed-effect design.  Do NOT include an intercept
#'   column: mu is already the intercept, and an X that carries one leaves beta
#'   unidentified.  The solve is ridged, so both arms still agree on a value,
#'   but that value means nothing.
#' @param Z1 optional J-by-J incidence matrix of lines; identity when absent.
#' @param v_T,S_T,v_R,S_R inverse-Wishart hyperparameters of steps 4 and 5.
#'   Default v = n_T + 2, S = I, the smallest degrees of freedom at which the
#'   inverse-Wishart mean exists.
#' @param tol fixed-point tolerance on the genetic effects.
#' @return list: estimate, gebv, b1, Sigma_T, R, mu, beta, n, method.
#' @keywords internal
#' @examples
#' Y <- cbind(c(1, 2, 3, 0.5), c(2, 1.5, 3.5, 1))
#' Rkhsmt(Y, diag(4))$estimate
#' @export
Rkhsmt <- function(Y, K, n_iter = 200L, X = NULL, Z1 = NULL, v_T = NULL,
                   S_T = NULL, v_R = NULL, S_R = NULL, tol = 1e-12) {
  YY <- .s03mat(Y)
  J <- nrow(YY)
  if (J < 2L) stop("rkhs_multitrait: need at least two lines")
  nT <- ncol(YY)
  if (nT < 1L) stop("rkhs_multitrait: need at least one trait")
  KK <- .s03mat(K)
  if (nrow(KK) != J || ncol(KK) != J) {
    stop("rkhs_multitrait: K must be a square matrix of order J")
  }
  if (max(abs(KK - t(KK))) > 1e-12) stop("rkhs_multitrait: K must be symmetric")
  if (is.null(X)) {
    XX <- NULL; p <- 0L
  } else {
    XX <- .s03mat(X)
    if (nrow(XX) != J) stop("rkhs_multitrait: X has a different number of rows than Y")
    p <- ncol(XX)
  }
  ZZ <- if (is.null(Z1)) diag(1, J) else .s03mat(Z1)
  if (nrow(ZZ) != J || ncol(ZZ) != J) {
    stop("rkhs_multitrait: Z1 must be a J-by-J incidence matrix")
  }
  vT <- if (is.null(v_T)) nT + 2 else as.numeric(v_T)
  vR <- if (is.null(v_R)) nT + 2 else as.numeric(v_R)
  if (vT <= nT + 1 || vR <= nT + 1) {
    stop("rkhs_multitrait: the degrees of freedom must exceed n_T + 1")
  }
  ST <- if (is.null(S_T)) diag(1, nT) else .s03mat(S_T)
  SR <- if (is.null(S_R)) diag(1, nT) else .s03mat(S_R)
  it <- as.integer(n_iter)
  if (it < 1L) stop("rkhs_multitrait: n_iter must be at least 1")

  Kinv <- .mvsinv(KK)
  ZtZ <- crossprod(ZZ)
  b1 <- matrix(0, J, nT)
  beta <- matrix(0, max(p, 1L), nT)
  mu <- numeric(nT)
  SigT <- diag(1, nT)
  Rm <- diag(1, nT)
  resid <- function(drop_mu = FALSE, drop_beta = FALSE, drop_b = FALSE) {
    out <- YY
    if (!drop_mu) out <- out - matrix(mu, J, nT, byrow = TRUE)
    if (!drop_beta && p > 0L) out <- out - XX %*% beta[seq_len(p), , drop = FALSE]
    if (!drop_b) out <- out - ZZ %*% b1
    out
  }
  for (sweep in seq_len(it)) {
    prev <- b1
    if (p > 0L) {
      Rres <- resid(drop_beta = TRUE)
      A <- crossprod(XX)
      for (t in seq_len(nT)) {
        beta[seq_len(p), t] <- .s03ridgesolve(A, as.numeric(crossprod(XX, Rres[, t])), 1e-12)
      }
    }
    Rres <- resid(drop_mu = TRUE)
    mu <- colSums(Rres) / J
    Rres <- resid(drop_b = TRUE)
    Rinv <- .mvsinv(Rm)
    STinv <- .mvsinv(SigT)
    M <- kronecker(STinv, Kinv) + kronecker(Rinv, ZtZ)
    RHS <- crossprod(ZZ, Rres) %*% Rinv
    g <- .s03ridgesolve(M, as.numeric(RHS), 1e-12)
    b1 <- matrix(g, J, nT)
    SS <- crossprod(b1, Kinv %*% b1) + ST
    SigT <- SS / (vT + J - nT - 1)
    E <- resid()
    SS <- crossprod(E) + SR
    Rm <- SS / (vR + J - nT - 1)
    if (max(abs(b1 - prev)) < tol) break
  }
  gebv <- ZZ %*% b1
  list(estimate = sum(gebv) / (J * nT), gebv = gebv, b1 = b1, Sigma_T = SigT,
       R = Rm, mu = mu, beta = if (p > 0L) beta[seq_len(p), , drop = FALSE] else beta[0, , drop = FALSE],
       n = J,
       method = paste0("Chapter 6 eq. (6.9) with G replaced by the kernel K ",
                       "per Sect. 8.9, every Gibbs step taken at its ",
                       "conditional mean"))
}

# Inverse of a symmetric positive definite matrix, by columns, symmetrised so
# that the two arms cannot drift on round-off.
.mvsinv <- function(A, ridge = 1e-12) {
  n <- nrow(A)
  out <- matrix(0, n, n)
  for (j in seq_len(n)) {
    e <- numeric(n); e[j] <- 1
    out[, j] <- .s03ridgesolve(A, e, ridge)
  }
  (out + t(out)) / 2
}
