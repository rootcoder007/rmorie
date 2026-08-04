# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian multi-trait multi-environment model (BMTME)
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume [Pages 171-208], Chapter 6, Section 6.9 "Bayesian Genomic Multi-trait
#' and Multi-environment Model (BMTME)", pp. 195-197, read as rendered page
#' images.  The chapter attributes the model to Montesinos-Lopez et al. (2016),
#' G3 6, "A genomic Bayesian multi-trait and multi-environment model".
#'
#' Equation (6.11), p. 195: Y = 1_IJ mu^T + X B + Z_1 b_1 + Z_2 b_2 + E, with
#' b_2 ~ MN_{IJ x n_T}(0, Sigma_E (x) G, Sigma_T),
#' b_1 ~ MN_{J x n_T}(0, G, Sigma_T) and E ~ MN_{IJ x n_T}(0, I_IJ, R).
#' I environments, J lines, n_T traits; Z_1 the incidence matrix of lines, Z_2
#' the incidence matrix of the environment-by-line interaction, G the genomic
#' relationship matrix, Sigma_T the trait genetic covariance, Sigma_E the
#' environment covariance, R the residual covariance.
#'
#' The Gibbs sampler is the eight numbered steps on p. 196.  Steps 3 and 4
#' carry the structure:
#' G~ = [(Sigma_T^-1 (x) G^-1) + (R^-1 (x) Z_1^T Z_1)]^-1,
#' g~ = G~ (R^-1 (x) Z_1^T) vec(Y - 1_IJ mu^T - X B - Z_2 b_2);
#' G~_2 = [(Sigma_T^-1 (x) Sigma_E^-1 (x) G^-1) + (R^-1 (x) Z_2^T Z_2)]^-1,
#' g~_2 = G~_2 (R^-1 (x) Z_2^T) vec(Y - 1_IJ mu^T - X B - Z_1 b_1).  Then
#' Sigma_T ~ IW(v_T + J + IJ,
#'              b_1^T G^-1 b_1 + b_2^T (Sigma_E^-1 (x) G^-1) b_2 + S_T),
#' Sigma_E ~ IW(v_E + J L, b_2*^T (G^-1 (x) Sigma_T^-1) b_2* + S_E), and
#' R ~ IW(v_R + IJ, S_R + residual crossproduct), where b_2* is the J n_T by I
#' matrix with vec(b_2^T) = vec(b_2*).  That reshaping requires the IJ rows of
#' the data to run environment-major; the function cannot check the row order,
#' so this documentation states it instead.
#'
#' BOOK ERRATA, all four recorded.  (1) The symbol L in the Sigma_E step is
#' never defined anywhere in Section 6.9.  It must be n_T: b_2* is J n_T by I,
#' so the exponent |Sigma_E|^{-JL/2} forces L = n_T and step 6's v_E + J L is
#' v_E + J n_T.  (2) Step 7 writes the residual scale as "S_T + ..." where it
#' must be S_R; the same typo appears in the Section 6.8 sampler on p. 193.
#' (3) Steps 3 and 4 both label the dimension N_J; the correct dimensions are
#' J n_T and I J n_T.  (4) Step 7 and the p. 193 analogue write X B and X beta
#' inside the same quadratic form; they mean X B.  The corrected readings are
#' used here.
#'
#' DETERMINISM.  Nothing is sampled.  Every step is taken at the exact mean of
#' its own full conditional, the three inverse-Wishart steps at
#' E[IW(v, S)] = S/(v - d - 1) with d the order of the matrix, the mean the
#' book's own exponents imply.  Iterating those conditional means is the EM
#' fixed point of the same sampler, so both arms land on identical numbers
#' rather than on the same posterior.
#'
#' @param Y (I*J)-by-n_T matrix of phenotypes, rows running ENVIRONMENT-MAJOR:
#'   all J lines of environment 1, then all J lines of environment 2, and so on.
#' @param G J-by-J genomic relationship matrix, symmetric.
#' @param n_env I, the number of environments.
#' @param n_iter maximum number of conditional-mean sweeps.
#' @param X optional (I*J)-by-p fixed-effect design.  Do NOT include an
#'   intercept column: mu is already the intercept, and an X that carries one
#'   leaves beta unidentified.  The solve is ridged, so both arms still agree on
#'   a value, but that value means nothing.
#' @param v_T,S_T,v_E,S_E,v_R,S_R inverse-Wishart hyperparameters of steps 5,
#'   6 and 7.
#' @param tol fixed-point tolerance.
#' @return list: estimate, gebv, b1, b2, sigma_g, Sigma_T, Sigma_E, R, mu,
#'   beta, n, method.
#' @keywords internal
#' @examples
#' Y <- cbind(c(1, 2, 3, 0.5, 2.2, 1.4), c(2, 1.5, 3.5, 1, 2.4, 1.9))
#' Bmtme(Y, diag(3), 2)$estimate
#' @export
Bmtme <- function(Y, G, n_env, n_iter = 200L, X = NULL, v_T = NULL, S_T = NULL,
                  v_E = NULL, S_E = NULL, v_R = NULL, S_R = NULL, tol = 1e-12) {
  YY <- .s03mat(Y)
  N <- nrow(YY)
  if (N < 2L) stop("bmtme_model: need at least two observations")
  nT <- ncol(YY)
  if (nT < 1L) stop("bmtme_model: need at least one trait")
  GG <- .s03mat(G)
  J <- nrow(GG)
  if (ncol(GG) != J) stop("bmtme_model: G must be square")
  if (max(abs(GG - t(GG))) > 1e-12) stop("bmtme_model: G must be symmetric")
  I <- as.integer(n_env)
  if (I < 1L) stop("bmtme_model: n_env must be at least 1")
  if (I * J != N) stop("bmtme_model: Y must have n_env * nrow(G) rows")
  if (is.null(X)) {
    XX <- NULL; p <- 0L
  } else {
    XX <- .s03mat(X)
    if (nrow(XX) != N) stop("bmtme_model: X has a different number of rows than Y")
    p <- ncol(XX)
  }
  vT <- if (is.null(v_T)) nT + 2 else as.numeric(v_T)
  vE <- if (is.null(v_E)) I + 2 else as.numeric(v_E)
  vR <- if (is.null(v_R)) nT + 2 else as.numeric(v_R)
  if (vT <= nT + 1 || vR <= nT + 1) stop("bmtme_model: v_T and v_R must exceed n_T + 1")
  if (vE <= I + 1) stop("bmtme_model: v_E must exceed n_env + 1")
  ST <- if (is.null(S_T)) diag(1, nT) else .s03mat(S_T)
  SE <- if (is.null(S_E)) diag(1, I) else .s03mat(S_E)
  SR <- if (is.null(S_R)) diag(1, nT) else .s03mat(S_R)
  it <- as.integer(n_iter)
  if (it < 1L) stop("bmtme_model: n_iter must be at least 1")

  Z1 <- matrix(0, N, J)
  for (i in seq_len(N)) Z1[i, ((i - 1L) %% J) + 1L] <- 1
  Ginv <- .mvsinv(GG)
  Z1tZ1 <- crossprod(Z1)
  Z2tZ2 <- diag(1, N)
  b1 <- matrix(0, J, nT)
  b2 <- matrix(0, N, nT)
  beta <- matrix(0, max(p, 1L), nT)
  mu <- numeric(nT)
  SigT <- diag(1, nT); SigE <- diag(1, I); Rm <- diag(1, nT)
  resid <- function(drop_mu = FALSE, drop_beta = FALSE, drop_b1 = FALSE, drop_b2 = FALSE) {
    out <- YY
    if (!drop_mu) out <- out - matrix(mu, N, nT, byrow = TRUE)
    if (!drop_beta && p > 0L) out <- out - XX %*% beta[seq_len(p), , drop = FALSE]
    if (!drop_b1) out <- out - Z1 %*% b1
    if (!drop_b2) out <- out - b2
    out
  }
  for (sweep in seq_len(it)) {
    prev <- b2
    if (p > 0L) {
      Rr <- resid(drop_beta = TRUE)
      A <- crossprod(XX)
      for (t in seq_len(nT)) {
        beta[seq_len(p), t] <- .s03ridgesolve(A, as.numeric(crossprod(XX, Rr[, t])), 1e-12)
      }
    }
    Rr <- resid(drop_mu = TRUE)
    mu <- colSums(Rr) / N
    Rinv <- .mvsinv(Rm); STinv <- .mvsinv(SigT); SEinv <- .mvsinv(SigE)
    Rr <- resid(drop_b1 = TRUE)
    M <- kronecker(STinv, Ginv) + kronecker(Rinv, Z1tZ1)
    RHS <- crossprod(Z1, Rr) %*% Rinv
    b1 <- matrix(.s03ridgesolve(M, as.numeric(RHS), 1e-12), J, nT)
    Rr <- resid(drop_b2 = TRUE)
    M <- kronecker(STinv, kronecker(SEinv, Ginv)) + kronecker(Rinv, Z2tZ2)
    RHS <- Rr %*% Rinv
    b2 <- matrix(.s03ridgesolve(M, as.numeric(RHS), 1e-12), N, nT)
    SEG <- kronecker(SEinv, Ginv)
    SS <- crossprod(b1, Ginv %*% b1) + crossprod(b2, SEG %*% b2) + ST
    SigT <- SS / (vT + J + N - nT - 1)
    STinv <- .mvsinv(SigT)
    b2s <- matrix(0, J * nT, I)
    for (j in seq_len(J)) {
      for (t in seq_len(nT)) {
        for (e in seq_len(I)) b2s[(j - 1L) * nT + t, e] <- b2[(e - 1L) * J + j, t]
      }
    }
    SS <- crossprod(b2s, kronecker(Ginv, STinv) %*% b2s) + SE
    SigE <- SS / (vE + J * nT - I - 1)
    E <- resid()
    SS <- crossprod(E) + SR
    Rm <- SS / (vR + N - nT - 1)
    if (max(abs(b2 - prev)) < tol) break
  }
  gebv <- Z1 %*% b1 + b2
  list(estimate = sum(gebv) / (N * nT), gebv = gebv, b1 = b1, b2 = b2,
       sigma_g = SigT, Sigma_T = SigT, Sigma_E = SigE, R = Rm, mu = mu,
       beta = if (p > 0L) beta[seq_len(p), , drop = FALSE] else beta[0, , drop = FALSE],
       n = N,
       method = paste0("Chapter 6 eq. (6.11) BMTME, the eight-step p.196 ",
                       "sampler taken at its conditional means"))
}
