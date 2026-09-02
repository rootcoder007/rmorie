# SPDX-License-Identifier: AGPL-3.0-or-later
#' Albert-Chib data augmentation for binary ordinal Gibbs sampler
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume \[Pages 209-249\], Chapter 7, Section 7.2 "Bayesian Ordinal Regression
#' Model", equation (7.2) on p. 214 and the numbered Gibbs samplers on pp. 212
#' and 214, all read as rendered page images.  The chapter attributes the
#' scheme to Albert, J. H. and Chib, S. (1993), Bayesian analysis of binary
#' and polychotomous response data, Journal of the American Statistical
#' Association 88(422), 669-679, which its reference list carries.
#'
#' Equation (7.2) is p_ic = P(Y_i = c) = Phi(gamma_c + b_i) -
#' Phi(gamma_{c-1} + b_i), c = 1, ..., C, "where now L_i = - b_i + epsilon_i
#' is the latent variable".  Step 2 of the sampler is: "For each i = 1, ..., n,
#' simulate l_i from the normal distribution N(-x_i^T beta, 1) truncated in
#' (gamma_{y_i - 1}, gamma_{y_i})", and step 3 draws b from N_n(b~, Sigma~_b)
#' with Sigma~_b = (sigma_g^{-2} G^{-1} + I_n)^{-1} and b~ = - Sigma~_b l.
#'
#' SIGN CONVENTION, stated because the book's minus signs are load-bearing and
#' the PDF text layer drops them.  The book's latent has mean MINUS the linear
#' predictor, and its b~ and its beta~ = - sigma~^2_{beta_j} (x_j^T e_j) carry
#' the matching minus.  This implementation works with l* = -l, the latent on
#' the positive orientation, so l* has mean +eta and step 3 becomes
#' b~ = Sigma~_b l*.  The two are algebraically the same model and the
#' returned beta and b are on the book's own scale; only z_samples and
#' estimate, which are l*, are the negatives of the book's l.
#'
#' A third erratum in this book, recorded here.  Page 214 writes "L_i = - b_i +
#' epsilon_i is the latent variable" and then, in the very next sentence, "in
#' matrix form the model for the latent variable can be specified as
#' L = b + epsilon".  Those two contradict each other; the elementwise form is
#' the one consistent with equation (7.2), with b~ = - Sigma~_b l, and with the
#' beta step on p. 212, so the matrix line is the misprint.
#'
#' DETERMINISM.  Steps 2 and 3 are not simulated.  The latent is set to the
#' exact mean of its truncated normal -- for the binary thresholds
#' (-Inf, 0, +Inf) that is eta_i + phi(eta_i)/Phi(eta_i) when y_i = 1 and
#' eta_i - phi(eta_i)/(1 - Phi(eta_i)) when y_i = 0 -- and step 3 is replaced
#' by its conditional mean.  Iterating those two exact conditional means is the
#' EM fixed point of the same augmentation, so both arms land on identical
#' numbers rather than on the same posterior.
#'
#' @param y_bin binary response, entries 0 or 1.
#' @param X n-by-p fixed-effects design matrix.
#' @param Z optional n-by-q design for the shrunken effects; the identity when
#'   absent, which is the chapter's b = (b_1, ..., b_n) parameterisation with
#'   G = I_n.
#' @param sigma_g2 the chapter's sigma_g^2, the prior variance of those effects.
#' @param max_iter,tol fixed-point controls.
#' @return list: estimate, z_samples, beta_samples, b, n, method.
#' @keywords internal
#' @examples
#' Augmn(c(1, 0, 1, 0), cbind(1, c(-1, 0.5, 1.2, -0.3)))$estimate
#' @export
Augmn <- function(y_bin, X, Z = NULL, sigma_g2 = 1, max_iter = 200L, tol = 1e-13) {
  yy <- .s03vec(y_bin)
  n <- length(yy)
  if (n == 0L) stop("albert_chib_augmentation: y_bin is empty")
  if (any(yy != 0 & yy != 1)) stop("albert_chib_augmentation: y_bin must be 0 or 1")
  XX <- .s03mat(X)
  if (nrow(XX) != n) {
    stop("albert_chib_augmentation: X has a different number of rows than y_bin")
  }
  p <- ncol(XX)
  if (is.null(Z)) {
    ZZ <- diag(1, n)
  } else {
    ZZ <- .s03mat(Z)
    if (nrow(ZZ) != n) {
      stop("albert_chib_augmentation: Z has a different number of rows than y_bin")
    }
  }
  q <- ncol(ZZ)
  s2 <- as.numeric(sigma_g2)
  if (s2 <= 0) stop("albert_chib_augmentation: sigma_g2 must be positive")
  beta <- numeric(p)
  b <- numeric(q)
  lat <- numeric(n)
  inv2pi <- 1 / sqrt(2 * pi)
  for (it in seq_len(as.integer(max_iter))) {
    prev <- lat
    for (i in seq_len(n)) {
      eta <- 0
      for (j in seq_len(p)) eta <- eta + XX[i, j] * beta[j]
      for (j in seq_len(q)) eta <- eta + ZZ[i, j] * b[j]
      d <- inv2pi * exp(-0.5 * eta * eta)
      P <- .s03pnorm(eta)
      if (yy[i] == 1) {
        lat[i] <- eta + (if (P > 1e-300) d / P else -eta)
      } else {
        Q <- 1 - P
        lat[i] <- eta - (if (Q > 1e-300) d / Q else eta)
      }
    }
    M <- matrix(0, p + q, p + q)
    r <- numeric(p + q)
    for (i in seq_len(n)) {
      row <- c(XX[i, ], ZZ[i, ])
      for (a in seq_len(p + q)) {
        r[a] <- r[a] + row[a] * lat[i]
        for (cc in seq_len(p + q)) M[a, cc] <- M[a, cc] + row[a] * row[cc]
      }
    }
    for (a in seq(p + 1L, p + q)) M[a, a] <- M[a, a] + 1 / s2
    sol <- .s03ridgesolve(M, r, 1e-12)
    beta <- sol[seq_len(p)]
    b <- sol[seq(p + 1L, p + q)]
    if (max(abs(lat - prev)) < tol) break
  }
  list(estimate = sum(lat) / n, z_samples = lat, beta_samples = beta, b = b,
       n = n,
       method = paste0("Chapter 7 eq. (7.2) augmentation, steps 2-3 taken at ",
                       "their exact conditional means"))
}
