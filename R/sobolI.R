# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sobol global sensitivity indices
#'
#' Sobol (1993), Sensitivity estimates for nonlinear mathematical models,
#' Mathematical Modelling and Computational Experiments 1(4), 407-414, for
#' the decomposition and S_i = V_i / V; Saltelli et al. (2010), Variance
#' based sensitivity analysis of model output, Computer Physics
#' Communications 181(2), 259-270, for the estimators V_i = mean_j f(B)_j
#' (f(A_B^i)_j - f(A)_j) and V_T,i = mean_j (f(A)_j - f(A_B^i)_j)^2 / 2.
#' Neither was retrievable here as a full text; both are quoted in their
#' standard published form.
#'
#' Determinism: A and B are low-discrepancy van der Corput points in
#' distinct prime bases -- the quasi-Monte Carlo design the method is
#' built for -- not pseudo-random draws.
#'
#' @param model function of a length-d vector on the unit cube.
#' @param input_dist optional per-dimension inverse CDFs.
#' @param N base sample size.
#' @param d input dimension.
#' @return list: estimate, S, ST, V, n, method.
#' @keywords internal
#' @examples
#' Sobolidx(function(x) x[1] + 2 * x[2], NULL, 32, 2)$S
#' @export
Sobolidx <- function(model, input_dist = NULL, N = 64, d = NULL) {
  primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)
  dd <- if (!is.null(d)) as.integer(d) else if (!is.null(input_dist)) length(input_dist) else 2L
  n <- as.integer(N)
  A <- matrix(0, n, dd); B <- matrix(0, n, dd)
  # A and B must be INDEPENDENT samples.  Continuing one low-discrepancy
  # sequence gives points that are not: with base 2 and n a power of two,
  # vdc(j + n) and vdc(j) share their leading bits, the estimator's cross
  # terms stop cancelling, and S_i comes out badly wrong.  A and B
  # therefore use disjoint prime bases.
  for (j in seq_len(n)) for (a in seq_len(dd)) {
    A[j, a] <- .s03vdc(j - 1L, primes[a])
    B[j, a] <- .s03vdc(j - 1L, primes[dd + a])
  }
  tf <- function(row) {
    if (is.null(input_dist)) return(as.numeric(row))
    vapply(seq_len(dd), function(a) input_dist[[a]](row[a]), 0)
  }
  fA <- numeric(n); fB <- numeric(n)
  for (j in seq_len(n)) { fA[j] <- as.numeric(model(tf(A[j, ]))); fB[j] <- as.numeric(model(tf(B[j, ]))) }
  V <- .s03var(c(fA, fB), 1L)
  S <- numeric(dd); ST <- numeric(dd)
  for (i in seq_len(dd)) {
    AB <- A
    AB[, i] <- B[, i]
    fAB <- numeric(n)
    for (j in seq_len(n)) fAB[j] <- as.numeric(model(tf(AB[j, ])))
    vi <- 0; vti <- 0
    for (j in seq_len(n)) {
      vi <- vi + fB[j] * (fAB[j] - fA[j]) / n
      vti <- vti + (fA[j] - fAB[j])^2 / (2 * n)
    }
    S[i] <- if (V > 0) vi / V else NaN
    ST[i] <- if (V > 0) vti / V else NaN
  }
  list(estimate = if (dd) S[1] else NaN, S = S, ST = ST, V = V, n = n,
       method = "Saltelli et al. (2010) estimators of the Sobol (1993) indices, on a quasi-Monte Carlo design")
}
