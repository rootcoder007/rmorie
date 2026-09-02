# SPDX-License-Identifier: AGPL-3.0-or-later
## Targeted counterfactual CDFs and their influence curves on a grid.
## One point-treatment TMLE per threshold on the binary outcome
## I(Y <= t), with an arm-specific clever covariate H_a = I(D = a)/g_a so
## that each arm mean -- not just the contrast -- solves its own
## efficient score. Monotonicity is restored by a running maximum, the
## cheapest rearrangement that cannot move a correctly ordered pair.
#' SPDX-License-Identifier: AGPL-3.0-or-later
#'
#' # Targeted counterfactual CDFs and their influence curves on a grid.
#' # One point-treatment TMLE per threshold on the binary outcome # I(Y
#' <= t), with an arm-specific clever covariate H_a = I(D = a)/g_a so #
#' that each arm mean -- not just the contrast -- solves its own #
#' efficient score. Monotonicity is restored by a running maximum, the #
#' cheapest rearrangement that cannot move a correctly ordered pair.
#'
#' @param yv A vector; its length is taken.
#' @param Dv Numeric; combined arithmetically in the body.
#' @param W Passed to \code{cbind}.
#' @param g Numeric; combined arithmetically in the body.
#' @param grid A vector; its length is taken and its elements indexed.
#' @return A list with \code{F}, \code{IC}.
#' @export
.tmlmpi_cdf_bank <- function(yv, Dv, W, g, grid) {
  n <- length(yv); K <- length(grid)
  Fv <- list(numeric(K), numeric(K))
  IC <- list(matrix(0, K, n), matrix(0, K, n))
  for (j in seq_len(K)) {
    z <- ifelse(yv <= grid[j], 1, 0)
    qb <- .s4_ols(cbind(Dv, W), z)$beta
    Q <- list(as.numeric(cbind(0, W) %*% qb), as.numeric(cbind(1, W) %*% qb))
    for (a in 1:2) {
      av <- a - 1L
      ga <- if (av == 1L) g else 1 - g
      H <- ifelse(abs(Dv - av) < 0.5, 1, 0) / ga
      den <- sum(H * H)
      eps <- if (den != 0) sum(H * (z - Q[[a]])) / den else 0
      Qs <- .s4_clip(Q[[a]] + eps / ga, 0, 1)
      p <- sum(Qs) / n
      Fv[[a]][j] <- p
      IC[[a]][j, ] <- H * (z - Q[[a]] - eps * H) + Qs - p
    }
  }
  for (a in 1:2) Fv[[a]] <- pmin(cummax(Fv[[a]]), 1)
  list(F = Fv, IC = IC)
}

#' TMLE for the marginal probabilistic index
#'
#' The probabilistic index is a functional of the two counterfactual
#' marginal distributions, not of a regression coefficient, so it is
#' built from a bank of threshold TMLEs: for every distinct observed
#' outcome value \code{F_a(t) = P(Y(a) <= t)} is targeted, and the index
#' is the Riemann-Stieltjes integral
#' \code{psi = sum_j [(F_0(t_{j-1}) + F_0(t_j))/2] dF_1(t_j)}, whose
#' mid-point weight is the half-credit-for-ties convention.  Because
#' \code{Y(1)} and \code{Y(0)} enter only through their marginals this is
#' the MARGINAL probabilistic index, not the within-pair one.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates.
#' @return List with \code{estimate}, \code{se}, \code{n_grid}, \code{n}.
#' @references Thas, O., De Neve, J., Clement, L. & Ottoy, J. P. (2012).
#'   JRSS B 74(4):623-671; van der Laan, M. J. & Rubin, D. (2006). IJB
#'   2(1):11.
#' @export
#' @examples
#' Tmlmpi(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlmpi <- function(y, D, X) {
  yv <- as.numeric(y); Dv <- as.numeric(D); n <- length(yv)
  if (n == 0L || length(Dv) != n)
    stop("Tmlmpi: y and D must share one length")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlmpi: X must have one row per subject")
  W <- cbind(1, Xm)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  grid <- sort(unique(yv)); K <- length(grid)
  bk <- .tmlmpi_cdf_bank(yv, Dv, W, g, grid)
  F <- bk$F; IC <- bk$IC
  psi <- 0; ic <- numeric(n)
  for (j in seq_len(K)) {
    f0p <- if (j > 1L) F[[1]][j - 1L] else 0
    f1p <- if (j > 1L) F[[2]][j - 1L] else 0
    bar <- 0.5 * (f0p + F[[1]][j])
    d1 <- F[[2]][j] - f1p
    psi <- psi + bar * d1
    prev1 <- if (j > 1L) IC[[2]][j - 1L, ] else numeric(n)
    prev0 <- if (j > 1L) IC[[1]][j - 1L, ] else numeric(n)
    ic <- ic + bar * (IC[[2]][j, ] - prev1) + 0.5 * (IC[[1]][j, ] + prev0) * d1
  }
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, n_grid = K, n = n,
             method = "TMLE for the marginal probabilistic index")
}
