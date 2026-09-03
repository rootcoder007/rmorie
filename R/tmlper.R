# SPDX-License-Identifier: AGPL-3.0-or-later
#' Point-treatment TMLE with seasonality in both nuisance models
#'
#' Season drives the outcome and drives who gets treated. Putting a
#' Fourier basis into the propensity as well as the outcome model stops
#' calendar time from masquerading as a treatment effect; a basis rather
#' than month dummies keeps January and December neighbours.
#'
#' Formula: augment with \code{cos(2 pi j t/p), sin(2 pi j t/p)} for
#' \code{j = 1..n_fourier}, then target with
#' \code{H = D/g - (1 - D)/(1 - g)}.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates; the first column is calendar time.
#' @param period Length of one cycle.
#' @param n_fourier Harmonics.
#' @return List with \code{estimate}, \code{se}, \code{eps}, \code{n_basis}, \code{n}.
#' @references Westreich, D. & Cole, S. R. (2010). Am J Epidemiol
#'   171:674-677; van der Laan & Rubin (2006) IJB 2(1):11.
#' @export
#' @examples
#' Tmlper(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = 5L, X = c(1, 2, 3, 4, 5, 6, 7, 8), period =
#' c(1, 2, 3, 4, 5, 6, 7, 8))
Tmlper <- function(y, D, X, period, n_fourier = 2) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  Xm <- as.matrix(X)
  n <- length(yv)
  p <- as.numeric(period)
  nf <- as.integer(n_fourier)
  W <- cbind(1, Xm)
  tt <- Xm[, 1]
  for (j in seq_len(nf)) {
    W <- cbind(W, cos(2 * pi * j * tt / p), sin(2 * pi * j * tt / p))
  }
  res <- .s4_tmle(yv, Dv, W)
  .t1_result(estimate = res$psi, se = res$se, eps = res$eps,
             n_basis = 2L * nf, n = n,
             method = "TMLE with a Fourier seasonal basis")
}
