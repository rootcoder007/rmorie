# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weight candidate predictions by cross-validated risk, on the simplex.
#'
#' \code{Z} must hold CROSS-VALIDATED predictions; in-sample predictions
#' make super learner pick the most overfit candidate and nothing in the
#' arithmetic can detect it. Weights are restricted to the simplex.
#' Optimisation is Frank-Wolfe with a FIXED step schedule 2/(t + 2).
#'
#' Formula: Psi_SL(W) = sum_k alpha_k Psi_k(W),
#'   alphahat = argmin over the simplex of sum_i (Y_i - (Z alpha)_i)^2
#'
#' @param Z Cross-validated predictions, one column per candidate.
#' @param Y Outcome.
#' @param iters Frank-Wolfe steps (fixed budget).
#' @return List with \code{weights}, \code{risk}, \code{sl_risk},
#'   \code{discrete_risk}, \code{discrete_index}, \code{fitted},
#'   \code{beats_discrete}, \code{n}, \code{K}.
#' @references van der Laan, Polley & Hubbard (2007), Super Learner, UC
#'   Berkeley Division of Biostatistics Working Paper 222. That PDF could
#'   not be downloaded (the bepress endpoint returned an empty body); the
#'   construction is taken from Polley's dissertation, Super Learner, UC
#'   Berkeley (escholarship qt4qn0067v), fetched in full.
#' @export
Superlrn <- function(Z, Y, iters = 500) {
  Z <- as.matrix(Z); n <- nrow(Z); K <- ncol(Z)
  Y <- .t1_vec(Y)
  if (n < 2L) stop("at least two observations are required")
  if (length(Y) != n) stop("Y must have one entry per observation")
  if (K < 1L) stop("at least one candidate is required")
  risk <- colSums((Y - Z)^2) / n
  dbest <- which.min(risk)
  a <- rep(0, K); a[dbest] <- 1
  for (t in seq_len(as.integer(iters))) {
    f <- as.numeric(Z %*% a)
    gr <- -2 / n * as.numeric(t(Z) %*% (Y - f))
    v <- which.min(gr)
    g <- 2 / (t + 1)
    a <- (1 - g) * a
    a[v] <- a[v] + g
  }
  a <- a / sum(a)
  fit <- as.numeric(Z %*% a)
  slr <- sum((Y - fit)^2) / n
  .t1_result(weights = a, risk = risk, sl_risk = slr,
             discrete_risk = risk[dbest],
             discrete_index = as.numeric(dbest), fitted = fit,
             beats_discrete = as.numeric(slr <= risk[dbest]),
             n = as.numeric(n), K = as.numeric(K),
             method = "Super learner: simplex-constrained ensemble of candidates")
}
