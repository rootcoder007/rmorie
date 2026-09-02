# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pool site-level TMLEs without moving any row between sites
#'
#' Each site fits its own nuisance models and targets locally; only the
#' site estimate and its influence-curve variance leave. The pooled
#' variance still comes out right because influence curves add.
#'
#' Formula: \code{psi = sum_s w_s psi_s / sum_s w_s} with
#' \code{w_s = n_s / var(IC_s)}, \code{var = 1 / sum_s w_s}.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates.
#' @param site Site label.
#' @return List with \code{estimate}, \code{se}, \code{site_psi},
#'   \code{site_n}, \code{n_sites}, \code{n}.
#' @references Vo, van der Laan & Petersen (2023), federated targeted
#'   learning; the pooling rule is the standard influence-curve-weighted
#'   combination.
#' @export
Tmlfed <- function(y, D, X, site) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  Xm <- as.matrix(X)
  sv <- as.integer(round(as.numeric(site)))
  labs <- unique(sv)
  psis <- numeric(0)
  ws <- numeric(0)
  ns <- numeric(0)
  for (lab in labs) {
    idx <- which(sv == lab)
    W <- cbind(1, Xm[idx, , drop = FALSE])
    r <- .s4_tmle(yv[idx], Dv[idx], W)
    v <- sum((r$ic - mean(r$ic))^2) / (length(idx) - 1)
    psis <- c(psis, r$psi)
    ns <- c(ns, length(idx))
    ws <- c(ws, if (v > 0) length(idx) / v else 0)
  }
  sw <- sum(ws)
  psi <- if (sw > 0) sum(ws * psis) / sw else NaN
  .t1_result(estimate = psi, se = if (sw > 0) sqrt(1 / sw) else NaN,
             site_psi = psis, site_n = ns, n_sites = length(labs),
             n = length(yv),
             method = "Federated TMLE, influence-curve-weighted pooling")
}
