# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE transporting a treatment effect to a target population
#'
#' Transport is an extrapolation in the covariate distribution, not in
#' the outcome model: the effect is identified in the target population
#' only under S-admissibility.  Relative to a subgroup analysis what
#' changes is the weight -- the source rows are reweighted by the
#' SAMPLING ODDS,
#' \code{H = I(S = 1)/P(S = 0) * (1 - p(X))/p(X) *
#' \[D/g(X) - (1 - D)/(1 - g(X))\]} with \code{p(X) = P(S = 1 | X)}.
#' Target-population outcomes are never used; only its covariates enter.
#'
#' @param y Outcome; entries with \code{S = 0} are ignored.
#' @param D Binary treatment; entries with \code{S = 0} are ignored.
#' @param X Covariates, in both populations.
#' @param S 1 for a source (trial) row, 0 for a target-population row.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{n_source}, \code{n_target}, \code{n}.
#' @references Rudolph, K. E. & van der Laan, M. J. (2017). JRSS B
#'   79(5):1509-1525.
#' @export
#' @examples
#' set.seed(1)
#' r <- Tmltrn(y = rnorm(10), D = rbinom(10, 1, 0.5), X = rnorm(10), S = rnorm(10)); TRUE
Tmltrn <- function(y, D, X, S) {
  yv <- as.numeric(y); Dv <- as.numeric(D); Sv <- as.numeric(S)
  n <- length(yv)
  if (n == 0L || length(Dv) != n || length(Sv) != n)
    stop("Tmltrn: y, D and S must share one length")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmltrn: X must have one row per subject")
  src <- which(Sv > 0.5); tgt <- which(Sv <= 0.5)
  if (length(src) < 2L || length(tgt) < 1L)
    stop("Tmltrn: need at least two source and one target row")
  W <- cbind(1, Xm)
  pb <- .s4_glmbin(W, Sv)
  p <- .s4_clip(.s4_expit(as.numeric(W %*% pb)), 0.025, 0.975)
  gb <- .s4_glmbin(W[src, , drop = FALSE], Dv[src])
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  qb <- .s4_ols(cbind(Dv, W)[src, , drop = FALSE], yv[src])$beta
  Q1 <- as.numeric(cbind(1, W) %*% qb)
  Q0 <- as.numeric(cbind(0, W) %*% qb)
  Qobs <- ifelse(Dv > 0.5, Q1, Q0)
  pt <- length(tgt) / n
  odds <- (1 - p) / p
  H <- Sv / pt * odds * (Dv / g - (1 - Dv) / (1 - g))
  den <- sum(H * H)
  eps <- if (den != 0) sum((H * (yv - Qobs))[Sv > 0.5]) / den else 0
  Q1s <- Q1 + eps * odds / (pt * g)
  Q0s <- Q0 - eps * odds / (pt * (1 - g))
  psi <- sum((Q1s - Q0s)[tgt]) / length(tgt)
  r <- ifelse(Sv > 0.5, yv - Qobs - eps * H, 0)
  ic <- H * r + (1 - Sv) / pt * (Q1 - Q0 - psi)
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps,
             n_source = length(src), n_target = length(tgt), n = n,
             method = "TMLE transporting a treatment effect to a target population")
}
