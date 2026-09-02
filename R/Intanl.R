# SPDX-License-Identifier: AGPL-3.0-or-later
#' Treatment by covariate interaction in a marginal structural model
#'
#' The marginal structural mean model with an effect modifier V is
#' \code{E\[Y^a | V\] = b0 + b1 a + b2 V a + b3 V}; Hernan and Robins
#' instruct that its parameters be estimated by fitting
#' \code{E\[Y|A,V\] = t0 + t1 A + t2 V A + t3 V} by weighted least squares
#' with the IP weights.  Additive effect modification is present when
#' \code{b2} differs from zero, so \code{b2} is the reported estimate.
#' Because the weights are estimated, the robust (sandwich) standard
#' error is reported rather than the model-based one.
#'
#' Formula: E\[Y^a|V\] = b0 + b1 a + b2 V a + b3 V.
#'
#' @param y Outcome vector.
#' @param A Treatment vector.
#' @param V Effect modifier.
#' @param H IP weights; \code{NULL} gives the unweighted fit.
#' @return List with \code{estimate} (the interaction), \code{beta0},
#'   \code{beta_a}, \code{beta_av}, \code{beta_v}, \code{se},
#'   \code{se_a}, \code{se0}, \code{se_v}, \code{n}, \code{method}.
#' @references Hernan and Robins (2020), Causal Inference: What If,
#'   Chapman and Hall/CRC, section 12.5, p. 171.
#' @export
#' @examples
#' set.seed(1)
#' Intanl(y = rnorm(20), A = rbinom(20, 1, 0.5), V = rbinom(20, 1, 0.5),
#'        H = runif(20, 0.5, 1.5))
Intanl <- function(y, A, V, H) {
  yv <- .s03vec(y); n <- length(yv)
  if (n == 0L) stop("interaction_analysis: y is empty")
  a <- .s03vec(A); v <- .s03vec(V)
  if (length(a) != n || length(v) != n) stop("interaction_analysis: y, A and V have different lengths")
  w <- if (!is.null(H)) .s03vec(H) else rep(1, n)
  if (length(w) != n) stop("interaction_analysis: H and y have different lengths")
  if (any(w < 0)) stop("interaction_analysis: weights must be non-negative")
  Z <- cbind(1, a, v * a, v)
  p <- 4L
  if (n <= p) stop("interaction_analysis: need more than four observations")
  ZtWZ <- matrix(0, p, p); ZtWy <- numeric(p)
  for (i in seq_len(n)) {
    for (r in seq_len(p)) {
      ZtWy[r] <- ZtWy[r] + Z[i, r] * w[i] * yv[i]
      for (cc in seq_len(p)) ZtWZ[r, cc] <- ZtWZ[r, cc] + Z[i, r] * w[i] * Z[i, cc]
    }
  }
  beta <- .s03cholsolve(ZtWZ, ZtWy)
  e <- yv - as.numeric(.s03matvec(Z, beta))
  inv <- do.call(cbind, lapply(seq_len(p), function(j) .s03cholsolve(ZtWZ, as.numeric(seq_len(p) == j))))
  meat <- matrix(0, p, p)
  for (i in seq_len(n)) {
    k <- w[i]^2 * e[i]^2
    for (r in seq_len(p)) for (cc in seq_len(p)) meat[r, cc] <- meat[r, cc] + k * Z[i, r] * Z[i, cc]
  }
  vcv <- .s03matmul(.s03matmul(inv, meat), inv)
  se <- sqrt(diag(vcv))
  .t1_result(estimate = beta[3], beta0 = beta[1], beta_a = beta[2],
             beta_av = beta[3], beta_v = beta[4], se = se[3], se_a = se[2],
             se0 = se[1], se_v = se[4], n = n,
             method = "E[Y^a|V] = b0 + b1 a + b2 V a + b3 V by IP-weighted least squares, Hernan & Robins (2020) s.12.5")
}
