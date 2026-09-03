# SPDX-License-Identifier: AGPL-3.0-or-later

#' EM step (single iteration) for random-effects variance
#'
#' Formula: for the random-intercept model y_ij = x_ij' b + u_j + e_ij
#' with u_j ~ N(0, sigma2_u) and e_ij ~ N(0, sigma2_e), the E-step
#' conditional moments are closed form,
#' \preformatted{
#'   uhat_j     = sigma2_u sum_i r_ij / (sigma2_e + n_j sigma2_u)
#'   var(u_j|y) = sigma2_u sigma2_e / (sigma2_e + n_j sigma2_u)
#' }
#' with r_ij = y_ij - x_ij' b, and the M-step is
#' sigma2_u' = mean_j (uhat_j^2 + var(u_j|y)) and
#' sigma2_e' = (sum_ij (r_ij - uhat_j)^2 + sum_j n_j var(u_j|y)) / N.
#'
#' b is the GLS estimate at the current variances, formed cluster-wise
#' from the Sherman-Morrison inverse
#' V_j^-1 = (I - sigma2_u J_j / (sigma2_e + n_j sigma2_u)) / sigma2_e,
#' or supplied fixed via \code{beta}, in which case sigma2_e = 0 is
#' admissible and the update reduces to its exact limit.
#'
#' @param y Response, length N.
#' @param X N x p fixed-effects design.
#' @param cluster Length-N cluster labels.
#' @param sigma2_u Current between-cluster variance (>= 0).
#' @param sigma2_e Current residual variance (> 0, or >= 0 with beta).
#' @param beta Optional fixed-effect vector held fixed.
#' @return List with \code{estimate}, \code{sigma2_u}, \code{sigma2_e},
#'   \code{beta}, \code{u_hat}, \code{var_u}, \code{J}, \code{n},
#'   \code{method}.
#' @references Dempster, Laird & Rubin (1977), JRSS-B 39(1):1-38;
#'   Laird & Ware (1982), Biometrics 38(4):963-974,
#'   doi:10.2307/2529876.
#' @export
#' @examples
#' set.seed(1)
#' r <- Emaxr(y = rnorm(10), X = rnorm(10), cluster = rnorm(10), sigma2_u = 0.5, sigma2_e
#' = 0.5); TRUE
Emaxr <- function(y, X, cluster, sigma2_u, sigma2_e, beta = NULL) {
  y <- as.numeric(y)
  N <- length(y)
  if (N == 0L) stop("empty input: y has no observations")
  Xm <- .s03mat(X)
  if (nrow(Xm) != N) stop("X must have one row per observation")
  p <- ncol(Xm)
  cluster <- as.character(cluster)
  if (length(cluster) != N) stop("cluster must have one label per observation")
  s2u <- as.numeric(sigma2_u)
  s2e <- as.numeric(sigma2_e)
  if (s2u < 0) stop("sigma2_u must be non-negative")
  if (s2e < 0 || (s2e == 0 && is.null(beta))) stop("sigma2_e must be positive")
  labs <- unique(cluster)
  J <- length(labs)
  grp <- lapply(labs, function(cc) which(cluster == cc))
  if (is.null(beta)) {
    A <- matrix(0, p, p)
    b <- numeric(p)
    for (g in grp) {
      nj <- length(g)
      f <- s2u / (s2e + nj * s2u)
      sx <- vapply(seq_len(p), function(a) sum(Xm[g, a]), 0)
      sy <- sum(y[g])
      for (a in seq_len(p)) {
        for (cc in seq_len(p))
          A[a, cc] <- A[a, cc] + (sum(Xm[g, a] * Xm[g, cc]) - f * sx[a] * sx[cc]) / s2e
        b[a] <- b[a] + (sum(Xm[g, a] * y[g]) - f * sx[a] * sy) / s2e
      }
    }
    bet <- .s03cholsolve(A, b)
  } else {
    bet <- as.numeric(beta)
    if (length(bet) != p) stop("beta must have one entry per column of X")
  }
  r <- y - as.numeric(Xm %*% bet)
  uh <- numeric(J)
  vu <- numeric(J)
  for (j in seq_len(J)) {
    g <- grp[[j]]
    nj <- length(g)
    den <- s2e + nj * s2u
    sr <- sum(r[g])
    uh[j] <- if (den > 0) s2u * sr / den else 0
    vu[j] <- if (den > 0) s2u * s2e / den else 0
  }
  new_u <- sum(uh * uh + vu) / J
  tot <- 0
  for (j in seq_len(J)) {
    g <- grp[[j]]
    tot <- tot + sum((r[g] - uh[j])^2) + length(g) * vu[j]
  }
  new_e <- tot / N
  .t1_result(estimate = new_u, sigma2_u = new_u, sigma2_e = new_e, beta = bet,
             u_hat = uh, var_u = vu, J = J, n = N,
             method = "EM step (single iteration) for random-effects variance")
}
