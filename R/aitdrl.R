# SPDX-License-Identifier: AGPL-3.0-or-later
#' Log-likelihood of a Dirichlet sample
#'
#' For N independent compositions on the open (D-1)-simplex,
#' l(alpha | X) = N\[lnGamma(sum alpha_i) - sum lnGamma(alpha_i)\]
#' + sum_i (alpha_i - 1) sum_n ln x_ni, the log of the product of Dirichlet
#' densities with the sufficient statistic sum_n ln x_ni factored out.  The
#' stub cites Wilks (1962), Mathematical Statistics, Wiley; that text was not
#' retrievable here, so the expression is the standard published form and is
#' pinned by closed forms instead: at alpha = (1, ..., 1) it reduces to
#' N lnGamma(D) = N ln (D-1)! for every data set, and at D = 2 it is the beta
#' log-likelihood.  The score is returned too, since it costs one digamma call
#' per part and makes the maximum-likelihood condition checkable,
#' dl/dalpha_i = N\[psi(sum alpha) - psi(alpha_i)\] + sum_n ln x_ni.
#'
#' @param alpha strictly positive concentration parameters.
#' @param X one composition, or a matrix whose N rows are compositions.
#' @return list: ll, estimate, score, score_max_abs, sum_log_x, log_const, N,
#'   D, method.
#' @keywords internal
#' @examples
#' Aitdrl(c(2, 3, 4), rbind(c(0.2, 0.3, 0.5), c(0.1, 0.6, 0.3)))$ll
#' @export
Aitdrl <- function(alpha, X) {
  aa <- as.numeric(.s03vec(alpha))
  D <- length(aa)
  if (D < 2L) stop("dirichlet_loglik: a composition needs at least 2 parts")
  if (any(!(aa > 0))) stop("dirichlet_loglik: alpha must be strictly positive")
  rows <- if (is.matrix(X) || is.data.frame(X)) as.matrix(X) else matrix(as.numeric(X), nrow = 1L)
  storage.mode(rows) <- "double"
  N <- nrow(rows)
  if (N == 0L || length(rows) == 0L) stop("dirichlet_loglik: X is empty")
  if (ncol(rows) != D) stop("dirichlet_loglik: a row of X has a length alpha does not match")
  slx <- numeric(D)
  for (n in seq_len(N)) {
    r <- rows[n, ]
    s <- 0
    for (v in r) {
      if (!(v > 0)) stop("dirichlet_loglik: X must lie strictly inside the simplex")
      s <- s + v
    }
    if (abs(s - 1) > 1e-8) stop("dirichlet_loglik: a row of X does not sum to one")
    for (i in seq_len(D)) slx[i] <- slx[i] + log(r[i])
  }
  a0 <- 0
  for (v in aa) a0 <- a0 + v
  lc <- lgamma(a0)
  for (v in aa) lc <- lc - lgamma(v)
  ll <- N * lc
  for (i in seq_len(D)) ll <- ll + (aa[i] - 1) * slx[i]
  d0 <- .s03digamma(a0)
  score <- numeric(D)
  for (i in seq_len(D)) score[i] <- N * (d0 - .s03digamma(aa[i])) + slx[i]
  gmax <- 0
  for (v in score) if (abs(v) > gmax) gmax <- abs(v)
  list(
    ll = ll, estimate = ll, score = score, score_max_abs = gmax, sum_log_x = slx,
    log_const = lc, N = N, D = D,
    method = "l = N[lnG(sum a) - sum lnG(a_i)] + sum_i (a_i-1) sum_n ln x_ni"
  )
}
