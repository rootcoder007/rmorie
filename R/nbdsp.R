# SPDX-License-Identifier: AGPL-3.0-or-later
#' Negative binomial regression for overdispersed count data
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer, read
#' as rendered pages.  Volume \[Pages 35-70\], Chapter 2, the generalised linear
#' model table on p. 40, lists the Counts / Negative binomial row with the Log
#' link and mean exp(X beta_hat), which is the link and mean used here.  Volume
#' \[Pages 379-425\], Chapter 10, Section 10.7.2, p. 401, adds that "for count
#' data the loss function can be obtained under a negative binomial
#' distribution, which can do a better job than the Poisson distribution when
#' the assumption of equal mean and variance is hard to justify".
#'
#' NOT IN THE BOOK BEYOND THAT.  All seventeen page-range volumes were
#' searched: the negative binomial appears only as a row in that table and as
#' the remark above.  The book never writes the mass function, never estimates
#' the dispersion, and its index (\[Pages 683-691\]) has no entry for it.  The
#' mass function and the moments used here are the ones the function's own
#' specification states, P(Y=k) = C(k+r-1, k) p^r (1-p)^k, E\[Y\] = mu,
#' Var\[Y\] = mu + mu^2/r, that is, the NB2 parameterisation; the dispersion is
#' estimated from the method-of-moments identity Var = mu + mu^2/r implied by
#' those same two moments, so nothing beyond that statement is assumed.  The
#' fit is Fisher scoring on the log link with the NB2 weight
#' w_i = mu_i / (1 + mu_i/r), alternated with that moment update for r.
#'
#' @param y non-negative integer counts.
#' @param X n-by-p design matrix; include an intercept column if wanted.
#' @param link only "log" is offered, the link the Chapter 2 table gives.
#' @param max_iter,tol fixed-point controls.
#' @return list: estimate, mu_hat, r_hat, beta, n, method.
#' @keywords internal
#' @examples
#' Nbdsp(c(1, 3, 2, 8, 5), cbind(1, c(-1, 0, 0.5, 1.5, 1)))$r_hat
#' @export
Nbdsp <- function(y, X, link = "log", max_iter = 100L, tol = 1e-12) {
  yy <- .s03vec(y)
  n <- length(yy)
  if (n == 0L) stop("negative_binomial_dispersion: y is empty")
  if (any(yy < 0) || any(yy != floor(yy))) {
    stop("negative_binomial_dispersion: y must be non-negative counts")
  }
  XX <- .s03mat(X)
  if (nrow(XX) != n) {
    stop("negative_binomial_dispersion: X has a different number of rows than y")
  }
  p <- ncol(XX)
  if (!identical(link, "log")) {
    stop("negative_binomial_dispersion: only the log link of the Chapter 2 table is offered")
  }
  beta <- numeric(p)
  r <- 1e6
  mu <- rep(1, n)
  for (it in seq_len(as.integer(max_iter))) {
    prev <- beta
    A <- matrix(0, p, p)
    b <- numeric(p)
    for (i in seq_len(n)) {
      eta <- 0
      for (j in seq_len(p)) eta <- eta + XX[i, j] * beta[j]
      eta <- min(max(eta, -300), 300)
      m <- exp(eta)
      mu[i] <- m
      w <- if (r > 0) m / (1 + m / r) else m
      z <- if (m > 0) eta + (yy[i] - m) / m else eta
      for (a in seq_len(p)) {
        b[a] <- b[a] + w * XX[i, a] * z
        for (cc in seq_len(p)) A[a, cc] <- A[a, cc] + w * XX[i, a] * XX[i, cc]
      }
    }
    beta <- .s03ridgesolve(A, b, 1e-12)
    s2 <- 0
    sm <- 0
    for (i in seq_len(n)) {
      eta <- 0
      for (j in seq_len(p)) eta <- eta + XX[i, j] * beta[j]
      m <- exp(min(max(eta, -300), 300))
      mu[i] <- m
      s2 <- s2 + (yy[i] - m)^2 - m
      sm <- sm + m * m
    }
    r <- if (s2 > 0) sm / s2 else Inf
    if (max(abs(beta - prev)) < tol) break
  }
  list(estimate = r, mu_hat = mu, r_hat = r, beta = beta, n = n,
       method = paste0("NB2 log-link mean (Chapter 2 GLM table) with r from ",
                       "Var = mu + mu^2/r"))
}
