# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean-field variational inference by coordinate ascent on the ELBO
#'
#' SOURCE. Jordan, M.I., Ghahramani, Z., Jaakkola, T.S. and Saul, L.K.
#' (1999), "An Introduction to Variational Methods for Graphical Models",
#' Machine Learning 37(2):183-233, doi:10.1023/A:1007665907178.
#'
#' The mean-field family factorises q(z) = prod_j q_j(z_j), and Section 3
#' gives the coordinate update that maximises the lower bound with the
#' other factors fixed: log q*_j(z_j) = E_{q_{-j}}[log p(x, z)] + const.
#' The bound is ELBO(q) = E_q[log p(x,z)] - E_q[log q(z)] <= log p(x), and
#' no coordinate update may decrease it -- asserted here as
#' \code{elbo_monotone} rather than assumed.
#'
#' MODEL. The general update is not executable without a joint, so the
#' joint is the conjugate univariate Normal-Gamma: x_n | mu, tau ~
#' N(mu, 1/tau), mu | tau ~ N(mu0, 1/(lambda0 tau)), tau ~ Gamma(a0, b0),
#' with q(mu, tau) = q(mu) q(tau). The updates are closed form:
#' mu_N = (lambda0 mu0 + N xbar)/(lambda0 + N), lambda_N = (lambda0 + N)
#' E[tau], a_N = a0 + (N+1)/2, b_N = b0 + E_mu[sum (x_n - mu)^2 +
#' lambda0 (mu - mu0)^2]/2, E[tau] = a_N/b_N.
#'
#' Only this joint is implemented; that is this implementation's scope
#' choice, stated rather than attributed -- a general log_p callable
#' cannot cross the Python/R boundary and would make the arms untestable.
#'
#' ANCHOR. In the improper limit lambda0 = a0 = b0 = 0, mu_N = xbar
#' exactly and the fixed point solves t = (N+1)/(S + 1/t) with
#' S = sum (x_n - xbar)^2, giving t = N/S, the maximum likelihood
#' precision.
#'
#' @param log_p Name of the joint; only "normal-gamma" is implemented.
#' @param q_family Variational family; only "meanfield" is implemented.
#' @param x Observed sample.
#' @param mu0,lambda0,a0,b0 Normal-Gamma prior hyperparameters.
#' @param max_iter Maximum coordinate sweeps.
#' @param tol Stop when E[tau] moves by less than this.
#' @return List with \code{mu_n}, \code{lambda_n}, \code{a_n}, \code{b_n},
#'   \code{e_tau}, \code{e_mu}, \code{var_mu}, \code{elbo},
#'   \code{elbo_path}, \code{elbo_monotone}, \code{iterations},
#'   \code{converged}, \code{n}.
#' @references Jordan, M.I., Ghahramani, Z., Jaakkola, T.S. and Saul, L.K.
#'   (1999). Machine Learning 37(2):183-233. doi:10.1023/A:1007665907178.
#' @examples
#' Vinfer("normal-gamma", "meanfield", c(1, 2, 3, 4, 5))$e_tau
#' @export
Vinfer <- function(log_p = "normal-gamma", q_family = "meanfield", x = NULL,
                   mu0 = 0, lambda0 = 0, a0 = 0, b0 = 0,
                   max_iter = 200, tol = 1e-12) {
  if (!(tolower(trimws(as.character(log_p)[1L])) %in%
        c("normal-gamma", "gaussian-gamma", "normalgamma"))) {
    stop("variational_inference: only the normal-gamma joint is implemented")
  }
  if (!(tolower(trimws(as.character(q_family)[1L])) %in% c("meanfield", "mean-field"))) {
    stop("variational_inference: only the mean-field family is implemented")
  }
  xv <- .s03vec(x)
  n <- length(xv)
  if (n < 2L) stop("variational_inference: need at least two observations")
  if (lambda0 < 0) stop("variational_inference: lambda0 must be non-negative")
  if (a0 < 0) stop("variational_inference: a0 must be non-negative")
  if (b0 < 0) stop("variational_inference: b0 must be non-negative")
  if (tol <= 0) stop("variational_inference: tol must be positive")
  xbar <- 0
  for (v in xv) xbar <- xbar + v
  xbar <- xbar / n
  ss <- 0
  for (v in xv) ss <- ss + (v - xbar) * (v - xbar)
  mu_n <- (lambda0 * mu0 + n * xbar) / (lambda0 + n)
  a_n <- a0 + 0.5 * (n + 1)
  e_tau <- 1
  lam_n <- (lambda0 + n) * e_tau
  b_n <- b0
  path <- numeric(0)
  it <- 0L
  converged <- FALSE
  for (k in seq_len(as.integer(max_iter))) {
    it <- k
    lam_n <- (lambda0 + n) * e_tau
    var_mu <- 1 / lam_n
    quad <- ss + n * (xbar - mu_n) * (xbar - mu_n) + n * var_mu
    quad <- quad + lambda0 * ((mu_n - mu0) * (mu_n - mu0) + var_mu)
    b_n <- b0 + 0.5 * quad
    new_tau <- a_n / b_n
    e_log_tau <- .s03digamma(a_n) - log(b_n)
    elbo <- 0.5 * n * e_log_tau - 0.5 * new_tau * quad
    elbo <- elbo + 0.5 * e_log_tau - 0.5 * log(2 * pi)
    elbo <- elbo + (a0 - 1) * e_log_tau - b0 * new_tau
    elbo <- elbo - (-0.5 * log(2 * pi * var_mu) - 0.5)
    elbo <- elbo - (a_n * log(b_n) - .s03lgamma(a_n) + (a_n - 1) * e_log_tau - a_n)
    path <- c(path, elbo)
    if (abs(new_tau - e_tau) < tol) {
      e_tau <- new_tau
      converged <- TRUE
      break
    }
    e_tau <- new_tau
  }
  lam_n <- (lambda0 + n) * e_tau
  var_mu <- 1 / lam_n
  mono <- TRUE
  if (length(path) > 1L) {
    for (i in seq(2L, length(path))) if (path[i] < path[i - 1L] - 1e-10) mono <- FALSE
  }
  .t1_result(estimate = e_tau, mu_n = mu_n, lambda_n = lam_n, a_n = a_n,
             b_n = b_n, e_tau = e_tau, e_mu = mu_n, var_mu = var_mu,
             elbo = path[length(path)], elbo_path = path,
             elbo_monotone = if (mono) 1 else 0, iterations = it,
             converged = if (converged) 1 else 0, n = n,
             method = paste("Coordinate-ascent mean-field VI, Normal-Gamma",
                            "joint (Jordan et al. 1999 Sec. 3)"))
}
