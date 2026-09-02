# SPDX-License-Identifier: AGPL-3.0-or-later
#' Variational Bayes for a Dirichlet process mixture (truncated stick-breaking)
#'
#' SOURCE. Blei, D.M. and Jordan, M.I. (2006), "Variational inference for
#' Dirichlet process mixtures", Bayesian Analysis 1(1):121-143,
#' doi:10.1214/06-BA104.
#'
#' The DP mixture is written in Sethuraman stick-breaking form,
#' V_t ~ Beta(1, alpha), pi_t(V) = V_t prod_{i<t} (1 - V_i), z_n ~ pi,
#' y_n | z_n ~ p(. | eta_{z_n}); the variational family is the TRUNCATED
#' mean-field family of Section 4, q(V, eta, z) = prod_{t<K} q(V_t)
#' prod_{t<=K} q(eta_t) prod_n q(z_n), with V_K set to 1. The truncation
#' is on the variational distribution, not the model.
#'
#' Coordinate updates (Eqs. 18-21): gamma_{t,1} = 1 + sum_n phi_{n,t};
#' gamma_{t,2} = alpha + sum_n sum_{j>t} phi_{n,j}; phi_{n,t} proportional
#' to exp(E\[log V_t\] + sum_{i<t} E\[log(1-V_i)\] + E_q\[log p(y_n|eta_t)\]),
#' with E\[log V_t\] = psi(g1) - psi(g1+g2) and E\[log(1-V_t)\] = psi(g2) -
#' psi(g1+g2).
#'
#' COMPONENT MODEL. Univariate Gaussian with KNOWN variance sigma2 and a
#' conjugate normal base measure N(m0, s0^2), so q(eta_t) = N(m_t, s_t^2)
#' is closed form: 1/s_t^2 = 1/s0^2 + N_t/sigma2, m_t = (m0/s0^2 +
#' sum_n phi_{n,t} y_n / sigma2) s_t^2, and E_q\[log p(y|eta_t)\] =
#' -log(2 pi sigma2)/2 - ((y - m_t)^2 + s_t^2)/(2 sigma2). An
#' unknown-variance component would need a Normal-Gamma base measure and
#' is not implemented -- this implementation's scope choice.
#'
#' INITIALISATION is deterministic (component means at the K type-7
#' quantiles at levels (t - 1/2)/K), because a random start would put the
#' two language arms on different local optima.
#'
#' @param y Observations.
#' @param K_truncate Truncation level, at least 1.
#' @param alpha DP concentration, positive.
#' @param sigma2 Known component variance, positive.
#' @param m0,s0 Base measure N(m0, s0^2); s0 positive.
#' @param max_iter Maximum coordinate sweeps.
#' @param tol Stop when the ELBO moves by less than this.
#' @return List with \code{phi}, \code{m}, \code{s2}, \code{gamma1},
#'   \code{gamma2}, \code{weights}, \code{elbo}, \code{elbo_path},
#'   \code{elbo_monotone}, \code{iterations}, \code{converged}, \code{n},
#'   \code{k}.
#' @references Blei, D.M. and Jordan, M.I. (2006). Bayesian Analysis
#'   1(1):121-143. doi:10.1214/06-BA104.
#' @examples
#' Vbnpc(c(-2, -1.8, 2, 2.2), 2)$weights
#' @export
Vbnpc <- function(y, K_truncate = 5, alpha = 1, sigma2 = 1, m0 = 0, s0 = 10,
                  max_iter = 100, tol = 1e-10) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("vb_nonparametric: y is empty")
  K <- as.integer(K_truncate)
  if (is.na(K) || K < 1L) stop("vb_nonparametric: K_truncate must be at least 1")
  if (alpha <= 0) stop("vb_nonparametric: alpha must be positive")
  if (sigma2 <= 0) stop("vb_nonparametric: sigma2 must be positive")
  if (s0 <= 0) stop("vb_nonparametric: s0 must be positive")
  if (tol <= 0) stop("vb_nonparametric: tol must be positive")
  lbeta2 <- function(a, b) .s03lgamma(a) + .s03lgamma(b) - .s03lgamma(a + b)
  p0 <- 1 / (s0 * s0)
  m <- numeric(K)
  for (t in seq_len(K)) m[t] <- .s03quantile7(yv, (t - 0.5) / K)
  s2 <- rep(1 / p0, K)
  g1 <- rep(1, K)
  g2 <- rep(alpha, K)
  phi <- matrix(1 / K, n, K)
  path <- numeric(0)
  it <- 0L
  converged <- FALSE
  for (k0 in seq_len(as.integer(max_iter))) {
    it <- k0
    elv <- numeric(K); el1v <- numeric(K)
    for (t in seq_len(K)) {
      dg <- .s03digamma(g1[t] + g2[t])
      elv[t] <- .s03digamma(g1[t]) - dg
      el1v[t] <- .s03digamma(g2[t]) - dg
    }
    elv[K] <- 0; el1v[K] <- 0
    cum <- numeric(K); acc <- 0
    for (t in seq_len(K)) { cum[t] <- acc; acc <- acc + el1v[t] }
    for (i in seq_len(n)) {
      sc <- numeric(K); best <- NA_real_
      for (t in seq_len(K)) {
        lp <- -0.5 * log(2 * pi * sigma2) -
          ((yv[i] - m[t])^2 + s2[t]) / (2 * sigma2)
        sc[t] <- elv[t] + cum[t] + lp
        if (is.na(best) || sc[t] > best) best <- sc[t]
      }
      tot <- 0
      for (t in seq_len(K)) { sc[t] <- exp(sc[t] - best); tot <- tot + sc[t] }
      for (t in seq_len(K)) phi[i, t] <- sc[t] / tot
    }
    Nt <- numeric(K); Sy <- numeric(K)
    for (t in seq_len(K)) {
      a <- 0; b <- 0
      for (i in seq_len(n)) { a <- a + phi[i, t]; b <- b + phi[i, t] * yv[i] }
      Nt[t] <- a; Sy[t] <- b
    }
    tail <- 0; gt <- numeric(K)
    for (t in seq(K, 1L)) { gt[t] <- tail; tail <- tail + Nt[t] }
    for (t in seq_len(K)) { g1[t] <- 1 + Nt[t]; g2[t] <- alpha + gt[t] }
    for (t in seq_len(K)) {
      prec <- p0 + Nt[t] / sigma2
      s2[t] <- 1 / prec
      m[t] <- (m0 * p0 + Sy[t] / sigma2) * s2[t]
    }
    for (t in seq_len(K)) {
      dg <- .s03digamma(g1[t] + g2[t])
      elv[t] <- .s03digamma(g1[t]) - dg
      el1v[t] <- .s03digamma(g2[t]) - dg
    }
    elv[K] <- 0; el1v[K] <- 0
    cum <- numeric(K); acc <- 0
    for (t in seq_len(K)) { cum[t] <- acc; acc <- acc + el1v[t] }
    elbo <- 0
    if (K > 1L) for (t in seq_len(K - 1L)) {
      elbo <- elbo + log(alpha) + (alpha - 1) * el1v[t]
      elbo <- elbo - (-lbeta2(g1[t], g2[t]) + (g1[t] - 1) * elv[t] +
                        (g2[t] - 1) * el1v[t])
    }
    for (t in seq_len(K)) {
      elbo <- elbo + (-0.5 * log(2 * pi * s0 * s0) -
                        ((m[t] - m0)^2 + s2[t]) / (2 * s0 * s0))
      elbo <- elbo + 0.5 * (log(2 * pi * s2[t]) + 1)
    }
    for (i in seq_len(n)) for (t in seq_len(K)) {
      p <- phi[i, t]
      if (p > 0) {
        lp <- -0.5 * log(2 * pi * sigma2) -
          ((yv[i] - m[t])^2 + s2[t]) / (2 * sigma2)
        elbo <- elbo + p * (elv[t] + cum[t] + lp - log(p))
      }
    }
    path <- c(path, elbo)
    if (length(path) > 1L && abs(path[length(path)] - path[length(path) - 1L]) < tol) {
      converged <- TRUE
      break
    }
  }
  ev <- g1 / (g1 + g2)
  ev[K] <- 1
  w <- numeric(K); rem <- 1
  for (t in seq_len(K)) { w[t] <- ev[t] * rem; rem <- rem * (1 - ev[t]) }
  mono <- TRUE
  if (length(path) > 1L) {
    for (i in seq(2L, length(path))) if (path[i] < path[i - 1L] - 1e-8) mono <- FALSE
  }
  .t1_result(estimate = path[length(path)], phi = phi, m = m, s2 = s2,
             gamma1 = g1, gamma2 = g2, weights = w,
             elbo = path[length(path)], elbo_path = path,
             elbo_monotone = if (mono) 1 else 0, iterations = it,
             converged = if (converged) 1 else 0, n = n, k = K,
             method = paste("Truncated stick-breaking mean-field VB for a DP",
                            "mixture (Blei and Jordan 2006 Sec. 4)"))
}
