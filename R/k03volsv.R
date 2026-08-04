# SPDX-License-Identifier: AGPL-3.0-or-later

## log of a chi-square(1) variate: mean psi(1/2) + log 2 = -gamma - log 2,
## variance psi'(1/2) = pi^2 / 2. These are the two constants that make
## the linearised measurement equation a valid quasi-likelihood.
.k03_logchi2_mean <- -1.2703628454614782
.k03_logchi2_var <- 4.934802200544679
.k03_gold <- 0.3819660112501051   # (3 - sqrt(5)) / 2

## Gaussian quasi log-likelihood from the Kalman filter.
.k03_kalman_qll <- function(y, mu, phi, sig2) {
  n <- length(y)
  if (!(phi > -0.999999 && phi < 0.999999) || sig2 <= 0) return(-1e300)
  a <- 0
  p <- sig2 / (1 - phi * phi)
  cc <- mu + .k03_logchi2_mean
  ll <- 0
  for (t in seq_len(n)) {
    v <- y[t] - cc - a
    f <- p + .k03_logchi2_var
    if (f <= 0) return(-1e300)
    ll <- ll - 0.5 * (log(2 * 3.141592653589793 * f) + v * v / f)
    k <- p / f
    a <- a + k * v
    p <- p - k * p
    a <- phi * a
    p <- phi * phi * p + sig2
  }
  ll
}

## Deterministic golden-section maximisation on [lo, hi].
.k03_golden <- function(f, lo, hi, iters = 80L) {
  x1 <- lo + .k03_gold * (hi - lo)
  x2 <- hi - .k03_gold * (hi - lo)
  f1 <- f(x1)
  f2 <- f(x2)
  for (i in seq_len(iters)) {
    if (f1 < f2) {
      lo <- x1; x1 <- x2; f1 <- f2
      x2 <- hi - .k03_gold * (hi - lo)
      f2 <- f(x2)
    } else {
      hi <- x2; x2 <- x1; f2 <- f1
      x1 <- lo + .k03_gold * (hi - lo)
      f1 <- f(x1)
    }
  }
  if (f1 >= f2) x1 else x2
}

#' Quasi-likelihood SV(1) via the Kalman filter
#'
#' The stochastic volatility model \eqn{r_t = \exp(h_t/2) z_t} with
#' \eqn{z_t} standard normal and \eqn{h_t} a Gaussian AR(1) is not a
#' linear Gaussian state space model, so the exact likelihood needs
#' simulation. Squaring and taking logs linearises it,
#' \deqn{\log r_t^2 = h_t + \log z_t^2,}
#' at the price of a measurement error \eqn{\log z_t^2} that is the log
#' of a chi-square with one degree of freedom, and so is very far from
#' normal: it is sharply left-skewed. Treating it as normal anyway, with
#' its true first two moments
#' \eqn{E[\log z^2] = \psi(1/2) + \log 2 = -\gamma - \log 2 = -1.2703628}
#' and \eqn{Var[\log z^2] = \psi'(1/2) = \pi^2/2 = 4.9348022}, turns the
#' model into a linear Gaussian one whose Kalman filter prediction errors
#' give a \emph{quasi} likelihood. Maximising it is consistent and
#' asymptotically normal but not efficient, and the reported \code{ll} is
#' a quasi log-likelihood, not a log-likelihood: it is not comparable
#' with the likelihood of a model estimated exactly.
#'
#' The filter runs on the state \eqn{h^*_t = h_t - \mu}, with stationary
#' prior \eqn{Var(h^*_0) = \sigma_\eta^2/(1 - \phi^2)} and measurement
#' intercept \eqn{\mu + E[\log z^2]}.
#'
#' Maximisation is a deterministic coordinate ascent: golden-section line
#' search on each of \eqn{\mu}, \eqn{\phi} and \eqn{\log \sigma_\eta} in
#' turn, for a fixed number of sweeps. There is no random restart and no
#' convergence tolerance, so the result is a deterministic function of
#' the data and of \code{init}.
#'
#' Zero returns make \eqn{\log r_t^2} infinite. \code{offset} is added to
#' \eqn{r_t^2} before the log to keep the filter finite; it is the usual
#' remedy and it biases the estimate slightly, so it is reported.
#'
#' Mirrors \code{morie.fn.volsv} on the Python side.
#'
#' @param r Numeric vector of returns, not squared and not logged.
#' @param init Optional starting \code{c(mu, phi, sigma_eta)}. Defaults to
#'   a method of moments start read off \eqn{\log r_t^2}.
#' @param sweeps Number of coordinate-ascent sweeps.
#' @param offset Added to \eqn{r_t^2} before taking logs.
#' @return Named list with \code{mu}, \code{phi}, \code{sigma_eta},
#'   \code{ll}, \code{n}, \code{sweeps}, \code{offset}, \code{init},
#'   \code{method}.
#' @references Harvey A C, Ruiz E & Shephard N (1994). Multivariate
#'   stochastic variance models. \emph{Review of Economic Studies} 61(2),
#'   247--264.
#' @examples
#' set.seed(3)
#' Volsv(rnorm(200) * 0.5, sweeps = 5)$phi
#' @export
Volsv <- function(r, init = NULL, sweeps = 25L, offset = 1e-8) {
  rv <- as.numeric(r)
  n <- length(rv)
  if (n < 10L) stop("need at least ten observations", call. = FALSE)
  offset <- as.numeric(offset)[1L]
  if (!is.finite(offset) || offset < 0) {
    stop("offset must be non-negative", call. = FALSE)
  }
  y <- log(rv * rv + offset)

  if (is.null(init)) {
    ybar <- 0
    for (v in y) ybar <- ybar + v
    ybar <- ybar / n
    s2 <- 0
    for (v in y) s2 <- s2 + (v - ybar)^2
    s2 <- s2 / (n - 1)
    vh <- s2 - .k03_logchi2_var
    if (vh <= 0) vh <- 0.05
    phi0 <- 0.9
    init <- c(ybar - .k03_logchi2_mean, phi0, sqrt(vh * (1 - phi0 * phi0)))
  }
  iv <- as.numeric(init)
  if (length(iv) != 3L) {
    stop("init must be (mu, phi, sigma_eta)", call. = FALSE)
  }
  mu <- iv[1L]; phi <- iv[2L]; sig <- iv[3L]
  if (!(phi > -0.999999 && phi < 0.999999)) {
    stop("initial phi must lie strictly inside (-1, 1)", call. = FALSE)
  }
  if (sig <= 0) {
    stop("initial sigma_eta must be positive", call. = FALSE)
  }
  lsig <- log(sig)

  sweeps <- as.integer(sweeps)
  if (is.na(sweeps) || sweeps < 1L) {
    stop("sweeps must be at least 1", call. = FALSE)
  }
  for (s in seq_len(sweeps)) {
    mu <- .k03_golden(function(v) .k03_kalman_qll(y, v, phi, exp(2 * lsig)),
                      mu - 5, mu + 5)
    phi <- .k03_golden(function(v) .k03_kalman_qll(y, mu, v, exp(2 * lsig)),
                       -0.999, 0.999)
    lsig <- .k03_golden(function(v) .k03_kalman_qll(y, mu, phi, exp(2 * v)),
                        lsig - 3, lsig + 3)
  }

  sig <- exp(lsig)
  ll <- .k03_kalman_qll(y, mu, phi, sig * sig)
  list(mu = mu,
       phi = phi,
       sigma_eta = sig,
       ll = ll,
       n = n,
       sweeps = sweeps,
       offset = offset,
       init = iv,
       method = paste("SV(1) quasi-likelihood via Kalman filter",
                      "(Harvey-Ruiz-Shephard 1994)"))
}
