# SPDX-License-Identifier: AGPL-3.0-or-later

# Hamilton (1989) filter and Kim (1994) smoother for a K-regime Gaussian
# model y_t | s_t = j ~ N(mu_j, sigma_j^2), written in MSwM's notation:
# P is COLUMN-stochastic, P[j, i] = Pr(s_t = j | s_{t-1} = i), and `ini`
# is the state distribution at t = 0 (before the first observation).
# Returns the log-likelihood, the (n + 1) x K smoothed probabilities
# (row 1 = t = 0) and the summed smoothed joint probabilities
# Jsum[i, j] = sum_t Pr(s_{t-1} = i, s_t = j | y).
.morie_msw_smooth <- function(y, mu, sg, P, ini) {
  n <- length(y)
  k <- length(mu)
  L <- matrix(vapply(seq_len(k), function(j) stats::dnorm(y, mu[j], sg[j]),
                     numeric(n)), n, k)
  L <- pmax(L, .Machine$double.xmin)
  f <- matrix(0, n, k)
  cst <- numeric(n)
  prev <- ini
  for (t in seq_len(n)) {
    a <- as.numeric(P %*% prev) * L[t, ]
    cst[t] <- sum(a)
    f[t, ] <- a / cst[t]
    prev <- f[t, ]
  }
  proba <- rbind(ini, f)
  pro <- proba %*% t(P)
  smo <- matrix(0, n + 1, k)
  smo[n + 1, ] <- f[n, ]
  Jsum <- matrix(0, k, k)
  tP <- t(P)
  for (i in n:1) {
    J <- (proba[i, ] %o% (smo[i + 1, ] / pro[i, ])) * tP
    smo[i, ] <- rowSums(J)
    Jsum <- Jsum + J
  }
  list(loglik = sum(log(cst)), smo = smo, Jsum = Jsum)
}

# EM iterations from an initial (n + 1) x K matrix of smoothed
# probabilities, reproducing MSwM::msmFit()'s iteraEM() for an
# intercept-only lm with sw = c(TRUE, TRUE): weighted regime means and
# variances (weights = smoothed probabilities at t = 1..n), initial state
# = smoothed t = 0 probabilities, transition update
# P[i, j] = sum_t Pr(s_{t-1} = i, s_t = j) / sum_{t=1}^n Pr(s_t = j)
# (MSwM's update), and MSwM's relative stopping rule on the
# log-likelihood and the coefficients.
.morie_msw_em <- function(y, w0, maxit = 2000L, tol = 1e-10) {
  n <- length(y)
  k <- ncol(w0)
  smo <- w0
  Jsum <- crossprod(w0[-(n + 1), , drop = FALSE], w0[-1, , drop = FALSE])
  ll_old <- NA_real_
  mu_old <- rep(0, k)
  floor_sd <- 1e-8 * max(stats::sd(y), 1e-12)
  for (it in seq_len(maxit)) {
    w <- smo[-1, , drop = FALSE]
    sw <- pmax(colSums(w), .Machine$double.xmin)
    mu <- colSums(w * y) / sw
    sg <- sqrt(colSums(w * (y - rep(mu, each = n))^2) / sw)
    sg <- pmax(sg, floor_sd)
    P <- sweep(Jsum, 2L, sw, "/")
    ini <- smo[1, ]
    h <- .morie_msw_smooth(y, mu, sg, P, ini)
    smo <- h$smo
    Jsum <- h$Jsum
    if (!is.finite(h$loglik)) break
    if (!is.na(ll_old) &&
        abs(h$loglik - ll_old) / (0.1 + abs(h$loglik)) < tol &&
        max(abs(mu - mu_old)) / (0.1 + max(abs(mu))) < tol) {
      break
    }
    ll_old <- h$loglik
    mu_old <- mu
  }
  list(mu = mu, sigma = sg, P = P, smo = smo, loglik = h$loglik, iter = it)
}

#' Markov-switching regression (Hamilton 1989)
#'
#' Fit a K-regime Markov-switching model with a switching mean and a
#' switching variance, \eqn{y_t \mid s_t = j \sim N(\mu_j,
#' \sigma_j^2)}, by EM with the Hamilton filter and Kim smoother.  This
#' is the model \code{MSwM::msmFit(lm(y ~ 1), k, sw = c(TRUE, TRUE))}
#' fits, computed natively: the E- and M-steps reproduce MSwM's EM
#' (including its transition-matrix update, which normalises the summed
#' smoothed joint probabilities by the occupancy of the destination
#' regime, so its fixed point can differ from the exact maximum
#' likelihood estimate in the fourth decimal of the log-likelihood).
#' Instead of MSwM's random starts the EM runs from two deterministic
#' starts (regimes split by the quantiles of \eqn{y} and of
#' \eqn{|y - median(y)|}) and keeps the higher log-likelihood.  No
#' external package is used.
#'
#' @param x Numeric univariate series.
#' @param k_regimes Number of latent regimes. Default 2.
#' @return Named list with \eqn{mu, sigma, transition,
#'   smoothed_probabilities, loglik, n, k_regimes, method}.  Regimes are
#'   ordered by increasing \code{mu}.  \code{transition} uses MSwM's
#'   column convention: \code{transition[j, i]} is
#'   \eqn{Pr(s_t = j \mid s_{t-1} = i)} (columns sum to 1).
#'   \code{smoothed_probabilities} is n x K (observations 1..n).
#'   \code{loglik} is the maximised log-likelihood (MSwM's
#'   \code{logLikel} slot stores its negative).
#' @references Hamilton, J. D. (1989). A new approach to the economic
#'   analysis of nonstationary time series and the business cycle.
#'   Econometrica 57, 357-384.
#' @examples
#' set.seed(1)
#' morie_regime_switching(x = c(rnorm(40, 0, 0.5), rnorm(40, 2, 1.5)))$mu
#' @export
morie_regime_switching <- function(x, k_regimes = 2) {
  y <- as.numeric(x)
  n <- length(y)
  if (n < 4 * k_regimes) stop("Series too short for K regimes.")
  K <- as.integer(k_regimes)
  start_from <- function(score) {
    br <- stats::quantile(score, probs = seq(0, 1, length.out = K + 1L),
                          names = FALSE, type = 7)
    cl <- findInterval(score, br, rightmost.closed = TRUE, all.inside = TRUE)
    w <- matrix(0.2 / max(K - 1L, 1L), n, K)
    w[cbind(seq_len(n), cl)] <- 0.8
    rbind(rep(1 / K, K), w)
  }
  fits <- list(
    .morie_msw_em(y, start_from(y)),
    .morie_msw_em(y, start_from(abs(y - stats::median(y))))
  )
  lls <- vapply(fits, function(f) if (is.finite(f$loglik)) f$loglik else -Inf,
                numeric(1))
  best <- fits[[which.max(lls)]]
  o <- order(best$mu, best$sigma)
  list(
    mu = best$mu[o],
    sigma = best$sigma[o],
    transition = best$P[o, o, drop = FALSE],
    smoothed_probabilities = best$smo[-1, o, drop = FALSE],
    loglik = best$loglik,
    n = n, k_regimes = k_regimes,
    method = sprintf(
      "Markov switching mean/variance via EM/Hamilton filter (K=%d, base R)",
      K
    )
  )
}
