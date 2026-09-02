# SPDX-License-Identifier: AGPL-3.0-or-later
#' Binomial-normal GLMM meta-analysis of proportions
#'
#' x_i | p_i ~ Binomial(n_i, p_i) with logit(p_i) = mu + sigma z_i and
#' z_i ~ N(0, 1); the marginal likelihood is evaluated by Gauss-Hermite
#' quadrature (z = sqrt(2) t) and maximised over (mu, sigma) by nested
#' golden-section search with a fixed iteration count.  The standard error
#' inverts the 2 by 2 observed information, as metafor::rma.glmm reports.
#' Source consulted: Hamza, van Houwelingen and Stijnen (2008), Journal of
#' Clinical Epidemiology 61, 41-51.
#'
#' @param xi event counts.
#' @param ni sample sizes.
#' @param quad number of Gauss-Hermite nodes.
#' @param level confidence level.
#' @return list: estimate, logit_mu, sigma, tau2, se, ci_lower, ci_upper,
#'   loglik, quad, n_events, n, method.
#' @keywords internal
#' @examples
#' magpa(c(3, 7, 12, 2, 9), c(40, 50, 60, 35, 55))$estimate
#' @export
magpa <- function(xi, ni, quad = 21L, level = 0.95) {
  x <- as.numeric(xi)
  nn <- as.numeric(ni)
  lchoose1 <- function(nv, kv) {
    tot <- 0
    if (kv >= 1) for (j in 1:kv) tot <- tot + log((nv - kv + j) / j)
    tot
  }
  lgc <- vapply(seq_along(x), function(i) lchoose1(nn[i], x[i]), numeric(1))
  g <- k02gh(as.integer(quad))
  s2 <- 1.4142135623730951
  sqpi <- 1.7724538509055159
  nll <- function(mu, sigma) {
    tot <- 0
    for (i in seq_along(x)) {
      acc <- 0
      for (q in seq_along(g$x)) {
        eta <- mu + sigma * s2 * g$x[q]
        if (eta >= 0) {
          lp <- -log1p(exp(-eta))
          lq <- -eta - log1p(exp(-eta))
        } else {
          lp <- eta - log1p(exp(eta))
          lq <- -log1p(exp(eta))
        }
        acc <- acc + g$w[q] * exp(x[i] * lp + (nn[i] - x[i]) * lq)
      }
      tot <- tot + (lgc[i] + (if (acc > 0) log(acc / sqpi) else -700))
    }
    -tot
  }
  inner <- function(s) k02gold(function(m) nll(m, s), -12, 12, 70L)
  outer <- function(s) nll(inner(s), s)
  sigma <- k02gold(outer, 0, 5, 70L)
  mu <- inner(sigma)
  f0 <- nll(mu, sigma)
  h <- 1e-4
  hmm <- (nll(mu + h, sigma) - 2 * f0 + nll(mu - h, sigma)) / (h * h)
  fss <- (nll(mu, sigma + h) - 2 * f0 + nll(mu, abs(sigma - h))) / (h * h)
  fms <- (nll(mu + h, sigma + h) - nll(mu + h, abs(sigma - h)) -
          nll(mu - h, sigma + h) + nll(mu - h, abs(sigma - h))) / (4 * h * h)
  det <- hmm * fss - fms * fms
  se <- if (det > 0 && fss > 0) sqrt(fss / det) else NA_real_
  crit <- k02z(0.5 + 0.5 * level)
  ilogit <- function(t) 1 / (1 + exp(-t))
  list(estimate = ilogit(mu), logit_mu = mu, sigma = sigma, tau2 = sigma^2,
       se = se, ci_lower = ilogit(mu - crit * se),
       ci_upper = ilogit(mu + crit * se), loglik = -f0,
       quad = as.integer(quad), n_events = sum(x), n = length(x),
       method = "Binomial-normal GLMM for proportions, Gauss-Hermite (Hamza, van Houwelingen & Stijnen 2008)")
}

# CANONICAL TEST
# r <- magpa(c(3,7,12,2,9), c(40,50,60,35,55))
# stopifnot(abs(r$estimate - 0.13630003) < 1e-6, abs(r$tau2 - 0.0127498) < 1e-6)

#' @rdname magpa
#' @keywords internal
#' @export
morie_magpa <- magpa
