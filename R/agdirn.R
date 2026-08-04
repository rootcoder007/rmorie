# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dirichlet exploration noise at the MCTS root
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED), states that
#' "Dirichlet noise Dir(alpha) was added to the prior probabilities in the
#' root node".  The mixture P(s,a) = (1 - eps) p_a + eps eta_a with
#' eta ~ Dir(alpha) is written out in Silver et al. (2017), Nature 550,
#' 354-359, and reproduced in Schrittwieser et al. (2020),
#' arXiv:1911.08265 (FETCHED), appendix C; eps = 0.25 and alpha = 0.3,
#' 0.15, 0.03 for chess, shogi, Go.
#'
#' Determinism: a random Dirichlet draw would put the two arms out of
#' step, so eta is either supplied or built by inverting the Gamma CDF at
#' van der Corput points and normalising.  No clock, no seed.
#'
#' @param p root priors.
#' @param alpha Dirichlet concentration.
#' @param eps mixing weight.
#' @param eta optional Dirichlet vector supplied by the caller.
#' @return list: estimate, p_noisy, eta, entropy, alpha, eps, method.
#' @keywords internal
#' @examples
#' Rootnoise(c(0.5, 0.3, 0.2))$p_noisy
#' @export
Rootnoise <- function(p, alpha = 0.3, eps = 0.25, eta = NULL) {
  gamma_lower_reg <- function(a, x, iters = 400L) {
    if (x <= 0) return(0)
    if (x < a + 1) {
      term <- 1 / a; s <- term
      for (n in seq_len(iters - 1L)) {
        term <- term * x / (a + n)
        s <- s + term
        if (abs(term) < abs(s) * 1e-16) break
      }
      return(s * exp(-x + a * log(x) - lgamma(a)))
    }
    b <- x + 1 - a; cc <- 1e300; d <- 1 / b; h <- d
    for (i in seq_len(iters - 1L)) {
      an <- -i * (i - a)
      b <- b + 2
      d <- an * d + b
      if (abs(d) < 1e-300) d <- 1e-300
      cc <- b + an / cc
      if (abs(cc) < 1e-300) cc <- 1e-300
      d <- 1 / d
      de <- d * cc
      h <- h * de
      if (abs(de - 1) < 1e-16) break
    }
    1 - exp(-x + a * log(x) - lgamma(a)) * h
  }
  gamma_quantile <- function(a, pp) {
    lo <- 0; hi <- 1
    while (gamma_lower_reg(a, hi) < pp && hi < 1e8) hi <- hi * 2
    for (i in seq_len(200L)) {
      mid <- 0.5 * (lo + hi)
      if (gamma_lower_reg(a, mid) < pp) lo <- mid else hi <- mid
    }
    0.5 * (lo + hi)
  }
  pr <- .s03vec(p)
  m <- length(pr)
  a <- as.numeric(alpha); e <- as.numeric(eps)
  if (is.null(eta)) {
    raw <- numeric(m)
    for (i in seq_len(m)) raw[i] <- gamma_quantile(a, .s03vdc(i - 1L, 2L))
    tot <- 0
    for (x in raw) tot <- tot + x
    et <- if (tot > 0) raw / tot else rep(1 / m, m)
  } else {
    et <- .s03vec(eta)
    tot <- 0
    for (x in et) tot <- tot + x
    if (tot > 0) et <- et / tot
  }
  mixed <- (1 - e) * pr + e * et
  h <- 0
  for (x in mixed) if (x > 0) h <- h - x * log(x)
  list(estimate = if (m > 0L) mixed[1] else NaN, p_noisy = mixed, eta = et,
       entropy = h, alpha = a, eps = e,
       method = "Dirichlet exploration noise at the MCTS root")
}
