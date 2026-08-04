# SPDX-License-Identifier: AGPL-3.0-or-later
#' ABC sequential Monte Carlo
#'
#' Toni, Welch, Strelkowa, Ipsen and Stumpf (2009), Approximate Bayesian
#' computation scheme for parameter inference and model selection in
#' dynamical systems, J. R. Soc. Interface 6(31), 187-202: a decreasing
#' tolerance schedule eps_1 > ... > eps_T; at population t a particle is
#' proposed from the previous population, perturbed by a kernel K,
#' accepted if d(S(x*), S(x)) <= eps_t, and weighted by w_t^i =
#' pi(theta_t^i) / sum_j w_(t-1)^j K(theta_t^i | theta_(t-1)^j).  The paper
#' is open access but was not retrievable here; the schedule, the
#' acceptance rule and the weight are quoted in their standard published
#' form.
#'
#' Determinism: particles are not drawn -- the initial population is a
#' low-discrepancy grid through the prior's inverse CDF and the
#' perturbation is applied at low-discrepancy offsets.  The weights, the
#' effective sample size and the acceptance rate are computed exactly.
#'
#' @param model function theta -> summary statistics.
#' @param summary_stats the observed summaries.
#' @param priors list of c(lo, hi) uniform supports.
#' @param n_particles particles per population.
#' @param schedule decreasing tolerances.
#' @param kernel_sd perturbation scale as a fraction of each prior width.
#' @return list: estimate, theta, weights, ess, accept, method.
#' @keywords internal
#' @examples
#' Abcsmc(function(th) th[1], 0.5, list(c(0, 1)), 8, c(0.5, 0.2))$ess
#' @export
Abcsmc <- function(model, summary_stats, priors = NULL, n_particles = 32,
                   schedule = NULL, kernel_sd = 0.1) {
  S <- .s03vec(summary_stats)
  pr <- if (!is.null(priors)) lapply(priors, as.numeric) else list(c(0, 1))
  d <- length(pr); N <- as.integer(n_particles)
  sch <- if (!is.null(schedule)) .s03vec(schedule) else c(2, 1, 0.5)
  theta <- matrix(0, N, d)
  for (i in seq_len(N)) for (a in seq_len(d)) {
    theta[i, a] <- pr[[a]][1] + (pr[[a]][2] - pr[[a]][1]) * .s03vdc(i - 1L, 1L + a)
  }
  w <- rep(1 / N, N)
  accept <- numeric(0)
  for (t in seq_along(sch)) {
    eps <- sch[t]
    newth <- list(); neww <- numeric(0); tries <- 0L; i <- 0L
    while (length(newth) < N && tries < 20L * N) {
      src <- theta[(i %% N) + 1L, ]
      cand <- numeric(d)
      for (a in seq_len(d)) {
        off <- (.s03vdc(tries * d + (a - 1L), 1L + a) - 0.5) * 2 *
          as.numeric(kernel_sd) * (pr[[a]][2] - pr[[a]][1])
        cand[a] <- min(max(src[a] + off, pr[[a]][1]), pr[[a]][2])
      }
      sim <- .s03vec(model(cand))
      dist <- 0
      for (a in seq_along(S)) dist <- dist + (sim[a] - S[a])^2
      dist <- sqrt(dist)
      tries <- tries + 1L
      i <- i + 1L
      if (dist <= eps) {
        den <- 0
        for (j in seq_len(N)) {
          q <- 1
          for (a in seq_len(d)) {
            h <- as.numeric(kernel_sd) * (pr[[a]][2] - pr[[a]][1])
            if (h > 0) {
              u <- (cand[a] - theta[j, a]) / h
              q <- q * exp(-0.5 * u * u) / (h * sqrt(2 * pi))
            }
          }
          den <- den + w[j] * q
        }
        newth[[length(newth) + 1L]] <- cand
        neww <- c(neww, if (den > 0) 1 / den else 0)
      }
    }
    accept <- c(accept, if (tries > 0L) length(newth) / tries else 0)
    if (length(newth) == 0L) break
    tot <- 0
    for (x in neww) tot <- tot + x
    theta <- do.call(rbind, newth)
    w <- if (tot > 0) neww / tot else rep(1 / length(newth), length(newth))
  }
  s1 <- 0; s2 <- 0
  for (x in w) { s1 <- s1 + x; s2 <- s2 + x * x }
  m0 <- 0
  for (i in seq_len(nrow(theta))) m0 <- m0 + w[i] * theta[i, 1]
  list(estimate = m0, theta = theta, weights = w,
       ess = if (s2 > 0) (s1 * s1) / s2 else 0, accept = accept,
       method = "ABC-SMC over a decreasing tolerance schedule (Toni et al. 2009), on a deterministic particle design")
}
