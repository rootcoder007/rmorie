# SPDX-License-Identifier: AGPL-3.0-or-later
#' Metropolis-Hastings sampler
#'
#' alpha = min(1, pi(x-prime) q(x | x-prime) / (pi(x) q(x-prime | x))), which
#' reduces to min(1, pi ratio) for a symmetric proposal.  Proposal increments
#' and acceptance uniforms are supplied by the caller so chains are exactly
#' reproducible.  Sources consulted: Metropolis, Rosenbluth, Rosenbluth,
#' Teller and Teller (1953), Journal of Chemical Physics 21(6), 1087-1092;
#' Hastings (1970), Monte Carlo sampling methods using Markov chains and their
#' applications, Biometrika 57(1), 97-109.
#'
#' @param target unnormalised target density.
#' @param x0 starting state.
#' @param n_iter number of iterations.
#' @param u uniforms in [0, 1) for the accept/reject decision.
#' @param z standard normal increments for the random-walk proposal.
#' @param scale random-walk proposal standard deviation.
#' @param q optional proposal density q(x_to, x_from).
#' @param burn number of leading draws to discard.
#' @return list: estimate, sd, accept_rate, chain, accepted, n, method.
#' @keywords internal
#' @examples
#' mhmcmc(function(x) 1, 0, 10, u = rep(0.5, 10), z = rep(1, 10), scale = 1)$accepted
#' @export
mhmcmc <- function(target, x0 = 0, n_iter = 1000L, u = NULL, z = NULL,
                   scale = 1, q = NULL, burn = 0L) {
  ni <- as.integer(n_iter)
  uv <- as.numeric(u)
  zv <- as.numeric(z)
  x <- as.numeric(x0)
  px <- as.numeric(target(x))
  chain <- numeric(ni)
  accepted <- 0L
  for (i in seq_len(ni)) {
    prop <- x + scale * zv[((i - 1) %% length(zv)) + 1]
    pp <- as.numeric(target(prop))
    ratio <- if (px <= 0) (if (pp > 0) 1 else 0) else pp / px
    if (!is.null(q)) {
      qf <- as.numeric(q(prop, x))
      qb <- as.numeric(q(x, prop))
      ratio <- if (qf > 0) ratio * (qb / qf) else 0
    }
    alpha <- min(ratio, 1)
    if (uv[((i - 1) %% length(uv)) + 1] < alpha) { x <- prop
    px <- pp
    accepted <- accepted + 1L }
    chain[i] <- x
  }
  keep <- if (burn < ni) chain[(burn + 1):ni] else chain
  m <- mean(keep)
  k <- length(keep)
  sd <- if (k > 1) sqrt(sum((keep - m)^2) / (k - 1)) else NA_real_
  list(estimate = as.numeric(m), sd = as.numeric(sd),
       accept_rate = if (ni > 0) accepted / ni else NA_real_,
       chain = chain, accepted = as.integer(accepted), n = as.integer(ni),
       method = "Metropolis-Hastings (Metropolis et al. 1953; Hastings 1970)")
}

# CANONICAL TEST
# r <- mhmcmc(function(x) 1, 0, 10, u = rep(0.5, 10), z = rep(1, 10), scale = 1)
# stopifnot(r$accepted == 10L, abs(r$chain[10] - 10) < 1e-12)

#' @rdname mhmcmc
#' @keywords internal
#' @export
morie_metropolis_hastings <- mhmcmc
