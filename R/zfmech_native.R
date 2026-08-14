# Zero-concentrated differential privacy and the Gaussian mechanism.
# Sources: Bun, M. & Steinke, T. (2016) "Concentrated Differential
# Privacy: Simplifications, Extensions, and Lower Bounds", in *Theory
# of Cryptography (TCC 2016-B)*, Lecture Notes in Computer Science
# 9985, 635-658, doi:10.1007/978-3-662-53641-4_24 (arXiv:1605.02065);
# Definition 1.1 for (xi,rho)-zCDP and the equivalent
# moment-generating form (2); Definition 1.2 for the privacy loss
# random variable; Definition 1.5 for sensitivity; Proposition 1.6
# for the Gaussian mechanism being (Delta^2/(2 sigma^2))-zCDP and
# the remark that the defining inequality is exactly tight for it at
# every alpha; Propositions 1.3 and 1.4 for the conversions to and
# from approximate and pure differential privacy; Lemma 1.7 for
# composition; Lemma 1.8 for post-processing; Proposition 1.9 for
# the k^2 rho group privacy bound. Dwork, C., Kenthapadi, K.,
# McSherry, F., Mironov, I. & Naor, M. (2006) "Our Data, Ourselves:
# Privacy via Distributed Noise Generation", in *Advances in
# Cryptology (EUROCRYPT 2006)*, Lecture Notes in Computer Science
# 4004, 486-503, doi:10.1007/11761679_29, for the Gaussian mechanism
# itself.
#
# Native implementation mirroring Python morie.fn.zfmech exactly: the
# same zCDP boundary, the same Gaussian mechanism with the same
# Box-Muller cosine branch and the same u1 = 1e-12 clamp against
# log(0), the same conversions in both directions, the same
# round-trip reporting the inflation rather than hiding it, and the
# same rejection conditions so both arms refuse the same inputs.

.zfmech_check_rho <- function(rho) {
  rho <- as.numeric(rho)
  if (rho <= 0)
    stop("zfmech: rho must be positive, got ", format(rho))
  rho
}

#' Renyi divergence D_alpha between two Gaussians of equal variance
#'
#' Equals alpha (mu0 - mu1)^2 / (2 sigma^2) -- linear in alpha, which
#' is what makes the Gaussian mechanism sit exactly on the zCDP
#' boundary rather than inside it.
#'
#' @param mu0 Mean of the first Gaussian.
#' @param mu1 Mean of the second Gaussian.
#' @param sigma Standard deviation, must be positive.
#' @param alpha Renyi order, must lie strictly in (1, inf).
#' @return A scalar numeric: the Renyi divergence.
#' @references Bun, M. & Steinke, T. (2016). Definition 1.1, the
#'   equivalent moment-generating form (2), and the remark after
#'   Proposition 1.6.
#' @export
renyi_divergence_gaussian <- function(mu0, mu1, sigma, alpha) {
  sigma <- as.numeric(sigma)
  if (sigma <= 0)
    stop("zfmech: sigma must be positive")
  alpha <- as.numeric(alpha)
  if (!is.finite(alpha) || !(alpha > 1))
    stop("zfmech: alpha must lie in (1, inf), got ", format(alpha))
  d <- as.numeric(mu0) - as.numeric(mu1)
  alpha * d * d / (2 * sigma * sigma)
}

#' Proposition 1.6: rho = Delta^2 / (2 sigma^2)
#'
#' @param sensitivity Sensitivity Delta, must be non-negative.
#' @param sigma Standard deviation, must be positive.
#' @return A scalar numeric: the zCDP parameter rho.
#' @references Bun, M. & Steinke, T. (2016). Proposition 1.6.
#' @export
zcdp_of_gaussian <- function(sensitivity, sigma) {
  sigma <- as.numeric(sigma)
  if (sigma <= 0)
    stop("zfmech: sigma must be positive")
  sensitivity <- as.numeric(sensitivity)
  if (sensitivity < 0)
    stop("zfmech: sensitivity cannot be negative")
  sensitivity * sensitivity / (2 * sigma * sigma)
}

#' @rdname zcdp_of_gaussian
#' @export
zero_concentrated_dp <- zcdp_of_gaussian

#' The noise scale that buys a given rho
#'
#' @param sensitivity Sensitivity Delta, must be non-negative.
#' @param rho zCDP parameter, must be positive.
#' @return A scalar numeric: the Gaussian standard deviation sigma.
#' @references Bun, M. & Steinke, T. (2016). Proposition 1.6.
#' @export
sigma_for_rho <- function(sensitivity, rho) {
  rho <- .zfmech_check_rho(rho)
  sensitivity <- as.numeric(sensitivity)
  if (sensitivity < 0)
    stop("zfmech: sensitivity cannot be negative")
  sensitivity / sqrt(2 * rho)
}

#' Release q(x) under rho-zCDP
#'
#' Draws Box-Muller cosine-branch normals from the shared generator
#' using two uniforms per variate, with u1 clamped at 1e-12 to match
#' the Python arm's safeguard against log(0). For \code{n = 1L} the
#' release is a scalar; for \code{n > 1L} it is a numeric vector of
#' length \code{n}, matching the Python list payload.
#'
#' @param value The true query value q(x).
#' @param sensitivity Sensitivity Delta, must be non-negative.
#' @param rho zCDP parameter, must be positive.
#' @param seed Seed for the shared generator.
#' @param n Number of releases to draw, a positive integer.
#' @return A list with \code{release}, \code{sigma}, \code{rho},
#'   \code{sensitivity}.
#' @references Bun, M. & Steinke, T. (2016). Proposition 1.6;
#'   Dwork et al. (2006) for the Gaussian mechanism itself.
#' @export
morie_zfmech <- function(value, sensitivity, rho, seed = 0, n = 1L) {
  sigma <- sigma_for_rho(sensitivity, rho)
  n <- as.integer(n)
  e <- .ghc_rng(as.numeric(seed))
  uu <- .ghc_unif(e, 2L * n)
  u1 <- pmax(uu[seq(1L, 2L * n, by = 2L)], 1e-12)
  u2 <- uu[seq(2L, 2L * n, by = 2L)]
  z <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  out <- as.numeric(value) + sigma * z
  if (n == 1L) out <- out[1L]
  list(release = out, sigma = sigma, rho = as.numeric(rho),
       sensitivity = as.numeric(sensitivity))
}

#' @rdname morie_zfmech
#' @export
gaussian_mechanism <- morie_zfmech

#' Lemma 1.7: rho simply adds
#'
#' @param rhos Numeric vector of rho values, each non-negative.
#' @return A list with \code{rho} (the sum), \code{k} (the count),
#'   and a \code{note} explaining that composition is additive in
#'   rho with no delta budget and no advanced composition theorem.
#' @references Bun, M. & Steinke, T. (2016). Lemma 1.7.
#' @export
compose <- function(rhos) {
  rs <- as.numeric(rhos)
  if (any(rs < 0))
    stop("zfmech: every rho must be non-negative")
  list(rho = sum(rs), k = length(rs),
       note = "additive, with no delta budget and no advanced composition theorem")
}

#' Proposition 1.9: groups of size k cost k^2 rho
#'
#' @param rho zCDP parameter, must be positive.
#' @param k Group size, must be at least 1.
#' @return A list with \code{rho}, \code{k}, and a \code{growth}
#'   string noting the quadratic, exactly tight bound.
#' @references Bun, M. & Steinke, T. (2016). Proposition 1.9.
#' @export
group_privacy <- function(rho, k) {
  rho <- .zfmech_check_rho(rho)
  k <- as.integer(k)
  if (k < 1L)
    stop("zfmech: the group size must be at least 1")
  list(rho = as.numeric(k)^2 * rho, k = k,
       growth = "quadratic in k, and exactly tight")
}

#' Proposition 1.3: (rho + 2 sqrt(rho log(1/delta)), delta)-DP
#'
#' @param rho zCDP parameter, must be positive.
#' @param delta Failure probability, must lie in (0, 1).
#' @return A list with \code{epsilon}, \code{delta}, \code{rho}.
#' @references Bun, M. & Steinke, T. (2016). Proposition 1.3.
#' @export
to_approx_dp <- function(rho, delta) {
  rho <- .zfmech_check_rho(rho)
  delta <- as.numeric(delta)
  if (!is.finite(delta) || !(delta > 0) || !(delta < 1))
    stop("zfmech: delta must lie in (0, 1), got ", format(delta))
  eps <- rho + 2 * sqrt(rho * log(1 / delta))
  list(epsilon = eps, delta = delta, rho = rho)
}

#' Proposition 1.4: epsilon-DP is 0.5 epsilon^2-zCDP
#'
#' @param epsilon Pure DP parameter, must be non-negative.
#' @return A list with \code{rho} and \code{epsilon}.
#' @references Bun, M. & Steinke, T. (2016). Proposition 1.4.
#' @export
from_pure_dp <- function(epsilon) {
  epsilon <- as.numeric(epsilon)
  if (epsilon < 0)
    stop("zfmech: epsilon cannot be negative")
  list(rho = 0.5 * epsilon * epsilon, epsilon = epsilon)
}

#' What the two conversions cost when chained
#'
#' \code{epsilon -> rho -> (epsilon', delta)} does not return
#' \code{epsilon}; the gap is reported as \code{inflation} rather
#' than quietly absorbed. When the input is \code{epsilon = 0} the
#' \code{to_approx_dp} step is skipped (it would refuse rho = 0) and
#' \code{epsilon_out} is set to 0.
#'
#' @param epsilon Pure DP epsilon, non-negative.
#' @param delta Failure probability for the conversion back to DP.
#' @return A list with \code{epsilon_in}, \code{rho},
#'   \code{epsilon_out}, \code{inflation}, \code{delta}.
#' @references Bun, M. & Steinke, T. (2016). Propositions 1.3 and 1.4.
#' @export
round_trip <- function(epsilon, delta) {
  rho <- from_pure_dp(epsilon)$rho
  if (rho > 0)
    back <- to_approx_dp(rho, delta)
  else
    back <- list(epsilon = 0)
  list(epsilon_in = as.numeric(epsilon), rho = rho,
       epsilon_out = back$epsilon,
       inflation = back$epsilon - as.numeric(epsilon),
       delta = as.numeric(delta))
}

#' Lemma 1.8: any function of the output keeps the same rho
#'
#' @param rho zCDP parameter, must be positive.
#' @return A list with \code{rho} and a \code{note} explaining that
#'   the parameter is invariant under post-processing -- unlike
#'   Dwork and Rothblum's mCDP, which is not closed under
#'   post-processing.
#' @references Bun, M. & Steinke, T. (2016). Lemma 1.8.
#' @export
postprocessing <- function(rho) {
  rho <- .zfmech_check_rho(rho)
  list(rho = rho,
       note = "invariant -- unlike Dwork and Rothblum's mCDP, which is not closed under post-processing")
}

#' One-line summary of zCDP for the cheatsheet
#'
#' @return A single character string summarising zCDP, the Gaussian
#'   mechanism, composition, post-processing, group privacy, and
#'   both conversions.
#' @references Bun, M. & Steinke, T. (2016).
#' @export
.zfmech_cheatsheet <- function() {
  paste("zfmech: rho-zCDP means D_alpha(M(x)||M(x')) <= rho alpha ",
        "for EVERY alpha > 1. Gaussian mechanism: rho = ",
        "Delta^2/(2 sigma^2), and the bound is exactly tight. ",
        "Composition adds rho; post-processing leaves it alone; ",
        "groups of size k cost k^2 rho, not k rho. Conversions: ",
        "eps-DP -> eps^2/2 zCDP, and rho-zCDP -> (rho + ",
        "2 sqrt(rho log(1/delta)), delta)-DP. Chaining them does ",
        "NOT return the original epsilon.", sep = "")
}
