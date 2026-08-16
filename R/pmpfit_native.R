# Pitman-Yor two-parameter Poisson-Dirichlet distribution.
# Sources: Pitman, J. & Yor, M. (1997) "The Two-Parameter
# Poisson-Dirichlet Distribution Derived from a Stable Subordinator",
# The Annals of Probability 25(2), 855-900, doi:10.1214/aop/1024404422
# (Definition 1: the Beta(1 - alpha, theta + n alpha) construction;
# Proposition 2: the size-biased permutation of the ranked values;
# Proposition 4: the parameter range is FORCED by the requirement
# that residual factors be independent betas, not a convention).
# Sethuraman, J. (1994) "A Constructive Definition of Dirichlet
# Priors", Statistica Sinica 4(2), 639-650 (the alpha = 0 case).
#
# Native implementation mirroring Python morie.fn.pmpfit exactly: the
# same parameter validation, the same Marsaglia-Tsang gamma for the
# Beta draws, the same uniform stream from the shared SplitMix64
# generator, and the same recursive expected-clusters formula.

#' Pitman-Yor two-parameter Poisson-Dirichlet stick breaking
#'
#' Draws K sticks Y_n ~ Beta(1 - alpha, theta + n alpha) for
#' 0 <= alpha < 1 and theta > -alpha. At alpha = 0 this is exactly
#' the Dirichlet process. The form is forced by Proposition 4: a
#' size-biased permutation admits the residual allocation form with
#' independent factors if and only if the betas take this form.
#'
#' @param alpha Discount, in [0, 1).
#' @param theta Concentration, with theta > -alpha.
#' @param K Number of sticks, at least 1.
#' @param seed Seed for the shared SplitMix64 generator.
#' @return A list with `weights`, `Y`, `remaining`, `kept_mass`,
#'   `alpha`, `theta`, `note`.
#' @references Pitman, J. & Yor, M. (1997). The Annals of
#'   Probability, 25(2), 855-900.
#' @export
morie_pmpfit <- function(alpha, theta, K, seed = 0L) {
  params <- morie_pmpfit_check(alpha, theta)
  a <- params$alpha
  th <- params$theta
  K <- as.integer(K)
  if (K < 1L) stop("pmpfit: at least one stick is needed")
  e <- .ghc_rng(seed)
  w <- numeric(K)
  Ys <- numeric(K)
  rest <- 1.0
  for (kk in seq_len(K)) {
    y <- if (a > 0.0) {
      .ghc_beta(e, 1.0 - a, th + kk * a)
    } else {
      1.0 - (max(.ghc_unif(e, 1L), 1e-15) ^ (1.0 / th))
    }
    Ys[kk] <- y
    w[kk] <- y * rest
    rest <- rest * (1.0 - y)
  }
  list(weights = w, Y = Ys, remaining = rest,
       kept_mass = sum(w), alpha = a, theta = th,
       note = "Beta(1-alpha, theta + n alpha): the parameters DRIFT with n, which is what fattens the tail")
}

#' Check Pitman-Yor parameters
#'
#' Validates 0 <= alpha < 1 and theta > -alpha. The range is dictated
#' by Proposition 4, not a convention.
#'
#' @param alpha Discount.
#' @param theta Concentration.
#' @return A list with `alpha`, `theta`, `is_dirichlet`, `note`.
#' @references Pitman, J. & Yor, M. (1997). Proposition 4.
#' @export
morie_pmpfit_check <- function(alpha, theta) {
  a <- as.numeric(alpha)
  th <- as.numeric(theta)
  if (a < 0.0 || a >= 1.0)
    stop(sprintf("pmpfit: the discount must satisfy 0 <= alpha < 1, got %g", a))
  if (th <= -a)
    stop(sprintf("pmpfit: the concentration must satisfy theta > -alpha = %g, got %g",
                 -a, th))
  list(alpha = a, theta = th, is_dirichlet = a == 0.0,
       note = "alpha = 0 is exactly the Dirichlet process")
}

#' Pitman-Yor predictive weights
#'
#' Each occupied cluster gets (n_j - alpha) / (theta + n); a new
#' cluster gets (theta + K*alpha) / (theta + n). The mass removed by
#' discounting every cluster by alpha is exactly the new-cluster term.
#'
#' @param counts Numeric vector of positive cluster counts.
#' @param alpha Discount.
#' @param theta Concentration.
#' @return A list with `occupied`, `new`, `total`, `n`, `K`,
#'   `discount_transferred`, `note`.
#' @references Pitman, J. & Yor, M. (1997).
#' @export
morie_pmpfit_predictive <- function(counts, alpha, theta) {
  c_ <- as.numeric(counts)
  params <- morie_pmpfit_check(alpha, theta)
  a <- params$alpha
  th <- params$theta
  if (any(c_ <= 0))
    stop("pmpfit: an occupied cluster must have a positive count")
  n <- sum(c_)
  K <- length(c_)
  if (any(c_ <= a) && a > 0.0)
    stop("pmpfit: a cluster of size <= alpha would get a negative weight; alpha must be smaller than every cluster size")
  occ <- (c_ - a) / (th + n)
  new <- (th + K * a) / (th + n)
  list(occupied = occ, new = new,
       total = sum(occ) + new, n = n, K = K,
       discount_transferred = K * a / (th + n),
       note = "each existing cluster is discounted by alpha, and the removed mass funds the new cluster")
}

#' Expected number of clusters under Pitman-Yor
#'
#' Computes E[K_n] by the exact recursion
#' ek_{i+1} = ek_i + (theta + ek_i * alpha) / (theta + i).
#' The DP's count grows like theta*log(n); a positive discount makes
#' it grow like n^alpha.
#'
#' @param n Number of draws, at least 1.
#' @param alpha Discount.
#' @param theta Concentration.
#' @return A list with `expected`, `n`, `alpha`, `theta`, `regime`,
#'   `note`.
#' @references Pitman, J. & Yor, M. (1997).
#' @export
morie_pmpfit_expected <- function(n, alpha, theta) {
  params <- morie_pmpfit_check(alpha, theta)
  a <- params$alpha
  th <- params$theta
  N <- as.integer(n)
  if (N < 1L) stop("pmpfit: n must be at least 1")
  ek <- 0.0
  for (i in seq(0L, N - 1L)) {
    ek <- ek + (th + ek * a) / (th + i)
  }
  list(expected = ek, n = N, alpha = a, theta = th,
       regime = if (a > 0.0) "power law n^alpha" else "logarithmic theta log n",
       note = "the DP's count grows like theta log n; a positive discount makes it grow like n^alpha")
}

#' Tail comparison across discounts
#'
#' Computes the expected number of clusters at the same theta for a
#' sequence of discounts, so the log-vs-power growth is measured
#' rather than quoted.
#'
#' @param n Number of draws.
#' @param theta Concentration, default 1.
#' @param alphas Numeric vector of discounts, default c(0, 0.3, 0.6).
#' @return A list with `estimate`, `expected_clusters`, `n`, `theta`,
#'   `monotone_in_alpha`, `method`, `note`.
#' @references Pitman, J. & Yor (1997). Definition 1.
#' @export
morie_pmpfit_tail <- function(n, theta = 1.0, alphas = c(0.0, 0.3, 0.6)) {
  th <- as.numeric(theta)
  N <- as.integer(n)
  out <- list()
  for (a_ in alphas) {
    out[[as.character(a_)]] <- morie_pmpfit_expected(N, as.numeric(a_), th)$expected
  }
  keys <- sort(as.numeric(names(out)))
  mono <- TRUE
  if (length(keys) >= 2L) {
    for (i in seq_len(length(keys) - 1L)) {
      if (out[[as.character(keys[i])]] > out[[as.character(keys[i + 1L])]]) {
        mono <- FALSE
        break
      }
    }
  }
  list(estimate = out, expected_clusters = out, n = N, theta = th,
       monotone_in_alpha = mono,
       method = "two-parameter Poisson-Dirichlet; Pitman & Yor (1997) Definition 1",
       note = "more discount, more distinct types at the same n -- which is why vocabulary-like data want alpha > 0")
}

#' Pitman-Yor cheatsheet
#'
#' One-paragraph summary of the construction and why the extra
#' parameter fattens the tail.
#'
#' @return A character string.
#' @export
morie_pmpfit_cheatsheet <- function() {
  paste("pmpfit: the DP breaks its stick with Beta(1, theta)",
        "at EVERY index; Pitman-Yor lets the parameters DRIFT --",
        "Y_n ~ Beta(1 - alpha, theta + n alpha) for 0 <= alpha < 1",
        "and theta > -alpha, with alpha = 0 recovering the DP",
        "exactly. The form is FORCED: Proposition 4 says a",
        "size-biased permutation has independent residual factors",
        "iff the betas take this form. The payoff is the TAIL:",
        "geometric decay becomes polynomial, so the distinct-type",
        "count grows like n^alpha instead of theta log n -- which",
        "is why vocabularies and species counts want alpha > 0.",
        "Predictively, each cluster is discounted by alpha and the",
        "removed mass funds the new-cluster term.")
}

# Internal: ratio-of-gammas beta, same RNG consumption as Python.
#' Internal: ratio-of-gammas beta, same RNG consumption as Python
#'
#' A step of the pmpfit_native implementation. Called by \code{morie_pmpfit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.ghc_gamma}.
#' @param a Passed to \code{.ghc_gamma}.
#' @param b Passed to \code{.ghc_gamma}.
#' @return One of two values, depending on the branch taken.
#' @export
.ghc_beta <- function(e, a, b) {
  g1 <- .ghc_gamma(e, a)
  g2 <- .ghc_gamma(e, b)
  s <- g1 + g2
  if (s > 1e-12) g1 / s else 0.5
}

# Internal: Marsaglia-Tsang gamma with Ahrens-Dieter boost for shape < 1.
#' Internal: Marsaglia-Tsang gamma with Ahrens-Dieter boost for shape <
#' 1
#'
#' A step of the pmpfit_native implementation. Called by \code{.ghc_beta}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param shape Numeric; combined arithmetically in the body.
#' @return The value of \code{repeat}.
#' @export
.ghc_gamma <- function(e, shape) {
  if (shape < 1.0) {
    u <- max(.ghc_unif(e, 1L), 1e-15)
    return(.ghc_gamma(e, shape + 1.0) * (u ^ (1.0 / shape)))
  }
  d <- shape - 1.0 / 3.0
  c_ <- 1.0 / sqrt(9.0 * d)
  repeat {
    u1 <- min(max(.ghc_unif(e, 1L), 1e-12), 1 - 1e-12)
    u2 <- min(max(.ghc_unif(e, 1L), 1e-12), 1 - 1e-12)
    z <- sqrt(-2.0 * log(u1)) * cos(2.0 * pi * u2)
    v <- (1.0 + c_ * z)^3
    if (v <= 0.0) next
    u <- max(.ghc_unif(e, 1L), 1e-15)
    if (log(u) < 0.5 * z * z + d - d * v + d * log(v))
      return(d * v)
  }
}

# Public aliases (per the Python pmpfit module).
pmpfit <- morie_pmpfit
pmp_fit <- morie_pmpfit
pitman_yor <- morie_pmpfit
