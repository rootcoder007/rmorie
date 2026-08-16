# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Bayesian nonparametrics shelf. Mirrors morie.fn._ghosal and the
# fifteen gh_* modules.
#
# Collision scan: ghosal_native.R and all fifteen exported names were
# free in both R trees.
#
# Spec: Ghosal, S. and van der Vaart, A., Fundamentals of
# Nonparametric Bayesian Inference, Cambridge. Section numbers were
# checked against the printed table of contents.
#
# Two facts drive the shelf. The Dirichlet process is CONJUGATE
# (Sec. 4.1.3), so the predictive machinery is closed form; and
# contraction rates are stated as eps_n with the minimax benchmark
# n^{-s/(2s+d)} for an s-smooth density in dimension d.

# n is kept as a DOUBLE, not coerced with as.integer(): these rate
# formulas are routinely evaluated at hypothetical sample sizes like
# 1e10, which overflow R's 32-bit integer and silently become NA.
# Python's int has no such limit, so coercing here would break parity
# exactly where the asymptotics become interesting.
#' N is kept as a DOUBLE, not coerced with as.integer(): these rate
#'
#' formulas are routinely evaluated at hypothetical sample sizes like
#' 1e10, which overflow R\'s 32-bit integer and silently become NA.
#' Python\'s int has no such limit, so coercing here would break parity
#' exactly where the asymptotics become interesting.
#'
#' @param n Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{v}, as built in the body.
#' @export
.morie_gh_n <- function(n) {
  v <- as.numeric(n)
  if (!is.finite(v) || v < 2) {
    stop(sprintf("n must be at least 2, got %s.", format(n)), call. = FALSE)
  }
  v
}

#' .morie_gh_minimax_rate
#'
#' A step of the ghosal_native implementation. Called by \code{Ghosalfrsdensity}, \code{Ghosalgpdenscrt}, \code{morie_gp_density_rate} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param s Numeric; combined arithmetically in the body.
#' @param d Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.morie_gh_minimax_rate <- function(n, s, d = 1) {
  n <- .morie_gh_n(n)
  s <- as.numeric(s)
  if (s <= 0) {
    stop(sprintf("smoothness must be positive, got %g.", s),
      call. = FALSE
    )
  }
  n^(-s / (2 * s + as.numeric(d)))
}

#' .morie_gh_trapz
#'
#' A step of the ghosal_native implementation. Called by \code{.morie_gh_hellinger}, \code{Ghosalfrsdensity}, \code{morie_polya_tree_density} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param xx See Usage.
#' @param yy See Usage.
#' @return A numeric value.
#' @export
.morie_gh_trapz <- function(xx, yy) {
  sum(diff(xx) * (utils::head(yy, -1L) + utils::tail(yy, -1L)) / 2)
}

#' .morie_gh_hellinger
#'
#' A step of the ghosal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @param q Coerced to numeric by the body, with \code{as.numeric}.
#' @param grid Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.morie_gh_hellinger <- function(p, q, grid) {
  pv <- pmax(as.numeric(p), 0)
  qv <- pmax(as.numeric(q), 0)
  sqrt(0.5 * .morie_gh_trapz(as.numeric(grid), (sqrt(pv) - sqrt(qv))^2))
}

#' .morie_gh_polya_tree
#'
#' A step of the ghosal_native implementation. Called by \code{morie_polya_tree_density}, \code{morie_polya_tree_mixture}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param grid Coerced to numeric by the body, with \code{as.numeric}.
#' @param levels Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6L}.
#' @param a_fn Defaults to \code{NULL}.
#' @param lo Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param hi Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{ifelse}.
#' @export
.morie_gh_polya_tree <- function(x, grid, levels = 6L, a_fn = NULL,
                                 lo = NULL, hi = NULL) {
  xv <- as.numeric(x)
  g <- as.numeric(grid)
  m <- as.integer(levels)
  if (m < 1L) {
    stop(sprintf("levels must be at least 1, got %d.", m),
      call. = FALSE
    )
  }
  a <- if (is.null(a_fn)) function(k) as.numeric(k)^2 else a_fn
  a0 <- if (is.null(lo)) min(xv) else as.numeric(lo)
  a1 <- if (is.null(hi)) max(xv) else as.numeric(hi)
  if (a1 <= a0) {
    stop(sprintf("need lo < hi, got (%g, %g).", a0, a1),
      call. = FALSE
    )
  }
  dens <- rep(1 / (a1 - a0), length(g))
  for (lev in seq_len(m)) {
    edges <- seq(a0, a1, length.out = 2^lev + 1L)
    counts <- as.numeric(graphics::hist(xv, breaks = edges, plot = FALSE)$counts)
    am <- a(lev)
    idx <- pmin(
      pmax(findInterval(g, edges, rightmost.closed = FALSE), 1L),
      2^lev
    ) - 1L
    left <- counts[seq(1L, length(counts), by = 2L)]
    right <- counts[seq(2L, length(counts), by = 2L)]
    p_left <- (am + left) / (2 * am + left + right)
    share <- ifelse(idx %% 2L == 0L, p_left[idx %/% 2L + 1L],
      1 - p_left[idx %/% 2L + 1L]
    )
    dens <- dens * 2 * share
  }
  ifelse(g < a0 | g > a1, 0, dens)
}

# Stick-breaking: w_k = V_k prod_{j<k}(1 - V_j), V_k ~ Beta(1, alpha),
# with the last stick closing so the weights sum to one exactly.
#' Stick-breaking: w_k = V_k prod_{j<k}(1 - V_j), V_k ~ Beta(1, alpha),
#'
#' with the last stick closing so the weights sum to one exactly.
#'
#' @param alpha See Usage.
#' @param K Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_gh_stick <- function(alpha, K) {
  v <- stats::rbeta(K, 1, alpha)
  v[K] <- 1
  v * c(1, cumprod(1 - v[-K]))
}

#' Exact Dirichlet-process posterior predictive
#'
#' \eqn{p(X_{n+1} \in \cdot | X_1..X_n) = \alpha/(\alpha+n) G_0 +
#' (\alpha+n)^{-1}\sum_k n_k \delta_{X_k^*}} (Sec. 4.1.4). Closed
#' form: conjugacy (Sec. 4.1.3) makes the posterior
#' \eqn{DP(\alpha G_0 + \sum \delta_{X_i})}.
#'
#' The predictive is NOT a density -- it has an atom at every
#' distinct observed value carrying total mass \eqn{n/(\alpha+n)},
#' because a DP draw is almost surely discrete however smooth
#' \eqn{G_0} is. Mirrors \code{morie.fn.gh_dp_post_ex}.
#'
#' @param x numeric observations.
#' @param alpha positive concentration.
#' @param grid evaluation points for the continuous part.
#' @return list: grid, base_density, atoms, atom_probs, base_weight,
#'   atom_weight, n_distinct, is_density, limit_note, n, method.
#' @references Ghosal and van der Vaart, Sec. 4.1.3-4.1.4.
#' @examples
#' morie_dp_predictive(rnorm(30), alpha = 2)$atom_weight
#' @export
morie_dp_predictive <- function(x, alpha = 1, grid = NULL) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 1L) stop("need at least one observation.", call. = FALSE)
  a <- as.numeric(alpha)
  if (a <= 0) {
    stop(sprintf("alpha must be positive, got %g.", a),
      call. = FALSE
    )
  }
  tb <- table(xv)
  vals <- as.numeric(names(tb))
  counts <- as.numeric(tb)
  g <- if (is.null(grid)) {
    seq(min(xv) - 3, max(xv) + 3, length.out = 200L)
  } else {
    as.numeric(grid)
  }
  list(
    grid = g, base_weight = a / (a + n), atom_weight = n / (a + n),
    atoms = vals, atom_probs = counts / (a + n),
    base_density = stats::dnorm(g) * a / (a + n),
    n_distinct = length(vals), is_density = FALSE,
    limit_note = "alpha -> 0 gives the empirical distribution; alpha -> infinity gives G_0",
    n = n,
    method = "Polya urn predictive (Sec. 4.1.4); atoms at the distinct values, mass n/(alpha+n)"
  )
}

#' Newton's predictive recursion
#'
#' \eqn{\hat f_i(\theta) = (1-w_i)\hat f_{i-1}(\theta) + w_i
#' \psi(X_i;\theta)\hat f_{i-1}(\theta) / \int \psi(X_i;t)\hat
#' f_{i-1}(t)dt} (Sec. 5.4), estimating the MIXING density in
#' \eqn{p_F(x) = \int \psi(x;\theta)dF} -- a deconvolution.
#'
#' A single sweep, no MCMC, which is what makes it cheap. The price
#' is that the estimate DEPENDS ON THE ORDER of the observations,
#' since each step conditions on the running estimate. Weights need
#' \eqn{\sum w_i = \infty} and \eqn{\sum w_i^2 < \infty};
#' \eqn{(i+2)^{-2/3}} satisfies both and stays inside (0, 1), which
#' \eqn{(i+1)^{-2/3}} does not at the first step. Mirrors
#' \code{morie.fn.gh_c5_7}.
#'
#' @param x numeric observations from the mixture.
#' @param theta_grid support of the mixing density.
#' @param sigma positive Gaussian kernel scale.
#' @param weights the w_i, strictly inside (0, 1).
#' @param f0 initial mixing density; uniform when NULL.
#' @return list: theta_grid, f_mixing, mixed_density,
#'   order_dependent, single_pass, weight_rule, n, method.
#' @references Ghosal and van der Vaart, Sec. 5.4; Newton (2002).
#' @examples
#' morie_predictive_recursion(rnorm(50), sigma = 1)$order_dependent
#' @export
morie_predictive_recursion <- function(x, theta_grid = NULL, sigma = 1,
                                       weights = NULL, f0 = NULL) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n),
      call. = FALSE
    )
  }
  s <- as.numeric(sigma)
  if (s <= 0) {
    stop(sprintf("sigma must be positive, got %g.", s),
      call. = FALSE
    )
  }
  th <- if (is.null(theta_grid)) {
    seq(min(xv) - 2 * s, max(xv) + 2 * s, length.out = 201L)
  } else {
    as.numeric(theta_grid)
  }
  f <- if (is.null(f0)) {
    rep(1 / (max(th) - min(th)), length(th))
  } else {
    as.numeric(f0)
  }
  if (length(f) != length(th)) {
    stop("f0 must match theta_grid.", call. = FALSE)
  }
  w <- if (is.null(weights)) (seq_len(n) + 1)^(-2 / 3) else as.numeric(weights)
  if (length(w) != n) {
    stop(sprintf(
      "weights has %d entries for %d observations.",
      length(w), n
    ), call. = FALSE)
  }
  if (any(w <= 0 | w >= 1)) {
    stop("weights must lie strictly in (0, 1).", call. = FALSE)
  }
  kern <- function(xi, t) stats::dnorm((xi - t) / s) / s
  for (i in seq_len(n)) {
    like <- kern(xv[i], th)
    denom <- .morie_gh_trapz(th, like * f)
    if (denom <= 0) next
    f <- (1 - w[i]) * f + w[i] * like * f / denom
  }
  mass <- .morie_gh_trapz(th, f)
  if (mass > 0) f <- f / mass
  mixed <- vapply(
    th, function(v) .morie_gh_trapz(th, kern(v, th) * f),
    numeric(1)
  )
  list(
    theta_grid = th, f_mixing = f, mixed_density = mixed,
    order_dependent = TRUE, single_pass = TRUE,
    weight_rule = "w_i = (i+2)^{-2/3}: sum w = inf, sum w^2 < inf, and w_1 < 1",
    n = n,
    method = "Predictive recursion (Sec. 5.4); one sweep, no MCMC, order dependent"
  )
}

#' Polya tree posterior mean density
#'
#' Beta conjugacy at every node of the dyadic partition gives the
#' posterior mean density in closed form (Sec. 3.7, 7.2.3).
#'
#' \strong{The choice of a_m decides whether the prior is usable.}
#' Constant \eqn{a_m} supports distributions SINGULAR with respect to
#' Lebesgue measure, so no density exists to be consistent for;
#' growing \eqn{a_m}, with \eqn{m^2} standard, puts mass on
#' absolutely continuous laws and the posterior is then
#' Hellinger-consistent at any Lipschitz density. Mirrors
#' \code{morie.fn.gh_c7_6}.
#'
#' @param x numeric observations.
#' @param grid evaluation points.
#' @param levels depth of the dyadic partition.
#' @param a_scale positive multiplier on \eqn{a_m = a\_scale\,m^2}.
#' @param lo,hi bounds of the partitioned interval.
#' @return list: grid, density, levels, a_rule,
#'   absolutely_continuous_prior, mass, consistent_at, n, method.
#' @references Ghosal and van der Vaart, Sec. 3.7 and 7.2.3.
#' @examples
#' morie_polya_tree_density(rnorm(200), levels = 4)$absolutely_continuous_prior
#' @export
morie_polya_tree_density <- function(x, grid = NULL, levels = 6L,
                                     a_scale = 1, lo = NULL, hi = NULL) {
  xv <- as.numeric(x)
  if (length(xv) < 4L) {
    stop(sprintf("need at least 4 observations, got %d.", length(xv)),
      call. = FALSE
    )
  }
  sc <- as.numeric(a_scale)
  if (sc <= 0) {
    stop(sprintf("a_scale must be positive, got %g.", sc),
      call. = FALSE
    )
  }
  a0 <- if (is.null(lo)) min(xv) else as.numeric(lo)
  a1 <- if (is.null(hi)) max(xv) else as.numeric(hi)
  g <- if (is.null(grid)) seq(a0, a1, length.out = 200L) else as.numeric(grid)
  dens <- .morie_gh_polya_tree(xv, g,
    levels = levels,
    a_fn = function(m) sc * m^2, lo = a0, hi = a1
  )
  list(
    grid = g, density = dens, levels = as.integer(levels),
    a_rule = "a_m = a_scale * m^2 (growing, so the prior is on densities)",
    absolutely_continuous_prior = TRUE,
    mass = .morie_gh_trapz(g, dens),
    consistent_at = "any Lipschitz density, in Hellinger distance",
    n = length(xv),
    method = "Polya tree posterior mean (Sec. 7.2.3); closed form by Beta conjugacy"
  )
}

#' Mixture of Polya trees
#'
#' \eqn{f \sim \int PT(\alpha, \pi) dH(\alpha, \pi)} (Sec. 3.7.2): a
#' Polya tree with the PARTITION itself given a prior.
#'
#' This removes an artefact rather than adding flexibility. A single
#' Polya tree is tied to a fixed dyadic partition and its posterior
#' mean has visible DISCONTINUITIES at the partition boundaries --
#' a property of the tessellation, not of the data. Mixing over the
#' partition averages those breaks away. Mirrors
#' \code{morie.fn.gh_c3_14}.
#'
#' @param x numeric observations.
#' @param grid evaluation points.
#' @param levels depth of each partition.
#' @param a_scale positive multiplier on \eqn{a_m}.
#' @param shifts partition offsets as fractions of a cell width.
#' @return list: grid, density, n_components, max_jump,
#'   max_jump_single, smoother_than_single, n, method.
#' @references Ghosal and van der Vaart, Sec. 3.7.2.
#' @examples
#' morie_polya_tree_mixture(rnorm(200), levels = 4)$smoother_than_single
#' @export
morie_polya_tree_mixture <- function(x, grid = NULL, levels = 6L,
                                     a_scale = 1, shifts = NULL) {
  xv <- as.numeric(x)
  if (length(xv) < 4L) {
    stop(sprintf("need at least 4 observations, got %d.", length(xv)),
      call. = FALSE
    )
  }
  sc <- as.numeric(a_scale)
  if (sc <= 0) {
    stop(sprintf("a_scale must be positive, got %g.", sc),
      call. = FALSE
    )
  }
  lo <- min(xv)
  hi <- max(xv)
  span <- hi - lo
  if (span <= 0) stop("the sample has zero spread.", call. = FALSE)
  g <- if (is.null(grid)) seq(lo, hi, length.out = 200L) else as.numeric(grid)
  sh <- if (is.null(shifts)) seq(0, 0.5, length.out = 8L) else as.numeric(shifts)
  cell <- span / (2^as.integer(levels))
  comps <- vapply(sh, function(s) {
    off <- s * cell
    .morie_gh_polya_tree(xv, g,
      levels = levels,
      a_fn = function(m) sc * m^2,
      lo = lo - off, hi = hi + (cell - off)
    )
  }, numeric(length(g)))
  dens <- rowMeans(comps)
  list(
    grid = g, density = dens, n_components = length(sh),
    max_jump = max(abs(diff(dens))),
    max_jump_single = max(abs(diff(comps[, 1L]))),
    smoother_than_single = max(abs(diff(dens))) <= max(abs(diff(comps[, 1L]))),
    n = length(xv),
    method = "Mixture of Polya trees (Sec. 3.7.2); averages away the partition artefacts"
  )
}

#' Adaptive Polya tree rate
#'
#' \eqn{\varepsilon_n \asymp n^{-s/(2s+1)}\log n} for an s-smooth
#' \eqn{p_0} (Sec. 7.2.3). The prior \eqn{a_m = m^2} attains this for
#' every s WITHOUT being told s; the logarithm is exactly the price
#' of that adaptation, and is returned separately so "near-optimal"
#' is a number. Mirrors \code{morie.fn.gh_pt_adapt}.
#'
#' @param x numeric observations, for the sample size.
#' @param s smoothness to report at.
#' @param n sample size to evaluate at.
#' @param levels,a_scale recorded for the corresponding fit.
#' @return list: n, smoothness, rate, minimax_rate, log_factor,
#'   ratio_to_minimax, adaptive, requires_knowing_s, scan, method.
#' @references Ghosal and van der Vaart, Sec. 7.2.3; Ch. 10.
#' @examples
#' morie_polya_tree_rate(numeric(5), s = 2, n = 10000)$ratio_to_minimax
#' @export
morie_polya_tree_rate <- function(x, s = NULL, n = NULL, levels = 6L,
                                  a_scale = 1) {
  nn <- if (is.null(n)) length(as.numeric(x)) else n
  nn <- .morie_gh_n(nn)
  lg <- log(nn)
  sv <- if (is.null(s)) 1 else as.numeric(s)
  if (sv <= 0) {
    stop(sprintf("smoothness must be positive, got %g.", sv),
      call. = FALSE
    )
  }
  mm <- .morie_gh_minimax_rate(nn, sv)
  scan <- lapply(c(0.5, 1, 1.5, 2, 3), function(v) {
    c(v, .morie_gh_minimax_rate(nn, v) * lg)
  })
  list(
    n = nn, smoothness = sv, rate = mm * lg, minimax_rate = mm,
    log_factor = lg, ratio_to_minimax = lg, adaptive = TRUE,
    requires_knowing_s = FALSE, scan = scan,
    a_rule = sprintf("a_m = %g * m^2", as.numeric(a_scale)),
    levels = as.integer(levels),
    method = "Adaptive Polya tree: n^{-s/(2s+1)} log n for every s, without knowing s"
  )
}

#' i.i.d. posterior contraction theorem
#'
#' A rate \eqn{\varepsilon_n} holds when THREE conditions hold
#' together: entropy \eqn{\lesssim n\varepsilon_n^2}, prior mass
#' \eqn{\ge e^{-Cn\varepsilon_n^2}}, and a sieve remainder
#' (Sec. 8.2). All three are calibrated by the same
#' \eqn{n\varepsilon_n^2}, and that is the content -- neither alone
#' gives a rate. Mirrors \code{morie.fn.gh_c8_6}.
#'
#' @param x numeric observations, for the sample size.
#' @param eps candidate rate.
#' @param n sample size.
#' @param prior_mass,entropy measured quantities, when available.
#' @param C constant in the prior-mass condition.
#' @return list: n, eps, n_eps_squared, entropy_budget,
#'   prior_mass_budget, entropy_ok, prior_mass_ok,
#'   all_conditions_checked, metric, conditions, method.
#' @references Ghosal and van der Vaart, Sec. 8.2; Ghosal, Ghosh and
#'   van der Vaart (2000).
#' @examples
#' morie_contraction_conditions(numeric(9), eps = 0.1, n = 1000)$n_eps_squared
#' @export
morie_contraction_conditions <- function(x, eps = NULL, n = NULL,
                                         prior_mass = NULL, entropy = NULL,
                                         C = 1) {
  nn <- if (is.null(n)) length(as.numeric(x)) else n
  nn <- .morie_gh_n(nn)
  e <- if (is.null(eps)) nn^(-1 / 3) else as.numeric(eps)
  if (e <= 0) stop(sprintf("eps must be positive, got %g.", e), call. = FALSE)
  ne2 <- nn * e * e
  ent_ok <- if (is.null(entropy)) NULL else as.numeric(entropy) <= ne2
  pm_ok <- if (is.null(prior_mass)) {
    NULL
  } else {
    as.numeric(prior_mass) >= exp(-as.numeric(C) * ne2)
  }
  list(
    n = nn, eps = e, n_eps_squared = ne2, entropy_budget = ne2,
    prior_mass_budget = exp(-as.numeric(C) * ne2),
    entropy_ok = ent_ok, prior_mass_ok = pm_ok,
    all_conditions_checked = !is.null(ent_ok) && !is.null(pm_ok),
    metric = "Hellinger",
    conditions = paste(
      "entropy <= n eps^2; prior mass >= exp(-C n eps^2);",
      "sieve remainder o(exp(-(C+4) n eps^2))"
    ),
    method = "i.i.d. contraction theorem (Sec. 8.2); all three conditions calibrated by n eps^2"
  )
}

#' Gaussian-process density contraction rate
#'
#' The rate follows from the concentration function, whose two terms
#' -- an RKHS approximation term and a small-ball probability -- are
#' balanced at \eqn{\varphi(\varepsilon_n) \le n\varepsilon_n^2}
#' (Sec. 11.3.1). The kernel decides the answer: Matern matched to
#' the truth attains the minimax rate, while a SQUARED-EXPONENTIAL
#' process has analytic sample paths and is far too smooth,
#' contracting only logarithmically unless rescaled.
#'
#' That contrast is asymptotic: at \eqn{n = 10^4, s = 1} the
#' logarithmic rate is numerically SMALLER than the rescaled
#' polynomial one. \code{ratio_to_minimax} is the honest summary --
#' it diverges, the rate value alone does not. Mirrors
#' \code{morie.fn.gh_c11_4}.
#'
#' @param x numeric observations, for the sample size.
#' @param s smoothness of the log-density.
#' @param n sample size.
#' @param kernel "squared_exponential", "matern" or "rescaled_se".
#' @return list: n, smoothness, kernel, rate, minimax_rate,
#'   attains_minimax, ratio_to_minimax, rate_kind, link, driver,
#'   method.
#' @references Ghosal and van der Vaart, Sec. 11.3.1 and Ch. 11.
#' @examples
#' morie_gp_density_rate(numeric(5), s = 1, n = 10000)$rate_kind
#' @export
morie_gp_density_rate <- function(x, s = NULL, n = NULL,
                                  kernel = "squared_exponential") {
  nn <- if (is.null(n)) length(as.numeric(x)) else n
  nn <- .morie_gh_n(nn)
  sv <- if (is.null(s)) 1 else as.numeric(s)
  if (sv <= 0) {
    stop(sprintf("smoothness must be positive, got %g.", sv),
      call. = FALSE
    )
  }
  if (!kernel %in% c("squared_exponential", "matern", "rescaled_se")) {
    stop("kernel must be 'squared_exponential', 'matern' or 'rescaled_se'.",
      call. = FALSE
    )
  }
  mm <- .morie_gh_minimax_rate(nn, sv)
  if (kernel == "matern") {
    rate <- mm
    kind <- "polynomial (minimax)"
    attains <- TRUE
  } else if (kernel == "rescaled_se") {
    rate <- mm * log(nn)^((sv + 1) / (2 * sv + 1))
    kind <- "polynomial up to a log factor"
    attains <- FALSE
  } else {
    rate <- log(nn)^(-sv)
    kind <- "LOGARITHMIC"
    attains <- FALSE
  }
  list(
    n = nn, smoothness = sv, kernel = kernel, rate = rate,
    minimax_rate = mm, attains_minimax = attains,
    ratio_to_minimax = rate / mm, rate_kind = kind,
    link = "f = exp(psi) / int exp(psi): positivity and normalisation for free",
    driver = "the concentration function: RKHS approximation + small-ball probability",
    method = "GP density contraction (Sec. 11.3.1); the kernel's smoothness decides the rate"
  )
}

#' Dirichlet-process survival and the Kaplan-Meier limit
#'
#' The DP posterior mean survival function interpolates between the
#' prior and the data and converges to the KAPLAN-MEIER estimator as
#' \eqn{\alpha \to 0} (Sec. 13.2) -- the nonparametric Bayes answer
#' becomes the classical frequentist one in the vanishing-prior
#' limit. The maximum discrepancy is returned so the convergence is
#' measured. Mirrors \code{morie.fn.gh_c13_2}.
#'
#' @param x observed times, non-negative.
#' @param event 1 for an event, 0 for right-censoring.
#' @param alpha positive DP concentration.
#' @param g0_rate rate of the exponential base measure.
#' @return list: times, survival_dp, survival_km, max_abs_diff_to_km,
#'   alpha, limit_note, n_events, n, method.
#' @references Ghosal and van der Vaart, Sec. 13.2; Susarla and
#'   Van Ryzin (1976).
#' @examples
#' morie_dp_survival(rexp(50), alpha = 0.01)$max_abs_diff_to_km
#' @export
morie_dp_survival <- function(x, event = NULL, alpha = 1, g0_rate = NULL) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n),
      call. = FALSE
    )
  }
  if (any(xv < 0)) stop("times must be non-negative.", call. = FALSE)
  ev <- if (is.null(event)) rep(1, n) else as.numeric(event)
  if (length(ev) != n) {
    stop(sprintf("event has %d entries for %d times.", length(ev), n),
      call. = FALSE
    )
  }
  if (!all(ev %in% c(0, 1))) stop("event must be binary 0/1.", call. = FALSE)
  a <- as.numeric(alpha)
  if (a <= 0) {
    stop(sprintf("alpha must be positive, got %g.", a),
      call. = FALSE
    )
  }
  uniq <- sort(unique(xv))
  km <- numeric(length(uniq))
  surv <- 1
  for (i in seq_along(uniq)) {
    t <- uniq[i]
    at_risk <- sum(xv >= t)
    deaths <- sum(xv == t & ev == 1)
    if (at_risk > 0 && deaths > 0) surv <- surv * (1 - deaths / at_risk)
    km[i] <- surv
  }
  rate <- if (is.null(g0_rate)) 1 / max(mean(xv), 1e-12) else as.numeric(g0_rate)
  wt <- n / (a + n)
  dp <- wt * km + (1 - wt) * exp(-rate * uniq)
  list(
    times = uniq, survival_dp = dp, survival_km = km,
    max_abs_diff_to_km = max(abs(dp - km)), alpha = a,
    limit_note = "alpha -> 0 gives Kaplan-Meier exactly; alpha -> infinity gives the base measure",
    n_events = sum(ev), n = n,
    method = "DP posterior survival (Sec. 13.2); Kaplan-Meier is the alpha -> 0 limit"
  )
}

#' Empirical Bayes for the Dirichlet concentration
#'
#' \eqn{\hat\alpha} maximises the marginal likelihood, which under
#' the Polya urn depends on the data only through the PARTITION:
#' \eqn{k\log\alpha + \log\Gamma(\alpha) - \log\Gamma(\alpha+n)}. So
#' \eqn{\hat\alpha} is driven by the number of clusters. Plugging it
#' back in treats an estimated hyperparameter as known and can
#' under-cover, which \code{understates_uncertainty} records.
#' Mirrors \code{morie.fn.gh_emp_bayes}.
#'
#' @param x numeric observations; ties define the clusters.
#' @param alpha_grid positive concentrations searched over.
#' @param sigma retained for interface symmetry.
#' @return list: alpha_hat, alpha_grid, log_marginal, n_clusters,
#'   understates_uncertainty, fully_bayes_alternative, n, method.
#' @references Ghosal and van der Vaart, Ch. 4 and Ch. 6;
#'   Antoniak (1974).
#' @examples
#' morie_empirical_bayes_dp(rep(1:3, each = 20))$n_clusters
#' @export
morie_empirical_bayes_dp <- function(x, alpha_grid = NULL, sigma = 1) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n),
      call. = FALSE
    )
  }
  k <- length(unique(xv))
  ag <- if (is.null(alpha_grid)) {
    10^seq(-2, 2, length.out = 200L)
  } else {
    as.numeric(alpha_grid)
  }
  if (any(ag <= 0)) stop("alpha values must be positive.", call. = FALSE)
  lm <- k * log(ag) + lgamma(ag) - lgamma(ag + n)
  list(
    alpha_hat = ag[which.max(lm)], alpha_grid = ag, log_marginal = lm,
    n_clusters = k, understates_uncertainty = TRUE,
    fully_bayes_alternative = "put a prior on alpha and integrate it out",
    n = n,
    method = "Empirical Bayes for the DP concentration; the marginal depends on the CLUSTER count"
  )
}
