# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of bnppvl -- nonparametric Bayes predictive value of a new
# observation, via the Beta quantile pyramid. Mirrors
# src/morie/fn/bnppvl.py operation for operation, on the shared numerics
# in R/aaa_helpers_w3num.R and the matched random stream in
# R/aaa_helpers_ghc_rng.R.
#
# A quantile pyramid turns the usual nonparametric Bayes construction
# inside out. A Polya tree fixes the partition of the sample space and
# puts random mass on it; a quantile pyramid fixes the MASS -- a half,
# two quarters, four eighths -- and lets the partition be random. The
# arbitrariness of "which partition?" disappears, because the cut points
# are the median, the quartiles, the octiles, which are the same objects
# whatever the scale.
#
# The construction is a recursion down the dyadic rationals. Writing
# Q(y) for the quantile function, level m fills in the odd dyadics from
# the level above it:
#
#     Q_m(j/2^m) = Q_{m-1}((j-1)/2^m) (1 - V) + Q_{m-1}((j+1)/2^m) V
#
# for j = 1, 3, ..., 2^m - 1, so each new quantile lands somewhere
# strictly between the two that bracket it, at a random point V of the
# way across. The Beta quantile pyramid takes V symmetric Beta with
# concentration a_m growing with the level -- a_m = c m^3 is the paper's
# example -- which is what makes the limit absolutely continuous rather
# than a singular mess. Centring the prior somewhere other than uniform
# is a shift of the MEAN of V, and the paper gives that mean exactly:
# the fraction of the parent interval that the prior guess assigns to
# the left child.
#
# Down at level m the quantile function is linear between the 2^m cut
# points, so the density is a histogram with fixed cell probabilities
# 1/k and random cell widths, and that is the likelihood. Two of them,
# in fact, and they are not the same inference: the exact one, a product
# of cell densities; and Jeffreys' substitution likelihood, the
# multinomial probability of the observed counts under equal cell
# probabilities. The second is NOT the conditional distribution of the
# data given any statistic and is known to be conservative -- a reason
# to offer it, not a reason to hide it.
#
# What comes out is the thing the question actually asks for: the
# predictive distribution of a new observation, with its mean and
# standard deviation computed from the cellwise moments of the histogram
# rather than from the draws' locations, so they are exact given the
# draws instead of a second Monte Carlo approximation stacked on the
# first.
#
# References
#   Hjort, N.L. and Walker, S.G. (2009) "Quantile pyramids for Bayesian
#     nonparametrics." The Annals of Statistics 37(1), 105-131.
#     doi:10.1214/07-AOS553. The construction (equation 3), the Beta
#     pyramid and the a_m = c m^3 schedule (section 4.1), the centring
#     identity (equation 6), the random-histogram density (equation 9),
#     the exact likelihood (equation 10), the multinomial substitute
#     likelihood (equation 11), the factorised prior (equation 15) and
#     both Metropolis-Hastings acceptance ratios (section 6).
#   Jeffreys, H. (1967) "Theory of Probability," 3rd edition. Oxford
#     University Press, chapter 4.
#   Lavine, M. (1995) "On an approximate likelihood for quantiles."
#     Biometrika 82(1), 220-222.
#   Ferguson, T.S. (1974) "Prior distributions on spaces of probability
#     measures." The Annals of Statistics 2(4), 615-629.

.BNPPVL_LIKELIHOODS <- c("exact", "substitute")
.BNPPVL_CENTRINGS <- c("uniform", "null")
.BNPPVL_SCHEDULES <- c("cubic", "constant")
.BNPPVL_INITS <- c("prior", "empirical")

# The Beta concentration a_m at a given level. "cubic" is the paper's
# a_m = c m^3, for which the sum of 1/sqrt(a_m) converges and the limit
# is absolutely continuous. "constant" holds a_m fixed, which does NOT:
# the limit is continuous but singular, and it is here because a reader
# who wants to see that happen should be able to.
#' The Beta concentration a_m at a given level. "cubic" is the paper\'s
#'
#' a_m = c m^3, for which the sum of 1/sqrt(a_m) converges and the limit
#' is absolutely continuous. "constant" holds a_m fixed, which does NOT:
#' the limit is continuous but singular, and it is here because a reader
#' who wants to see that happen should be able to.
#'
#' @param level Numeric; combined arithmetically in the body.
#' @param c Coerced to numeric by the body, with \code{as.numeric}.
#' @param schedule One of \code{"constant"}, \code{"cubic"}.
#' @return Nothing; this branch always raises.
#' @export
.bnppvl_conc <- function(level, c, schedule) {
  if (schedule == "cubic") return(as.numeric(c) * level * level * level)
  if (schedule == "constant") return(as.numeric(c))
  stop("schedule must be one of ", paste(.BNPPVL_SCHEDULES, collapse = ", "))
}

# The mean of V that centres the pyramid on a prior guess: the fraction
# of the parent interval that the guess assigns to the left child, which
# is the paper's equation (6). A guess that is flat over the parent
# gives exactly a half, the symmetric case.
#' The mean of V that centres the pyramid on a prior guess: the fraction
#'
#' of the parent interval that the guess assigns to the left child,
#' which is the paper\'s equation (6). A guess that is flat over the
#' parent gives exactly a half, the symmetric case.
#'
#' @param nullq Accepted by the signature and not used anywhere in the body.
#' @param j Numeric; combined arithmetically in the body.
#' @param level Passed to \code{bitwShiftL}.
#' @return A numeric value.
#' @export
.bnppvl_null_mean <- function(nullq, j, level) {
  d <- 1 / bitwShiftL(1L, level)
  a <- nullq((j - 1) * d)
  b <- nullq(j * d)
  cc <- nullq((j + 1) * d)
  wide <- cc - a
  if (wide <= 0)
    stop("the centring quantile function is not strictly increasing on ",
         "the dyadic grid")
  (b - a) / wide
}

#' .bnppvl_ab
#'
#' A step of the bnppvl_native implementation. Called by \code{morie_bnppvl_draw},
#' \code{morie_bnppvl_log_prior}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param level Passed to \code{.bnppvl_conc}.
#' @param c Passed to \code{.bnppvl_conc}.
#' @param schedule Passed to \code{.bnppvl_conc}.
#' @param centring Compared against \code{"uniform"}.
#' @param nullq Passed to \code{.bnppvl_null_mean}.
#' @param j Passed to \code{.bnppvl_null_mean}.
#' @return A vector, from \code{c}.
#' @export
.bnppvl_ab <- function(level, c, schedule, centring, nullq, j) {
  a_m <- .bnppvl_conc(level, c, schedule)
  if (a_m <= 0) stop("the concentration must be positive")
  if (centring == "uniform") return(c(0.5 * a_m, 0.5 * a_m))
  mu <- .bnppvl_null_mean(nullq, j, level)
  if (!(mu > 0 && mu < 1))
    stop("the centring puts a node on the edge of its parent interval")
  c(a_m * mu, a_m * (1 - mu))
}

#' One draw of the quantile pyramid down to level m
#'
#' Levels are filled in order and, within a level, the odd nodes left to
#' right, so two implementations that consume the same random stream in
#' that order produce the same pyramid.
#'
#' @param e A random stream from the shared generator.
#' @param m Pyramid depth.
#' @param c Concentration scale.
#' @param schedule A member of the schedule list.
#' @param centring A member of the centring list.
#' @param nullq The centring quantile function, or NULL.
#' @return The full dyadic grid of length 2^m + 1, with 0 and 1 at the
#'   ends.
#' @export
morie_bnppvl_draw <- function(e, m, c = 2.5, schedule = "cubic",
                              centring = "uniform", nullq = NULL) {
  m <- as.integer(m)
  if (m < 1L) stop("the pyramid needs at least one level")
  k <- bitwShiftL(1L, m)
  q <- numeric(k + 1L)
  q[k + 1L] <- 1
  for (level in seq_len(m)) {
    step <- bitwShiftL(1L, m - level)
    j <- 1L
    while (j < bitwShiftL(1L, level)) {
      i <- j * step
      ab <- .bnppvl_ab(level, c, schedule, centring, nullq, j)
      v <- .ghc_beta1(e, ab[1], ab[2])
      q[i + 1L] <- q[i - step + 1L] * (1 - v) + q[i + step + 1L] * v
      j <- j + 2L
    }
  }
  q
}

#' .bnppvl_log_beta
#'
#' A step of the bnppvl_native implementation. Called by \code{morie_bnppvl_log_prior}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{log}.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.bnppvl_log_beta <- function(v, a, b) {
  if (!(v > 0 && v < 1)) return(-Inf)
  (a - 1) * log(v) + (b - 1) * log1p(-v) + .w3_lgamma(a + b) -
    .w3_lgamma(a) - .w3_lgamma(b)
}

#' The log prior density of a pyramid, factorised level by level
#'
#' Each node contributes the density of its own V plus the Jacobian of
#' the map from V to the node's position, which is one over the width of
#' the parent interval. That Jacobian is the piece it is easy to drop,
#' and dropping it silently tilts every acceptance ratio.
#'
#' @param q The dyadic grid.
#' @param m Pyramid depth.
#' @param c Concentration scale.
#' @param schedule A member of the schedule list.
#' @param centring A member of the centring list.
#' @param nullq The centring quantile function, or NULL.
#' @return The log prior density.
#' @export
morie_bnppvl_log_prior <- function(q, m, c = 2.5, schedule = "cubic",
                                   centring = "uniform", nullq = NULL) {
  m <- as.integer(m)
  terms <- numeric(0)
  for (level in seq_len(m)) {
    step <- bitwShiftL(1L, m - level)
    j <- 1L
    while (j < bitwShiftL(1L, level)) {
      i <- j * step
      lo <- q[i - step + 1L]
      hi <- q[i + step + 1L]
      wide <- hi - lo
      if (wide <= 0 || !(q[i + 1L] > lo && q[i + 1L] < hi)) return(-Inf)
      ab <- .bnppvl_ab(level, c, schedule, centring, nullq, j)
      terms <- c(terms,
                 .bnppvl_log_beta((q[i + 1L] - lo) / wide, ab[1], ab[2]) -
                   log(wide))
      j <- j + 2L
    }
  }
  .w3_csum(terms)
}

# The index j in 1..k of the cell holding u, ties to the left cell. A
# linear scan, not a bisection: k is 2^m with m small by construction,
# and a scan visits the cells in one fixed order in both arms.
#' The index j in 1..k of the cell holding u, ties to the left cell. A
#'
#' linear scan, not a bisection: k is 2^m with m small by construction,
#' and a scan visits the cells in one fixed order in both arms.
#'
#' @param u Passed to \code{<=}.
#' @param q A vector; indexed elementwise.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{k}, as built in the body.
#' @export
.bnppvl_cell <- function(u, q, k) {
  for (j in seq_len(k)) if (u <= q[j + 1L]) return(j)
  k
}

#' Counts of the observations falling in each of the k cells
#'
#' @param u Observations mapped onto the unit interval.
#' @param q The dyadic grid.
#' @return An integer vector of length k.
#' @export
morie_bnppvl_counts <- function(u, q) {
  k <- length(q) - 1L
  n <- integer(k)
  for (v in u) {
    j <- .bnppvl_cell(v, q, k)
    n[j] <- n[j] + 1L
  }
  n
}

#' The log likelihood of the cell configuration
#'
#' The exact version is the product of the random-histogram densities.
#' The substitute version is the multinomial probability of the counts
#' under equal cell probabilities, which does not depend on the widths
#' at all -- and that is precisely the property that makes it
#' conservative.
#'
#' @param u Observations mapped onto the unit interval.
#' @param q The dyadic grid.
#' @param kind A member of the likelihood list.
#' @return The log likelihood.
#' @export
morie_bnppvl_loglik <- function(u, q, kind = "exact") {
  if (!(kind %in% .BNPPVL_LIKELIHOODS))
    stop("kind must be one of ", paste(.BNPPVL_LIKELIHOODS, collapse = ", "))
  k <- length(q) - 1L
  cnt <- morie_bnppvl_counts(u, q)
  n <- sum(cnt)
  if (kind == "exact") {
    terms <- numeric(0)
    for (j in seq_len(k)) if (cnt[j] > 0L) {
      w <- q[j + 1L] - q[j]
      if (w <= 0) return(-Inf)
      terms <- c(terms, cnt[j] * (-log(as.numeric(k)) - log(w)))
    }
    return(.w3_csum(terms))
  }
  terms <- c(.w3_lgamma(n + 1), -n * log(as.numeric(k)))
  for (j in seq_len(k)) terms <- c(terms, -.w3_lgamma(cnt[j] + 1))
  .w3_csum(terms)
}

# Cellwise first and second moments of the random histogram. The mean of
# a cell is its midpoint and the second moment is (a^2 + ab + b^2)/3,
# because the histogram is uniform inside the cell. Taking these in
# closed form rather than from the sampled positions is what keeps the
# predictive moments exact given the draws.
#' Cellwise first and second moments of the random histogram. The mean
#' of
#'
#' a cell is its midpoint and the second moment is (a^2 + ab + b^2)/3,
#' because the histogram is uniform inside the cell. Taking these in
#' closed form rather than from the sampled positions is what keeps the
#' predictive moments exact given the draws.
#'
#' @param draws A vector; its length is taken and its elements indexed.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{m1}, \code{m2}.
#' @export
.bnppvl_moments <- function(draws, k) {
  m1 <- numeric(length(draws))
  m2 <- numeric(length(draws))
  for (d in seq_along(draws)) {
    q <- draws[[d]]
    t1 <- numeric(k)
    t2 <- numeric(k)
    for (j in seq_len(k)) {
      a <- q[j]
      b <- q[j + 1L]
      t1[j] <- 0.5 * (a + b) / k
      t2[j] <- (a * a + a * b + b * b) / (3 * k)
    }
    m1[d] <- .w3_csum(t1)
    m2[d] <- .w3_csum(t2)
  }
  list(m1 = m1, m2 = m2)
}

#' .bnppvl_density_at
#'
#' A step of the bnppvl_native implementation. Called by \code{morie_bnppvl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param u Passed to \code{.bnppvl_cell}.
#' @param q A vector; indexed elementwise.
#' @param k Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.bnppvl_density_at <- function(u, q, k) {
  j <- .bnppvl_cell(u, q, k)
  w <- q[j + 1L] - q[j]
  if (w <= 0) 0 else 1 / (k * w)
}

#' .bnppvl_cdf_at
#'
#' A step of the bnppvl_native implementation. Called by \code{morie_bnppvl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param u Numeric; combined arithmetically in the body.
#' @param q A vector; indexed elementwise.
#' @param k Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.bnppvl_cdf_at <- function(u, q, k) {
  if (u <= 0) return(0)
  if (u >= 1) return(1)
  j <- .bnppvl_cell(u, q, k)
  w <- q[j + 1L] - q[j]
  if (w <= 0) return((j - 1) / k)
  (j - 1) / k + (u - q[j]) / (k * w)
}

# A starting pyramid halfway between the data and the uniform one. The
# empirical quantiles alone will not do: with ties, or with fewer
# observations than cells, two of them can coincide and a quantile
# function with a zero-width cell is not a quantile function at all.
# Averaging with the uniform grid guarantees a strict increase of at
# least one over twice the cell count while still starting the chain
# where the data are, which is worth a great many sweeps of a
# single-site sampler.
#' A starting pyramid halfway between the data and the uniform one. The
#'
#' empirical quantiles alone will not do: with ties, or with fewer
#' observations than cells, two of them can coincide and a quantile
#' function with a zero-width cell is not a quantile function at all.
#' Averaging with the uniform grid guarantees a strict increase of at
#' least one over twice the cell count while still starting the chain
#' where the data are, which is worth a great many sweeps of a
#' single-site sampler.
#'
#' @param u A vector; its length is taken.
#' @param k Numeric; combined arithmetically in the body.
#' @return The value of \code{q}, as built in the body.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .bnppvl_empirical_start(u = x, k = 3L)
#' res
.bnppvl_empirical_start <- function(u, k) {
  n <- length(u)
  su <- sort(u, method = "radix")
  q <- numeric(k + 1L)
  q[k + 1L] <- 1
  for (j in seq_len(k - 1L)) {
    idx <- ceiling(j * n / k)
    if (idx < 1) idx <- 1L
    if (idx > n) idx <- n
    q[j + 1L] <- 0.5 * (su[idx] + j / k)
  }
  q
}

#' Fit a Beta quantile pyramid and report the predictive for a new draw
#'
#' @param x The observations, which must lie inside the support.
#' @param m Pyramid depth; the histogram has k = 2^m cells.
#' @param c Concentration scale. Larger holds the pyramid closer to its
#'   centring.
#' @param schedule A member of the schedule list.
#' @param centring A member of the centring list; "null" centres on the
#'   given quantile function.
#' @param nullq The centring quantile function on the unit interval,
#'   strictly increasing, or NULL.
#' @param likelihood A member of the likelihood list.
#' @param lo Lower end of the support.
#' @param hi Upper end of the support.
#' @param sweeps Metropolis-Hastings sweeps in total.
#' @param burn Sweeps discarded.
#' @param thin Keep-every rate.
#' @param seed The random stream.
#' @param init A member of the init list. "prior" starts the chain at a
#'   draw from the prior, which is what the paper describes;
#'   "empirical" starts it near the data, which reaches the same
#'   posterior in far fewer sweeps of a single-site sampler.
#' @param grid Points at which the predictive density and distribution
#'   function are reported, or NULL.
#' @param probs Predictive quantile levels.
#' @return A list with the posterior mean quantile function, the
#'   predictive mean and standard deviation of a new observation, the
#'   predictive density and distribution function on the grid, the
#'   predictive quantiles, and the acceptance rate.
#' @export
morie_bnppvl <- function(x, m = 4L, c = 2.5, schedule = "cubic",
                         centring = "uniform", nullq = NULL,
                         likelihood = "exact", lo = 0, hi = 1,
                         sweeps = 400L, burn = 100L, thin = 2L, seed = 0,
                         init = "prior", grid = NULL,
                         probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  if (!(likelihood %in% .BNPPVL_LIKELIHOODS))
    stop("likelihood must be one of ",
         paste(.BNPPVL_LIKELIHOODS, collapse = ", "))
  if (!(centring %in% .BNPPVL_CENTRINGS))
    stop("centring must be one of ",
         paste(.BNPPVL_CENTRINGS, collapse = ", "))
  if (!(schedule %in% .BNPPVL_SCHEDULES))
    stop("schedule must be one of ",
         paste(.BNPPVL_SCHEDULES, collapse = ", "))
  if (!(init %in% .BNPPVL_INITS))
    stop("init must be one of ", paste(.BNPPVL_INITS, collapse = ", "))
  if (centring == "null" && is.null(nullq))
    stop("centring on a null distribution needs nullq")
  m <- as.integer(m)
  if (m < 1L) stop("the pyramid needs at least one level")
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)
  if (hi <= lo) stop("hi must exceed lo")
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one observation")
  if (any(xs < lo | xs > hi))
    stop("every observation must lie inside [lo, hi]")
  sweeps <- as.integer(sweeps)
  burn <- as.integer(burn)
  thin <- as.integer(thin)
  if (sweeps < 1L || burn < 0L || burn >= sweeps || thin < 1L)
    stop("need 0 <= burn < sweeps and thin >= 1")
  k <- bitwShiftL(1L, m)
  u <- (xs - lo) / (hi - lo)
  n <- length(u)

  e <- .ghc_rng(seed)
  q <- if (init == "prior")
    morie_bnppvl_draw(e, m, c, schedule, centring, nullq)
  else .bnppvl_empirical_start(u, k)
  lp <- morie_bnppvl_log_prior(q, m, c, schedule, centring, nullq)
  cnt <- morie_bnppvl_counts(u, q)

  draws <- list()
  tried <- 0L
  taken <- 0L
  for (it in seq_len(sweeps)) {
    for (j in seq_len(k - 1L)) {
      a <- q[j]
      b <- q[j + 2L]
      prop <- a + (b - a) * .ghc_unif(e, 1L)
      tried <- tried + 1L
      if (!(prop > a && prop < b)) {
        # A proposal that lands exactly on a neighbour is not a valid
        # quantile function; reject rather than divide by a zero width.
        .ghc_unif(e, 1L)
        next
      }
      qq <- q
      qq[j + 1L] <- prop
      lpp <- morie_bnppvl_log_prior(qq, m, c, schedule, centring, nullq)
      # Only the two cells either side of j change, so only their counts
      # have to be recomputed.
      left <- 0L
      right <- 0L
      for (v in u) {
        # The leftmost cell is CLOSED at its lower end, exactly as the
        # cell lookup treats it. Writing a strict inequality here loses
        # every observation sitting on the lower bound of the support,
        # and the likelihood then never squeezes the first cell.
        if ((v > a || j == 1L) && v <= prop) left <- left + 1L
        else if (v > prop && v <= b) right <- right + 1L
      }
      if (likelihood == "exact") {
        # The paper's ratio: the CURRENT widths raised to the current
        # counts over the proposed widths raised to the proposed counts,
        # because the density is one over the width.
        num <- cnt[j] * log(q[j + 1L] - a) +
          cnt[j + 1L] * log(b - q[j + 1L]) + lpp
        den <- left * log(prop - a) + right * log(b - prop) + lp
      } else {
        num <- .w3_lgamma(cnt[j] + 1) + .w3_lgamma(cnt[j + 1L] + 1) + lpp
        den <- .w3_lgamma(left + 1) + .w3_lgamma(right + 1) + lp
      }
      logr <- num - den
      acc <- .ghc_unif(e, 1L)
      if (logr >= 0 || (acc > 0 && log(acc) < logr)) {
        q <- qq
        lp <- lpp
        cnt[j] <- left
        cnt[j + 1L] <- right
        taken <- taken + 1L
      }
    }
    if (it - 1L >= burn && (it - 1L - burn) %% thin == 0L)
      draws[[length(draws) + 1L]] <- q
  }
  if (!length(draws)) stop("no draws were kept; lower burn or thin")

  d <- length(draws)
  mo <- .bnppvl_moments(draws, k)
  e1 <- .w3_csum(mo$m1) / d
  e2 <- .w3_csum(mo$m2) / d
  v1 <- e2 - e1 * e1
  span <- hi - lo
  pred_mean <- lo + span * e1
  pred_sd <- if (v1 > 0) span * sqrt(v1) else 0

  qbar <- numeric(k + 1L)
  for (i in seq_len(k + 1L))
    qbar[i] <- lo + span *
      (.w3_csum(vapply(draws, function(dr) dr[i], numeric(1))) / d)

  if (is.null(grid))
    grid <- vapply(0:7, function(t) lo + span * (t + 0.5) / 8, numeric(1))
  grid <- as.numeric(grid)
  dens <- numeric(length(grid))
  cdf <- numeric(length(grid))
  for (g in seq_along(grid)) {
    gu <- (grid[g] - lo) / span
    dens[g] <- .w3_csum(vapply(draws, function(dr)
      .bnppvl_density_at(gu, dr, k), numeric(1))) / (d * span)
    cdf[g] <- .w3_csum(vapply(draws, function(dr)
      .bnppvl_cdf_at(gu, dr, k), numeric(1))) / d
  }

  FF <- function(t) .w3_csum(vapply(draws, function(dr)
    .bnppvl_cdf_at(t, dr, k), numeric(1))) / d

  probs <- as.numeric(probs)
  pq <- numeric(length(probs))
  for (i in seq_along(probs)) {
    p <- probs[i]
    if (!(p > 0 && p < 1))
      stop("every predictive probability must lie strictly inside (0, 1)")
    # A fixed number of bisection steps on the averaged distribution
    # function: the predictive CDF is a mixture of piecewise linear
    # pieces and has no closed-form inverse, and a fixed step count is
    # the only root find that cannot iterate a different number of times
    # in the two arms.
    pq[i] <- lo + span * .w3_bisect(function(t) FF(t) - p, 0, 1)
  }

  ll <- morie_bnppvl_loglik(u, (qbar - lo) / span, likelihood)
  list(quantile_mean = qbar, estimate = pred_mean, se = pred_sd / sqrt(d),
       predictive_mean = pred_mean, predictive_sd = pred_sd, grid = grid,
       density = dens, cdf = cdf, probs = probs, predictive_quantile = pq,
       counts = cnt, log_likelihood = ll, log_prior = lp,
       accept_rate = if (tried > 0L) taken / as.numeric(tried) else NaN,
       n_draws = d, n = n, k = k, m = m, c = as.numeric(c), lo = lo,
       hi = hi, schedule = schedule, centring = centring,
       likelihood = likelihood, init = init,
       method = "Beta quantile pyramid predictive")
}

#' One-line summary of the bnppvl module
#'
#' @return A character scalar.
#' @export
morie_bnppvl_cheatsheet <- function()
  paste0("bnppvl: quantile-pyramid predictive for a new observation. ",
         "likelihoods ", paste(.BNPPVL_LIKELIHOODS, collapse = ", "),
         "; inits ", paste(.BNPPVL_INITS, collapse = ", "),
         "; centrings ", paste(.BNPPVL_CENTRINGS, collapse = ", "),
         "; schedules ", paste(.BNPPVL_SCHEDULES, collapse = ", "))
