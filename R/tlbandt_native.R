# morie.fn -- function file (rootcoder007/morie)
# Targeting a simple statistical bandit problem.
#
# An infinite i.i.d. sequence (W_n, Y_n(0), Y_n(1)) is disclosed
# one step at a time. At step n the context W_n is revealed, we choose a
# randomised action A_n in {0,1} with probability g_n(1 | W_n) that we
# design from O_1,...,O_{n-1}, and we receive only Y_n(A_n) -- the
# other reward is never seen. Contexts may be high-dimensional; rewards
# lie in (0,1).
#
# Two goals that pull apart. A bandit algorithm usually maximises
# cumulative reward. The statistical goal here is inference: a
# confidence interval for the mean reward under the optimal rule. An
# algorithm that converges to always playing the better arm stops
# producing the data needed to estimate the other one -- so the design
# must keep randomising.
#
# Which is why the randomisation is bounded away from 0 and 1.
# Choosing g_n in [delta, 1-delta] costs some reward and buys the
# positivity that both identification and the variance estimate
# require. design_probability enforces it rather than letting a
# greedy rule silently destroy the estimator's basis.
#
# The data are not i.i.d., and the estimator accounts for it.
# g_n depends on the past, so O_1,...,O_n are dependent. The TMLE's
# influence terms are nevertheless a martingale difference sequence
# with respect to the history -- each term has conditional mean zero
# given the past because the action was randomised with a known,
# past-measurable probability. Variance is the sum of squares and the
# limit is normal by the martingale central limit theorem. That known
# randomisation is what makes an adaptive design analysable at all,
# and the anchor checks the martingale property rather than assuming it.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 24 (Chambaz,
# Zheng & van der Laan): an infinite i.i.d. sequence
# (W_n, Y_n(0), Y_n(1)) sequentially and partially disclosed; the
# context W_n revealed first, then a randomized action A_n carried out
# with probability g_n(.|W_n) determined by the observations accrued so
# far, and only the reward Y_n = Y_n(A_n) of the action taken granted --
# the alternative never observed; a possibly high-dimensional context
# set and rewards in the open unit interval.
#
# Chambaz, A., Zheng, W. & van der Laan, M. J. (2017) "Targeted
# sequential design for targeted learning inference of the optimal
# treatment rule and its mean reward", Annals of Statistics 45(6),
# 2537-2564, doi:10.1214/16-AOS1534.
#
# Lai, T. L. & Robbins, H. (1985) "Asymptotically efficient adaptive
# allocation rules", Advances in Applied Mathematics 6(1), 4-22,
# doi:10.1016/0196-8858(85)90002-8. The reward-maximising tradition
# this departs from.

#' .tlbandt_design_probability
#'
#' A step of the tlbandt_native implementation. Called by \code{morie_tlbandt}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param blip_estimate See Usage.
#' @param delta Defaults to \code{0.1}.
#' @param greedy A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
.tlbandt_design_probability <- function(blip_estimate, delta = 0.1,
                                        greedy = FALSE) {
  d <- as.numeric(delta)
  if (!(d > 0 && d < 0.5)) {
    stop(sprintf("tlbandt: delta must lie in (0, 0.5), got %s",
                 format(delta)))
  }
  b <- as.numeric(blip_estimate)
  if (isTRUE(greedy)) {
    return(if (b > 0) 1.0 else 0.0)
  }
  if (b > 0) 1.0 - d else d
}

#' morie_tlbandt
#'
#' A step of the tlbandt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param W A matrix; passed to \code{as.matrix}.
#' @param Y1 See Usage.
#' @param Y0 See Usage.
#' @param blip_fn See Usage.
#' @param delta Passed to \code{.tlbandt_design_probability}. Defaults to \code{0.1}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param greedy A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param burn_in Defaults to \code{20L}.
#' @return A list with \code{A}, \code{Y}, \code{g}, \code{history}, \code{greedy}, \code{min_g}, \code{max_g}, \code{note}.
#' @export
morie_tlbandt <- function(W, Y1, Y0, blip_fn, delta = 0.1, seed = 0,
                          greedy = FALSE, burn_in = 20L) {
  if (is.list(W) && !is.data.frame(W) && !is.matrix(W)) {
    rows <- lapply(W, as.numeric)
    n <- length(rows)
  } else {
    Wm <- as.matrix(W)
    n <- nrow(Wm)
    rows <- lapply(seq_len(n), function(i) as.numeric(Wm[i, ]))
  }
  y1 <- as.numeric(Y1)
  y0 <- as.numeric(Y0)
  if (length(y1) != n || length(y0) != n) {
    stop("tlbandt: the arms differ in length")
  }
  e <- .ghc_rng(seed)
  u <- .ghc_unif(e, n)
  burn <- as.integer(burn_in)
  hist <- vector("list", n)
  A <- numeric(n)
  Y <- numeric(n)
  G <- numeric(n)
  for (t in seq_len(n)) {
    if (t <= burn) {
      b <- 0.0
      g <- 0.5
    } else {
      prev <- if (t > 1L) hist[seq_len(t - 1L)] else list()
      b <- as.numeric(blip_fn(prev))
      g <- .tlbandt_design_probability(b, delta, greedy)
    }
    a <- if (u[t] < g) 1.0 else 0.0
    r <- if (a == 1.0) y1[t] else y0[t]
    A[t] <- a
    Y[t] <- r
    G[t] <- g
    hist[[t]] <- list(W = rows[[t]], A = a, Y = r, g = g)
  }
  list(A = A, Y = Y, g = G, history = hist,
       greedy = isTRUE(greedy),
       min_g = min(G), max_g = max(G),
       note = "only the reward of the action TAKEN is observed")
}

#' .tlbandt_martingale_terms
#'
#' A step of the tlbandt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A See Usage.
#' @param Y See Usage.
#' @param g See Usage.
#' @param Q1 See Usage.
#' @param Q0 See Usage.
#' @param psi See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.tlbandt_martingale_terms <- function(A, Y, g, Q1, Q0, psi) {
  a <- as.numeric(A)
  y <- as.numeric(Y)
  gg <- as.numeric(g)
  q1 <- as.numeric(Q1)
  q0 <- as.numeric(Q0)
  n <- length(a)
  if (any(gg <= 0 | gg >= 1)) {
    stop("tlbandt: the design probability left (0,1) -- a greedy rule destroys the positivity the inference rests on")
  }
  out <- numeric(n)
  for (i in seq_len(n)) {
    qa <- if (a[i] == 1.0) q1[i] else q0[i]
    h <- a[i] / gg[i] - (1.0 - a[i]) / (1.0 - gg[i])
    out[i] <- h * (y[i] - qa) + q1[i] - q0[i] - as.numeric(psi)
  }
  out
}

#' .tlbandt_sequential_ci
#'
#' A step of the tlbandt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param D See Usage.
#' @param level Defaults to \code{1.96}.
#' @return A list with \code{se}, \code{half_width}, \code{T}, \code{note}.
#' @export
.tlbandt_sequential_ci <- function(D, level = 1.96) {
  v <- as.numeric(D)
  Tlen <- length(v)
  if (Tlen < 2L) {
    stop("tlbandt: at least 2 steps are needed")
  }
  s2 <- sum(v * v) / Tlen
  se <- sqrt(s2 / Tlen)
  list(se = se, half_width = as.numeric(level) * se, T = Tlen,
       note = "sum of squares, not the i.i.d. variance -- the terms are dependent but uncorrelated")
}

#' .tlbandt_regret
#'
#' A step of the tlbandt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y See Usage.
#' @param Y1 See Usage.
#' @param Y0 See Usage.
#' @return A list with \code{cumulative_regret}, \code{mean_regret}, \code{note}.
#' @export
.tlbandt_regret <- function(Y, Y1, Y0) {
  y <- as.numeric(Y)
  a <- as.numeric(Y1)
  b <- as.numeric(Y0)
  n <- length(y)
  best <- pmax(a, b)
  diff <- best - y
  list(cumulative_regret = sum(diff),
       mean_regret = sum(diff) / n,
       note = "the price of keeping the design randomised")
}

#' .tlbandt_cheatsheet
#'
#' A step of the tlbandt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
.tlbandt_cheatsheet <- function() {
  "tlbandt: contexts arrive, we choose a RANDOMISED action with a probability we design from the past, and only the reward of the action taken is revealed. The goal is INFERENCE, not cumulative reward -- and those pull apart, because an algorithm that converges to one arm stops generating data about the other. So keep g in [delta, 1-delta]: it costs regret and buys positivity. The data are dependent, but the influence terms are a MARTINGALE difference sequence precisely because the randomisation probability is known and past-measurable."
}

.tlbandt_statisticalbandit <- morie_tlbandt
