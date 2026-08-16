# Auxiliary-variable slice sampling inside a Gibbs sweep.
#
# Sources: Damien, P., Wakefield, J. & Walker, S. (1999) "Gibbs sampling
# for Bayesian non-conjugate and hierarchical models by using auxiliary
# variables", JRSS Series B 61(2), 331-344. The auxiliary-variable
# construction that turns a non-conjugate full conditional into a
# uniform draw on an interval, and the hybrid Gibbs sweep in which
# every coordinate gets a slice update.
#
# Neal, R. M. (2003) "Slice sampling", Annals of Statistics 31(3),
# 705-767. The stepping-out and shrinkage procedures reproduced here,
# the log-scale formulation, and the result that correctness does not
# depend on the width w.
#
# Native R mirror of morie.fn.baygsl: every uniform is pulled from the
# shared .ghc_rng so the same seed reproduces the same chain down to the
# last bit, and the shrinkage / stepping-out bookkeeping matches the
# Python arm iteration for iteration.

.baygsl_NEG_INF <- -1.0e300      # a finite stand-in for -inf in R
.baygsl_POS_INF <-  1.0e300      # ditto for +inf
.baygsl_SHRINK_LIMIT <- 10000L   # Python arm's hard cap on shrinkage steps
.baygsl_INTERVAL_TINY <- 1e-15   # Python arm's convergence tolerance

# Exp(1) on the log scale without underflow. Mirrors baygsl._expo: keep
# drawing uniforms until we get one strictly positive, then take -log(u).
# R's qexp would consume the rng from somewhere else, so the loop
# matches the Python arm draw-for-draw.
#' Exp(1) on the log scale without underflow. Mirrors baygsl._expo: keep
#'
#' drawing uniforms until we get one strictly positive, then take
#' -log(u). R\'s qexp would consume the rng from somewhere else, so the
#' loop matches the Python arm draw-for-draw.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @return A numeric value.
#' @export
.baygsl_expo <- function(e) {
  u <- .ghc_unif(e, 1L)
  while (u <= 0) u <- .ghc_unif(e, 1L)
  -log(u)
}

# One univariate slice transition. Returns list(x, n_eval, interval).
# `logf` is the log of an unnormalised density; rng is the .ghc_rng
# environment so the chain stays reproducible.
#' One univariate slice transition. Returns list(x, n_eval, interval)
#'
#' `logf` is the log of an unnormalised density; rng is the .ghc_rng
#' environment so the chain stays reproducible.
#'
#' @param logf The body requires: baygsl: the shrinkage loop did not terminate; is logf returning a constant or NaN?.
#' @param x0 Numeric; combined arithmetically in the body.
#' @param e Passed to \code{.baygsl_expo}.
#' @param w Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param max_steps Numeric; combined arithmetically in the body. Defaults to \code{50L}.
#' @param lower Defaults to \code{.baygsl_NEG_INF}.
#' @param upper Defaults to \code{.baygsl_POS_INF}.
#' @return Nothing; this branch always raises.
#' @export
.baygsl_slice_1d <- function(logf, x0, e, w = 1.0, max_steps = 50L,
                             lower = .baygsl_NEG_INF,
                             upper = .baygsl_POS_INF) {
  if (!(w > 0)) stop("baygsl: the slice width must be positive")
  fx <- as.numeric(logf(x0))
  if (is.nan(fx) || fx == -Inf)
    stop("baygsl: the chain is at a point of zero density, so no slice exists there")
  n_eval <- 1L
  logu <- fx - .baygsl_expo(e)             # u ~ U(0, f(x)) on the log scale
  # stepping out
  r <- .ghc_unif(e, 1L)
  L <- x0 - r * w
  R <- L + w
  j <- as.integer(max_steps * .ghc_unif(e, 1L))
  k <- as.integer(max_steps) - 1L - j
  while (j > 0L && L > lower) {
    n_eval <- n_eval + 1L
    if (as.numeric(logf(L)) <= logu) break
    L <- L - w
    j <- j - 1L
  }
  while (k > 0L && R < upper) {
    n_eval <- n_eval + 1L
    if (as.numeric(logf(R)) <= logu) break
    R <- R + w
    k <- k - 1L
  }
  if (L < lower) L <- lower
  if (R > upper) R <- upper
  # shrinkage
  for (tt in seq_len(.baygsl_SHRINK_LIMIT)) {
    x1 <- L + .ghc_unif(e, 1L) * (R - L)
    n_eval <- n_eval + 1L
    if (as.numeric(logf(x1)) > logu)
      return(list(x = x1, n_eval = n_eval, interval = c(L, R)))
    if (x1 < x0) L <- x1 else R <- x1
    if (R - L < .baygsl_INTERVAL_TINY)
      return(list(x = x0, n_eval = n_eval, interval = c(L, R)))
  }
  stop("baygsl: the shrinkage loop did not terminate; is logf returning a constant or NaN?")
}

#' One univariate slice transition
#'
#' Returns the draw, the number of log-density evaluations, and the
#' final slice interval. The chain stays in distribution regardless of
#' the choice of \code{w}; what \code{w} affects is only the cost.
#'
#' @param logf Function returning the log of an unnormalised density.
#' @param x0 Current state.
#' @param e Generator environment (a \code{.ghc_rng} handle).
#' @param w Slice width; must be positive.
#' @param max_steps Maximum number of stepping-out steps per side.
#' @param lower,upper Support bounds; the draw stays in
#'   \code{[lower, upper]} without any rejection.
#' @return List with \code{x}, \code{n_eval}, \code{interval}.
#' @references Neal, R. M. (2003). Slice sampling. Annals of Statistics
#'   31(3), 705-767.
#' @export
morie_slice_sample_1d <- function(logf, x0, e, w = 1.0, max_steps = 50L,
                                  lower = -Inf, upper = Inf) {
  .baygsl_slice_1d(logf, as.numeric(x0), e, w = as.numeric(w),
                   max_steps = as.integer(max_steps),
                   lower = if (is.null(lower)) .baygsl_NEG_INF else as.numeric(lower),
                   upper = if (is.null(upper)) .baygsl_POS_INF else as.numeric(upper))
}

#' Univariate slice-sampling chain
#'
#' Runs \code{burn + n * thin} slice transitions and records every
#' \code{thin}-th draw from \code{burn} onwards.
#'
#' @param logf Log unnormalised density.
#' @param x0 Starting value.
#' @param n Number of draws to keep.
#' @param w Slice width.
#' @param burn Iterations to discard before recording.
#' @param seed Seed for the shared generator.
#' @param lower,upper Support bounds.
#' @param thin Thinning factor: keep every \code{thin}-th draw.
#' @return List with \code{draws}, \code{n_eval}, \code{w},
#'   \code{evals_per_draw}.
#' @references Neal, R. M. (2003). Damien, P. et al. (1999).
#' @export
morie_slice_chain <- function(logf, x0, n = 2000L, w = 1.0, burn = 0L,
                              seed = 1L, lower = -Inf, upper = Inf,
                              thin = 1L) {
  if (as.integer(n) < 1L) stop("baygsl: need at least one draw")
  if (as.integer(thin) < 1L) stop("baygsl: thin must be at least 1")
  e <- .ghc_rng(as.integer(seed))
  x <- as.numeric(x0)
  out <- numeric(as.integer(n))
  evals <- 0L
  total <- as.integer(burn) + as.integer(n) * as.integer(thin)
  lo <- if (is.null(lower)) .baygsl_NEG_INF else as.numeric(lower)
  up <- if (is.null(upper)) .baygsl_POS_INF else as.numeric(upper)
  idx <- 0L
  for (i in seq_len(total) - 1L) {
    st <- .baygsl_slice_1d(logf, x, e, w = as.numeric(w), lower = lo, upper = up)
    x <- st$x
    evals <- evals + st$n_eval
    if (i >= as.integer(burn) && (i - as.integer(burn)) %% as.integer(thin) == 0L) {
      idx <- idx + 1L
      out[idx] <- x
    }
  }
  list(draws = out, n_eval = as.integer(evals), w = as.numeric(w),
       evals_per_draw = as.numeric(evals) / length(out))
}

#' Effective sample size from the initial-positive autocorrelations
#'
#' Sums autocorrelations from lag 1 while they stay above 0.05 and
#' returns \code{n / (1 + 2 sum rho)} -- the same initial-positive-
#' sequence estimator that \code{stats::acf} uses by default and that
#' the Python arm's \code{baygsl.effective_sample_size} reproduces
#' iteration for iteration.
#'
#' Named with the \code{_ipseq} suffix to avoid clashing with the
#' Kish-weights ESS that ships under the same root in this package.
#'
#' @param x Numeric vector of draws.
#' @return Estimated ESS.
#' @export
morie_ess_ipseq <- function(x) {
  v <- as.numeric(x)
  n <- length(v)
  if (n < 4L) stop("baygsl: too few draws for an ESS")
  m <- sum(v) / n
  d <- v - m
  c0 <- sum(d * d) / n
  if (c0 <= 0) return(as.numeric(n))
  s <- 0.0
  max_lag <- min(n - 2L, 1000L)
  for (lag in seq_len(max_lag)) {
    nn <- n - lag
    c <- 0.0
    for (i in seq_len(nn)) c <- c + d[i] * d[i + lag]
    c <- c / n
    r <- c / c0
    if (r < 0.05) break
    s <- s + r
  }
  n / (1.0 + 2.0 * s)
}

#' Gibbs sweep with a slice update per coordinate
#'
#' Each coordinate is updated in turn through one slice transition on
#' its full conditional. This is the Damien-Wakefield-Walker construction:
#' a non-conjugate conditional is replaced by an auxiliary uniform draw.
#'
#' @param log_conditionals List of functions; element \code{k} takes a
#'   candidate value and the current state vector and returns the log of
#'   the unnormalised full conditional of coordinate \code{k}.
#' @param x0 Starting state vector.
#' @param n Number of Gibbs sweeps to keep.
#' @param w Per-coordinate slice width: scalar, vector, or \code{NULL}
#'   for the default \code{1.0} per coordinate.
#' @param burn Sweeps to discard.
#' @param seed Seed.
#' @param bounds Per-coordinate \code{list(c(lo, hi))} or \code{NULL}.
#' @return RichResult-like list with \code{estimate}, \code{draws},
#'   \code{mean}, \code{n_draws}, \code{n_eval}, \code{ess},
#'   \code{method}.
#' @references Damien, P., Wakefield, J. & Walker, S. (1999).
#' @export
morie_gibbs_slice <- function(log_conditionals, x0, n = 2000L, w = NULL,
                              burn = 0L, seed = 1L, bounds = NULL) {
  p <- length(x0)
  if (length(log_conditionals) != p)
    stop(sprintf("baygsl: %d conditionals for %d coordinates",
                 length(log_conditionals), p))
  if (p == 0L) stop("baygsl: no coordinates to sample")
  if (is.null(w)) {
    ws <- rep(1.0, p)
  } else if (is.numeric(w) && length(w) == 1L) {
    ws <- rep(as.numeric(w), p)
  } else {
    ws <- as.numeric(w)
    if (length(ws) != p)
      stop(sprintf("baygsl: w has length %d but %d coordinates",
                   length(ws), p))
  }
  if (is.null(bounds)) {
    bd <- vector("list", p)
    for (k in seq_len(p)) bd[[k]] <- c(.baygsl_NEG_INF, .baygsl_POS_INF)
  } else {
    bd <- lapply(bounds, function(b) c(as.numeric(b[1]), as.numeric(b[2])))
    if (length(bd) != p)
      stop(sprintf("baygsl: bounds has length %d but %d coordinates",
                   length(bd), p))
  }
  e <- .ghc_rng(as.integer(seed))
  state <- as.numeric(x0)
  keep <- vector("list", as.integer(n))
  evals <- 0L
  total <- as.integer(burn) + as.integer(n)
  kidx <- 0L
  for (i in seq_len(total) - 1L) {
    for (k in seq_len(p)) {
      kk <- k                     # freeze k for the closure below
      lf <- function(v) {
        s <- state
        s[kk] <- v
        as.numeric(log_conditionals[[kk]](v, s))
      }
      st <- .baygsl_slice_1d(lf, state[k], e, w = ws[k],
                             lower = bd[[k]][1], upper = bd[[k]][2])
      state[k] <- st$x
      evals <- evals + st$n_eval
    }
    if (i >= as.integer(burn)) {
      kidx <- kidx + 1L
      keep[[kidx]] <- state
    }
  }
  # column-wise mean and ESS over the kept draws
  draw_mat <- matrix(0.0, nrow = length(keep), ncol = p)
  for (j in seq_along(keep)) draw_mat[j, ] <- keep[[j]]
  means <- colMeans(draw_mat)
  ess <- vapply(seq_len(p), function(k)
    morie_ess_ipseq(draw_mat[, k]), numeric(1))
  list(estimate = means, draws = keep, mean = means,
       n_draws = as.integer(length(keep)), n_eval = as.integer(evals),
       ess = ess,
       method = paste("Gibbs sweep with a slice update per coordinate ",
                      "(Damien, Wakefield & Walker 1999; Neal 2003)",
                      sep = ""))
}

#' Entry point: hybrid Gibbs sweep with a slice update per coordinate
#'
#' Currently every coordinate is slice-updated; this name is preserved
#' to mirror the Python arm's hybrid interface, so a future module that
#' mixes conjugate draws and slice draws can slot in here without
#' breaking callers.
#'
#' @inheritParams morie_gibbs_slice
#' @return Same payload as \code{morie_gibbs_slice}.
#' @export
morie_hybrid_gibbs_slice <- function(log_conditionals, x0, n = 2000L, ...) {
  morie_gibbs_slice(log_conditionals, x0, n, ...)
}
