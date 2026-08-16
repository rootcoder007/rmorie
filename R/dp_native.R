# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Differential-privacy shelf. R mirrors of the morie.fn modules dpglap,
# dpgaus, dpexpm, dprrep, dpcnt, dpsum, dpmed, dpqua and dphis over the
# shared budget/noise helpers, as _dp.py does.
#
# Every mechanism here is randomised, so cross-language parity is
# asserted on the DETERMINISTIC parts -- the noise scale, the
# sensitivity, the selection probabilities, the debiasing arithmetic --
# plus the sampling distribution of the released quantity over many
# draws. Comparing individual releases across two generators would be
# comparing two different random numbers.

#' .morie_dp_check_budget
#'
#' Part of the dp_native implementation; see the file header for the
#' source it follows.
#'
#' @param epsilon See Usage.
#' @param delta Defaults to \code{NULL}.
#' @return A list with \code{epsilon}, \code{delta}.
#' @export
.morie_dp_check_budget <- function(epsilon, delta = NULL) {
  epsilon <- as.numeric(epsilon)[1L]
  if (!is.finite(epsilon) || epsilon <= 0) {
    stop("epsilon must be finite and positive", call. = FALSE)
  }
  if (is.null(delta)) {
    return(list(epsilon = epsilon, delta = NULL))
  }
  delta <- as.numeric(delta)[1L]
  if (delta < 0 || delta >= 1) stop("delta must be in [0, 1)", call. = FALSE)
  list(epsilon = epsilon, delta = delta)
}

# Laplace(0, b) by inverse transform; R has no rlaplace in base.
#' Laplace(0, b) by inverse transform; R has no rlaplace in base
#'
#' Part of the dp_native implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @param scale See Usage.
#' @return A numeric value.
#' @export
.morie_dp_rlaplace <- function(n, scale) {
  u <- stats::runif(n) - 0.5
  -scale * sign(u) * log1p(-2 * abs(u))
}

#' .morie_dp_gaussian_sigma
#'
#' Part of the dp_native implementation; see the file header for the
#' source it follows.
#'
#' @param sensitivity See Usage.
#' @param epsilon See Usage.
#' @param delta See Usage.
#' @return A numeric value.
#' @export
.morie_dp_gaussian_sigma <- function(sensitivity, epsilon, delta) {
  if (delta <= 0) {
    stop(paste(
      "the Gaussian mechanism needs delta > 0; use the Laplace",
      "mechanism for pure epsilon-DP"
    ), call. = FALSE)
  }
  as.numeric(sensitivity) * sqrt(2 * log(1.25 / delta)) / as.numeric(epsilon)
}

#' .morie_dp_clip
#'
#' Part of the dp_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param a See Usage.
#' @param b See Usage.
#' @return A list with \code{x}, \code{a}, \code{b}.
#' @export
.morie_dp_clip <- function(x, a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (a >= b) {
    stop(sprintf("need a < b, got a=%g, b=%g", a, b), call. = FALSE)
  }
  list(x = pmin(pmax(as.numeric(x), a), b), a = a, b = b)
}

#' .morie_dp_seed
#'
#' Part of the dp_native implementation; see the file header for the
#' source it follows.
#'
#' @param seed See Usage.
#' @return The value of \code{old}, as built in the body.
#' @export
.morie_dp_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  set.seed(as.integer(seed))
  old
}

#' .morie_dp_unseed
#'
#' Part of the dp_native implementation; see the file header for the
#' source it follows.
#'
#' @param old See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_dp_unseed <- function(old) {
  if (!is.null(old)) assign(".Random.seed", old, envir = globalenv())
}


#' Laplace mechanism for pure epsilon-differential privacy
#'
#' Adds Laplace(0, \eqn{\Delta_1/\epsilon}) noise, where \eqn{\Delta_1} is
#' the L1 sensitivity of the query.
#'
#' Sensitivity is a property of the QUERY, never of the data in hand.
#' Estimating it from the dataset is itself a non-private query, and a
#' sensitivity that is too small does not degrade the guarantee -- it
#' voids it.
#'
#' @param y the true query answer; scalar or vector.
#' @param sensitivity L1 sensitivity of the query, positive.
#' @param epsilon privacy budget, positive.
#' @param seed optional integer seed.
#' @return list with \code{release}, \code{noise_scale}, \code{noise_sd}
#'   (\eqn{\sqrt{2}b}), \code{epsilon}, \code{sensitivity}.
#' @references Dwork, C., McSherry, F., Nissim, K. and Smith, A. (2006).
#'   Calibrating noise to sensitivity in private data analysis.
#'   \emph{TCC}, 265-284.
#' @examples
#' morie_dp_laplace_mechanism(42,
#'   sensitivity = 1, epsilon = 0.5,
#'   seed = 1
#' )$noise_scale
#' @export
morie_dp_laplace_mechanism <- function(y, sensitivity = 1, epsilon = 1,
                                       seed = NULL) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  sensitivity <- as.numeric(sensitivity)[1L]
  if (sensitivity <= 0) stop("sensitivity must be positive", call. = FALSE)
  y <- as.numeric(y)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  b <- sensitivity / eps
  rel <- y + .morie_dp_rlaplace(length(y), b)
  list(
    release = rel, noise_scale = b, noise_sd = sqrt(2) * b, epsilon = eps,
    delta = 0, sensitivity = sensitivity, mechanism = "laplace",
    method = "dp_laplace_mechanism"
  )
}


#' Gaussian mechanism for (epsilon, delta)-differential privacy
#'
#' Adds N(0, sigma^2) noise with
#' \eqn{\sigma = \Delta_2\sqrt{2\log(1.25/\delta)}/\epsilon}.
#'
#' Calibrated to L2 sensitivity, not L1, which is why it beats Laplace
#' for high-dimensional queries. The classical bound above is only valid
#' for \eqn{\epsilon \le 1}; past that it is not a proof of anything, and
#' this function says so rather than returning a number that looks fine.
#'
#' @inheritParams morie_dp_laplace_mechanism
#' @param sensitivity L2 sensitivity.
#' @param delta failure probability, in (0, 1).
#' @return list with \code{release}, \code{sigma}, \code{epsilon},
#'   \code{delta}, \code{sensitivity}, and \code{warnings} when the
#'   classical bound does not apply.
#' @references Dwork, C. and Roth, A. (2014). The algorithmic foundations
#'   of differential privacy. \emph{FnTTCS}, 9(3-4), Appendix A.
#' @examples
#' round(morie_dp_gaussian_mechanism(10, 1, 0.5, 1e-5, seed = 1)$sigma, 4)
#' @export
morie_dp_gaussian_mechanism <- function(y, sensitivity = 1, epsilon = 1,
                                        delta = 1e-5, seed = NULL) {
  bud <- .morie_dp_check_budget(epsilon, delta)
  sensitivity <- as.numeric(sensitivity)[1L]
  if (sensitivity <= 0) stop("sensitivity must be positive", call. = FALSE)
  sigma <- .morie_dp_gaussian_sigma(sensitivity, bud$epsilon, bud$delta)
  y <- as.numeric(y)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  rel <- y + stats::rnorm(length(y), 0, sigma)
  warn <- character(0)
  if (bud$epsilon > 1) {
    warn <- sprintf(paste(
      "the classical Gaussian bound requires epsilon <= 1",
      "but epsilon=%g; sigma here is not a proof of",
      "(epsilon, delta)-DP -- use the analytic Gaussian",
      "mechanism or split the budget"
    ), bud$epsilon)
  }
  list(
    release = rel, sigma = sigma, noise_sd = sigma, epsilon = bud$epsilon,
    delta = bud$delta, sensitivity = sensitivity, mechanism = "gaussian",
    warnings = warn, method = "dp_gaussian_mechanism"
  )
}


#' Exponential mechanism for private selection
#'
#' Samples a candidate with probability proportional to
#' \eqn{\exp(\epsilon u / 2\Delta u)}.
#'
#' This is the mechanism for choices rather than numbers -- where adding
#' noise to the answer makes no sense because the answer is not a number.
#' The 2 in the denominator is load-bearing: it is what covers the change
#' in the normalising constant as well as in the utility, and dropping it
#' halves the claimed epsilon.
#'
#' @param candidates vector or list of candidates.
#' @param utility utility of each candidate, finite.
#' @param epsilon privacy budget.
#' @param sensitivity sensitivity of the utility function.
#' @param seed optional integer seed.
#' @return list with \code{selected}, \code{index} (0-based, matching the
#'   Python module), \code{probabilities}.
#' @references McSherry, F. and Talwar, K. (2007). Mechanism design via
#'   differential privacy. \emph{FOCS}, 94-103.
#' @examples
#' r <- morie_dp_exponential_mechanism(c("a", "b", "c"), c(0, 5, 1),
#'   epsilon = 2, seed = 1
#' )
#' round(r$probabilities, 3)
#' @export
morie_dp_exponential_mechanism <- function(candidates, utility, epsilon = 1,
                                           sensitivity = 1, seed = NULL) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  sensitivity <- as.numeric(sensitivity)[1L]
  if (sensitivity <= 0) stop("sensitivity must be positive", call. = FALSE)
  cands <- as.list(candidates)
  u <- as.numeric(utility)
  if (length(u) != length(cands)) {
    stop(sprintf(
      "utility has %d entries but there are %d candidates",
      length(u), length(cands)
    ), call. = FALSE)
  }
  if (!all(is.finite(u))) stop("utility must be finite", call. = FALSE)
  logp <- eps * u / (2 * sensitivity)
  logp <- logp - max(logp)
  p <- exp(logp)
  p <- p / sum(p)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  idx <- sample.int(length(cands), 1L, prob = p)
  list(
    selected = cands[[idx]], index = as.integer(idx - 1L),
    probabilities = p, epsilon = eps, sensitivity = sensitivity,
    mechanism = "exponential", method = "dp_exponential_mechanism"
  )
}


#' Warner randomized response
#'
#' Each respondent answers truthfully with probability
#' \eqn{p = e^\epsilon/(1+e^\epsilon)} and flips otherwise. This is LOCAL
#' differential privacy: there is no trusted curator, and the raw data
#' never leaves the respondent.
#'
#' The raw proportion of 1s is biased toward 1/2 by construction. Use
#' \code{estimate}, which inverts the flip; reporting the raw proportion
#' is the standard misreading and understates any real effect.
#'
#' @param truth binary vector of true responses (0/1).
#' @param epsilon privacy budget.
#' @param seed optional integer seed.
#' @return list with \code{responses}, \code{p_truth},
#'   \code{raw_proportion}, \code{estimate} (debiased), \code{se}.
#' @references Warner, S. L. (1965). Randomized response: a survey
#'   technique for eliminating evasive answer bias. \emph{JASA}, 60(309),
#'   63-69.
#' @examples
#' set.seed(1)
#' r <- morie_randomized_response_dp(rbinom(2000, 1, 0.3), epsilon = 2)
#' round(r$estimate, 2)
#' @export
morie_randomized_response_dp <- function(truth, epsilon = 1, seed = NULL) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  t <- as.numeric(truth)
  if (!all(t == 0 | t == 1)) {
    stop("truth must contain only 0 and 1", call. = FALSE)
  }
  p <- exp(eps) / (1 + exp(eps))
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  keep <- stats::runif(length(t)) < p
  resp <- ifelse(keep, t, 1 - t)
  raw <- mean(resp)
  est <- (raw - (1 - p)) / (2 * p - 1)
  n <- length(t)
  var <- raw * (1 - raw) / max(n, 1) / (2 * p - 1)^2
  list(
    responses = resp, p_truth = p, raw_proportion = raw, estimate = est,
    se = sqrt(max(var, 0)), n = as.integer(n), epsilon = eps,
    mechanism = "randomized_response", method = "randomized_response_dp"
  )
}


#' Differentially private count
#'
#' A count has sensitivity 1 no matter how large the dataset is -- one
#' person changes it by one -- so the noise is O(1) and the RELATIVE
#' error falls like 1/n. Counting is the query differential privacy is
#' kindest to.
#'
#' @param D data: a logical vector, a 0/1 vector, or anything with a
#'   length (in which case the count is the number of rows).
#' @param epsilon privacy budget.
#' @param predicate optional function applied to each element; the count
#'   is the number of TRUE results.
#' @param seed optional integer seed.
#' @param nonneg clamp a negative release to zero. The clamp is a
#'   post-processing step and costs no privacy, but it does bias the
#'   release upward near zero, which is recorded in \code{clamped}.
#' @return list with \code{release}, \code{raw} (pre-clamp),
#'   \code{true_count}, \code{noise_scale}, \code{clamped}.
#' @references Dwork, C. and Roth, A. (2014). \emph{FnTTCS}, 9(3-4),
#'   Sec. 3.3.
#' @examples
#' morie_dp_count(rep(TRUE, 1000), epsilon = 1, seed = 1)$noise_scale
#' @export
morie_dp_count <- function(D, epsilon = 1, predicate = NULL, seed = NULL,
                           nonneg = TRUE) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  true_count <- if (!is.null(predicate)) {
    sum(vapply(D, function(r) isTRUE(as.logical(predicate(r))), logical(1)))
  } else if (is.logical(D)) {
    sum(D)
  } else if (is.numeric(D) && is.null(dim(D)) && all(D %in% c(0, 1))) {
    sum(D)
  } else {
    length(D)
  }
  true_count <- as.numeric(true_count)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  b <- 1 / eps
  raw <- true_count + .morie_dp_rlaplace(1L, b)
  rel <- if (nonneg) max(raw, 0) else raw
  list(
    release = rel, raw = raw, true_count = true_count, noise_scale = b,
    sensitivity = 1, epsilon = eps, clamped = nonneg && raw < 0,
    method = "dp_count"
  )
}


#' Differentially private bounded sum
#'
#' Values are clipped to \code{[a, b]} and the sum released with Laplace
#' noise of scale \eqn{(b-a)/\epsilon}.
#'
#' The sensitivity IS the width of the clipping range, so the bounds must
#' come from outside the data. Reading them off the sample -- taking a
#' min and a max -- is a non-private query, and using them here voids the
#' guarantee entirely rather than weakening it.
#'
#' Heavy clipping biases the release toward the interior; the fraction
#' clipped is reported and warned about past 10%.
#'
#' @param x values to sum.
#' @param a,b clipping bounds, from outside the data.
#' @param epsilon privacy budget.
#' @param seed optional integer seed.
#' @return list with \code{release}, \code{true_sum} (of the clipped
#'   values), \code{noise_scale}, \code{sensitivity},
#'   \code{clipped_fraction}.
#' @references Dwork, C. and Roth, A. (2014). \emph{FnTTCS}, 9(3-4).
#' @examples
#' morie_dp_sum(rnorm(100), -3, 3, epsilon = 1, seed = 1)$sensitivity
#' @export
morie_dp_sum <- function(x, a, b, epsilon = 1, seed = NULL) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  raw_x <- as.numeric(x)
  cl <- .morie_dp_clip(raw_x, a, b)
  clipped_frac <- if (length(raw_x)) mean(raw_x != cl$x) else 0
  sens <- cl$b - cl$a
  scale <- sens / eps
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  rel <- sum(cl$x) + .morie_dp_rlaplace(1L, scale)
  list(
    release = rel, true_sum = sum(cl$x), noise_scale = scale,
    sensitivity = sens, clipped_fraction = clipped_frac,
    bounds = c(cl$a, cl$b), n = length(cl$x), epsilon = eps,
    warnings = if (clipped_frac > 0.1) {
      paste(
        "more than 10% of values were clipped; the bounds are biting",
        "and the release is biased toward the interior"
      )
    } else {
      character(0)
    },
    method = "dp_sum"
  )
}


#' Differentially private quantile
#'
#' The exponential mechanism over rank, with utility
#' \eqn{-|rank - qn|} and each interval weighted by its width.
#'
#' Adding noise to a quantile does not work: the global sensitivity of a
#' median is the whole domain, because one changed record can move it
#' from one end to the other. Selecting an INTERVAL by rank has
#' sensitivity 1 however extreme the values are, which is what makes the
#' release usable.
#'
#' @param x values.
#' @param q quantile in (0, 1).
#' @param epsilon privacy budget.
#' @param a,b bounds. Taken from the data when NULL, which is itself a
#'   non-private query -- the function warns.
#' @param seed optional integer seed.
#' @return list with \code{release}, \code{true_quantile},
#'   \code{interval}, \code{probabilities}, \code{bounds}.
#' @references Smith, A. (2011). Privacy-preserving statistical
#'   estimation with optimal convergence rates. \emph{STOC}, 813-822.
#' @examples
#' set.seed(1)
#' r <- morie_dp_quantile(rnorm(500), q = 0.5, epsilon = 2, a = -4, b = 4)
#' round(r$release, 2)
#' @export
morie_dp_quantile <- function(x, q = 0.5, epsilon = 1, a = NULL, b = NULL,
                              seed = NULL) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  if (q <= 0 || q >= 1) stop("q must be in (0, 1)", call. = FALSE)
  v <- as.numeric(x)
  if (length(v) == 0L) stop("x must be non-empty", call. = FALSE)
  warn <- character(0)
  if (is.null(a) || is.null(b)) {
    a <- min(v)
    b <- max(v)
    if (a == b) {
      a <- a - 0.5
      b <- b + 0.5
    }
    warn <- paste(
      "bounds were taken from the data, which is itself a",
      "non-private query; supply `a` and `b` from outside the",
      "data for a real release"
    )
  }
  cl <- .morie_dp_clip(v, a, b)
  s <- sort(cl$x)
  edges <- c(cl$a, s, cl$b)
  n <- length(s)
  gaps <- diff(edges)
  ranks <- seq_len(n + 1L) - 1L
  util <- -abs(ranks - q * n)
  logp <- eps * util / 2 + log(pmax(gaps, 1e-300))
  logp <- logp - max(logp)
  p <- exp(logp)
  p <- p / sum(p)
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  i <- sample.int(n + 1L, 1L, prob = p)
  rel <- stats::runif(1L, edges[i], edges[i + 1L])
  list(
    release = rel, true_quantile = stats::quantile(cl$x, q,
      names = FALSE,
      type = 7L
    ),
    interval = c(edges[i], edges[i + 1L]), probabilities = p,
    bounds = c(cl$a, cl$b), q = q, n = as.integer(n), epsilon = eps,
    warnings = warn, method = "dp_quantile"
  )
}


#' Differentially private median
#'
#' \code{\link{morie_dp_quantile}} at q = 0.5. Kept separate because the
#' median is the case where the naive approach fails most clearly: its
#' global sensitivity is the entire domain, so noise addition is not an
#' option and rank selection is the only route.
#'
#' @inheritParams morie_dp_quantile
#' @return list with \code{release}, \code{true_median}, \code{interval},
#'   \code{bounds}.
#' @references Smith, A. (2011). \emph{STOC}, 813-822.
#' @examples
#' set.seed(1)
#' round(morie_dp_median(rnorm(500), epsilon = 2, a = -4, b = 4)$release, 2)
#' @export
morie_dp_median <- function(x, epsilon = 1, a = NULL, b = NULL, seed = NULL) {
  r <- morie_dp_quantile(x,
    q = 0.5, epsilon = epsilon, a = a, b = b,
    seed = seed
  )
  list(
    release = r$release, true_median = r$true_quantile,
    interval = r$interval, bounds = r$bounds, n = r$n, epsilon = r$epsilon,
    warnings = r$warnings, method = "dp_median"
  )
}


#' Differentially private histogram
#'
#' Laplace noise on each bin count. Because the bins are disjoint, one
#' record touches exactly one of them: this is PARALLEL composition, so
#' the same epsilon covers every bin at once. Dividing the budget across
#' bins is the common error and wastes it entirely.
#'
#' The sensitivity used is 2, which covers the add/remove-one neighbouring
#' relation where a record can move between bins.
#'
#' @param x values to bin.
#' @param bins bin count or explicit edges.
#' @param epsilon privacy budget.
#' @param range_ optional \code{c(min, max)} for the binning range.
#' @param seed optional integer seed.
#' @param nonneg clamp negative counts to zero (post-processing, free).
#' @return list with \code{release}, \code{raw}, \code{true_counts},
#'   \code{edges}, \code{noise_scale}.
#' @references McSherry, F. (2009). Privacy integrated queries.
#'   \emph{SIGMOD}, 19-30. (parallel composition)
#' @examples
#' set.seed(1)
#' morie_dp_histogram(rnorm(500), bins = 5, epsilon = 1)$noise_scale
#' @export
morie_dp_histogram <- function(x, bins = 10, epsilon = 1, range_ = NULL,
                               seed = NULL, nonneg = TRUE) {
  eps <- .morie_dp_check_budget(epsilon)$epsilon
  v <- as.numeric(x)
  if (length(bins) == 1L) {
    if (bins < 1) {
      stop("bins must be a positive integer or an array of edges",
        call. = FALSE
      )
    }
    rng_ <- if (is.null(range_)) range(v) else as.numeric(range_)
    edges <- seq(rng_[1L], rng_[2L], length.out = as.integer(bins) + 1L)
  } else {
    edges <- as.numeric(bins)
  }
  nb <- length(edges) - 1L
  keep <- v >= edges[1L] & v <= edges[nb + 1L]
  idx <- pmin(pmax(findInterval(v[keep], edges), 1L), nb)
  counts <- tabulate(idx, nbins = nb)
  sens <- 2
  scale <- sens / eps
  old <- .morie_dp_seed(seed)
  on.exit(.morie_dp_unseed(old), add = TRUE)
  raw <- as.numeric(counts) + .morie_dp_rlaplace(nb, scale)
  rel <- if (nonneg) pmax(raw, 0) else raw
  list(
    release = rel, raw = raw, true_counts = counts, edges = edges,
    noise_scale = scale, sensitivity = sens, epsilon = eps,
    method = "dp_histogram"
  )
}
