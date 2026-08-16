# Page's likelihood-ratio CUSUM changepoint detector.  Source: Lai,
# T. L. (1995), Sequential changepoint detection in quality control and
# dynamical systems, JRSS-B 57(4), 613-658, Sec. 2.4 and eq. (2.3):
#   N = inf{ n : S_n - min_{0<=i<=n} S_i >= c },  S_n = sum_{i<=n} Z_i,
#   Z_i = log( f1(X_i) / f0(X_i) ),
# with Lorden's (1971) delay E_1(N) ~ log(gamma) / I(f1, f0) where
# I(f1, f0) = E_f1[ log( f1(X_1) / f0(X_1) ) ].
# Native implementation mirroring Python morie.fn.glm, loop for loop.

#' .morie_glrtest_scores
#'
#' Part of the glrtest_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param p0 See Usage.
#' @param p1 See Usage.
#' @param family See Usage.
#' @param sd See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_glrtest_scores <- function(x, p0, p1, family, sd) {
  if (family == "bernoulli") {
    if (!(p0 > 0 && p0 < 1) || !(p1 > 0 && p1 < 1))
      stop("morie_glrtest: bernoulli p0 and p1 must lie strictly in (0, 1)")
    a <- log(p1 / p0)
    b <- log((1 - p1) / (1 - p0))
    if (any(!(x %in% c(0, 1))))
      stop("morie_glrtest: bernoulli data must be 0 or 1")
    list(z = ifelse(x == 1, a, b), kl = p1 * a + (1 - p1) * b)
  } else if (family == "normal") {
    if (sd <= 0) stop("morie_glrtest: sd must be positive")
    d <- p1 - p0
    mid <- 0.5 * (p0 + p1)
    s2 <- sd * sd
    list(z = d * (x - mid) / s2, kl = d * d / (2 * s2))
  } else {
    if (p0 <= 0 || p1 <= 0)
      stop("morie_glrtest: poisson rates must be positive")
    if (any(x < 0) || any(x != floor(x)))
      stop("morie_glrtest: poisson data must be non-negative integers")
    lr <- log(p1 / p0)
    list(z = x * lr - (p1 - p0), kl = p1 * lr - (p1 - p0))
  }
}

#' Page likelihood-ratio CUSUM (Lai 1995, eq. 2.3)
#'
#' Scores each observation by its log-likelihood ratio between a named
#' in-control and out-of-control density and returns the reflected
#' cumulative sum \eqn{S_n - \min_{0 \le i \le n} S_i}, the statistic of
#' Page's CUSUM scheme.  Mirrors Python \code{morie.fn.glm}.
#'
#' @param x Observed sequence, in time order.
#' @param p0,p1 In-control and out-of-control parameters, read according
#'   to \code{family}.
#' @param threshold Optional stopping boundary \eqn{c}; when supplied the
#'   first crossing is reported.
#' @param family One of \code{"bernoulli"} (default; \code{p0}/\code{p1}
#'   are success probabilities), \code{"normal"} (means, known
#'   \code{sd}) or \code{"poisson"} (rates).
#' @param sd Known standard deviation, \code{family = "normal"} only.
#' @return A list with the running \code{estimate}, terminal
#'   \code{statistic}, per-observation \code{scores}, the maximum
#'   likelihood \code{changepoint} and Lorden's information number
#'   \code{kl}.
#' @references Lai, T. L. (1995). Sequential changepoint detection in
#'   quality control and dynamical systems. Journal of the Royal
#'   Statistical Society B, 57(4), 613-658.
#' @export
morie_glrtest <- function(x, p0, p1, threshold = NULL, family = "bernoulli",
                      sd = 1) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 1) stop("morie_glrtest: x must hold at least one observation")
  fam <- tolower(as.character(family)[1])
  if (!fam %in% c("bernoulli", "normal", "poisson"))
    stop("morie_glrtest: family must be bernoulli, normal or poisson")
  p0 <- as.numeric(p0)[1]
  p1 <- as.numeric(p1)[1]
  if (p0 == p1)
    stop("morie_glrtest: p0 and p1 must differ -- with one density there is ",
         "no change to detect")

  sc <- .morie_glrtest_scores(x, p0, p1, fam, as.numeric(sd)[1])
  z <- sc$z

  cusum <- numeric(n)
  S <- 0
  smin <- 0
  smin_at <- 0L
  best <- -Inf
  best_k <- 0L
  stop_index <- NULL
  for (i in seq_len(n)) {
    S <- S + z[i]
    # min over S_0 .. S_n inclusive: absorb S before reading, which is
    # what reflects the chart at zero.
    if (S < smin) {
      smin <- S
      smin_at <- i
    }
    val <- S - smin
    cusum[i] <- val
    if (val > best) {
      best <- val
      best_k <- smin_at
    }
    if (!is.null(threshold) && is.null(stop_index) &&
        val >= as.numeric(threshold)[1])
      stop_index <- i - 1L
  }

  out <- list(estimate = cusum,
              statistic = cusum[n],
              max_statistic = best,
              scores = z,
              changepoint = as.integer(best_k),
              kl = sc$kl,
              n = as.integer(n),
              family = fam,
              p0 = p0,
              p1 = p1,
              method = "Page likelihood-ratio CUSUM (Lai 1995, eq. 2.3)")
  if (!is.null(threshold)) {
    out$threshold <- as.numeric(threshold)[1]
    out$detected <- !is.null(stop_index)
    out$stop_index <- if (is.null(stop_index)) -1L else as.integer(stop_index)
    out$expected_delay <- if (sc$kl > 0) out$threshold / sc$kl else Inf
  }
  out
}
