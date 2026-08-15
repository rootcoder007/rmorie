# Page's likelihood-ratio CUSUM changepoint detector.
#
# Lai, T. L. (1995) "Sequential changepoint detection in quality control
# and dynamical systems", Journal of the Royal Statistical Society B
# 57(4), 613-658, section 2.4 and equation (2.3).
#
# The setting is a sequence X_1, X_2, ... that is i.i.d. with density f_0
# up to a changepoint nu, and i.i.d. with f_1 from nu onward. Page (1954)
# scores each observation by its log-likelihood ratio
#   Z_i = log(f_1(X_i) / f_0(X_i)),
# writes S_n = sum_{i<=n} Z_i with S_0 = 0, and stops at
#   N = inf{ n : max_{1<=k<=n} sum_{i=k}^{n} log(f_1(X_i)/f_0(X_i)) >= c }.
#
# That inner maximum is what this module returns at every n. It equals
# S_n - min_{0<=i<=n} S_i, which is why the detector needs only one pass
# and O(1) state: the running minimum of the partial sums is the best
# guess at where the change began.
#
# Moustakides (1986) and Ritov (1990) proved (2.3) exactly minimax for the
# worst-case expected delay; Lorden (1971) gave the asymptotics
#   E_1(N) ~ log(gamma) / I(f_1, f_0),
#   I(f_1, f_0) = E_{f_1}[ log(f_1(X_1)/f_0(X_1)) ],
# the Kullback-Leibler information number, reported here as kl.
#
# This is the simple-versus-simple case, where both densities are named.
# Lai's *generalized* likelihood ratio, equation (2.4), replaces f_1 by
# a supremum over an exponential family when the out-of-control
# distribution is not specified in advance; it reduces to (2.3) exactly
# when the family is the single point p1.
#
# Routes
# ------
# family selects the score, since (2.3) is stated for any pair of densities
# and the useful ones differ by field:
#   "bernoulli" : p0/p1 are success probabilities and x is 0/1.
#                 The default -- the argument names are probabilities.
#   "normal"    : p0/p1 are means of a Gaussian with known sd; the score
#                 collapses to (mu1 - mu0)(x - (mu0+mu1)/2) / sigma^2,
#                 the classical two-sided-chart score.
#   "poisson"   : p0/p1 are rates and x counts.

.glm_score_bernoulli <- function(p0, p1) {
  if (!is.numeric(p0) || length(p0) != 1L || p0 <= 0 || p0 >= 1)
    stop("glr_test: bernoulli p0 must lie strictly in (0, 1)")
  if (!is.numeric(p1) || length(p1) != 1L || p1 <= 0 || p1 >= 1)
    stop("glr_test: bernoulli p1 must lie strictly in (0, 1)")
  a <- log(p1 / p0)
  b <- log((1 - p1) / (1 - p0))
  z <- function(v) {
    if (length(v) != 1L || !is.numeric(v) || (v != 0 && v != 1))
      stop("glr_test: bernoulli data must be 0 or 1")
    if (v == 1) a else b
  }
  kl <- p1 * a + (1 - p1) * b
  list(z = z, kl = kl)
}

.glm_score_normal <- function(p0, p1, sd) {
  if (!is.numeric(sd) || length(sd) != 1L || sd <= 0)
    stop("glr_test: sd must be positive")
  d <- p1 - p0
  mid <- 0.5 * (p0 + p1)
  s2 <- sd * sd
  z <- function(v) d * (v - mid) / s2
  kl <- d * d / (2 * s2)
  list(z = z, kl = kl)
}

.glm_score_poisson <- function(p0, p1) {
  if (!is.numeric(p0) || length(p0) != 1L || p0 <= 0)
    stop("glr_test: poisson rate p0 must be positive")
  if (!is.numeric(p1) || length(p1) != 1L || p1 <= 0)
    stop("glr_test: poisson rate p1 must be positive")
  lr <- log(p1 / p0)
  z <- function(v) {
    if (length(v) != 1L || !is.numeric(v) || v < 0 || v != floor(v))
      stop("glr_test: poisson data must be non-negative integers")
    v * lr - (p1 - p0)
  }
  kl <- p1 * lr - (p1 - p0)
  list(z = z, kl = kl)
}

morie_glm <- function(x, p0, p1, threshold = NULL,
                      family = "bernoulli", sd = 1.0) {
  if (is.null(x) || length(x) == 0L)
    stop("glr_test: x must hold at least one observation")
  xv <- as.numeric(x)
  n <- length(xv)
  fam <- tolower(as.character(family))
  if (!(fam %in% c("bernoulli", "normal", "poisson")))
    stop("glr_test: family must be one of bernoulli, normal, poisson")
  p0 <- as.numeric(p0)
  p1 <- as.numeric(p1)
  if (length(p0) != 1L || length(p1) != 1L)
    stop("glr_test: p0 and p1 must be scalars")
  if (p0 == p1)
    stop("glr_test: p0 and p1 must differ -- with one density there is no change to detect")

  if (fam == "bernoulli") {
    sc <- .glm_score_bernoulli(p0, p1)
  } else if (fam == "normal") {
    sc <- .glm_score_normal(p0, p1, as.numeric(sd))
  } else {
    sc <- .glm_score_poisson(p0, p1)
  }
  z <- sc$z
  kl <- sc$kl

  scores <- numeric(n)
  cusum <- numeric(n)
  S <- 0
  smin <- 0
  smin_at <- 1L          # 1-based index of first post-change observation
  best <- -Inf
  best_k <- 1L          # 1-based
  stop_idx_0 <- -1L     # 0-based; -1 sentinel for no detection

  for (i in seq_len(n)) {
    zi <- z(xv[i])
    scores[i] <- zi
    S <- S + zi
    # The minimum in Lai (2.3) runs over S_0 .. S_n INCLUSIVE, so it must
    # absorb the new S before the statistic is read. That is what reflects
    # the chart at zero: with the update after the read, an all-in-control
    # stretch would drift negative instead of resting at 0.
    if (S < smin) {
      smin <- S
      smin_at <- i + 1L
    }
    val <- S - smin
    cusum[i] <- val
    if (val > best) {
      best <- val
      best_k <- smin_at
    }
    if (!is.null(threshold) && stop_idx_0 == -1L && val >= threshold)
      stop_idx_0 <- i - 1L
  }

  payload <- list(
    estimate = cusum,
    statistic = cusum[n],
    max_statistic = best,
    scores = scores,
    changepoint = as.integer(best_k - 1L),
    kl = kl,
    n = as.integer(n),
    family = fam,
    p0 = p0,
    p1 = p1,
    method = "Page likelihood-ratio CUSUM (Lai 1995, eq. 2.3)"
  )
  if (!is.null(threshold)) {
    payload$threshold <- as.numeric(threshold)
    payload$detected <- stop_idx_0 >= 0L
    payload$stop_index <- as.integer(stop_idx_0)
    # Lorden (1971): E_1(N) ~ log(gamma) / I(f_1, f_0), and the threshold
    # is c = log(gamma), so the ARL to detection is c / kl.
    payload$expected_delay <- if (kl > 0) as.numeric(threshold) / kl else Inf
  }
  payload
}

page_cusum <- function(x, p0, p1, threshold = NULL,
                       family = "bernoulli", sd = 1.0) {
  morie_glm(x = x, p0 = p0, p1 = p1, threshold = threshold,
            family = family, sd = sd)
}

glrtest <- function(x, p0, p1, threshold = NULL,
                    family = "bernoulli", sd = 1.0) {
  morie_glm(x = x, p0 = p0, p1 = p1, threshold = threshold,
            family = family, sd = sd)
}

.glm_cheatsheet <- function() {
  paste0("glm: Page likelihood-ratio CUSUM, max_k sum_{i=k}^{n} ",
         "log(f1/f0) (Lai 1995 eq. 2.3); families bernoulli/normal/",
         "poisson; KL number gives Lorden's delay log(gamma)/KL.")
}
