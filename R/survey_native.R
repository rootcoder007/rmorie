# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Design-based survey estimation. Mirrors morie.fn._survey and the
# htest1/hjkest/ratest/regest/genrgr/calibr/straprp/cluvar/ebayes/
# smplts modules.
#
# Collision scan: two near-misses, both resolved rather than
# overwritten. R/investigation.R already defines .morie_hajek_ate, a
# causal-inference internal, so the mean estimator here is named
# morie_hajek_mean. R/sampling.R already defines
# morie_calibration_weights, which is RAKING (iterative proportional
# fitting) on categorical margins -- a different distance function
# and a different interface, and precisely the bounded alternative
# the chi-square version below points to. It is cross-referenced,
# not replaced.
#
# The organising fact: in a design-based analysis the population
# values are FIXED and the randomness is sample inclusion, so
# variances come from the inclusion probabilities, not from a model
# for y.

#' Horvitz-Thompson estimator of a population total
#'
#' \eqn{\hat T = \sum_{i \in s} y_i/\pi_i}. Design-unbiased for ANY
#' design with strictly positive inclusion probabilities -- no model,
#' no distributional assumption. Its weakness is the same coin: it
#' never uses a known population size, so when the weights happen to
#' sum to much more or less than N, the estimate inherits that error
#' directly. Mirrors \code{morie.fn.htest1}.
#'
#' @param y observed values for the sampled units.
#' @param pi inclusion probabilities in (0, 1].
#' @param N known population size, for comparison.
#' @return list: total, mean, weight_sum, implied_N, N,
#'   design_unbiased, uses_known_N, n, method.
#' @examples
#' morie_horvitz_thompson(c(3, 5, 9), c(0.1, 0.2, 0.3))$total
#' @export
morie_horvitz_thompson <- function(y, pi, N = NULL) {
  yv <- as.numeric(y)
  p <- as.numeric(pi)
  if (length(yv) < 1L) stop("need at least one sampled unit.", call. = FALSE)
  if (length(p) != length(yv)) {
    stop(sprintf("pi has %d entries for %d observations.",
                 length(p), length(yv)), call. = FALSE)
  }
  if (any(p <= 0) || any(p > 1)) {
    stop("inclusion probabilities must lie in (0, 1].", call. = FALSE)
  }
  total <- sum(yv / p)
  wsum <- sum(1 / p)
  list(total = total, mean = total / wsum, weight_sum = wsum,
       implied_N = wsum, N = if (is.null(N)) NULL else as.integer(N),
       design_unbiased = TRUE, uses_known_N = FALSE, n = length(yv),
       method = "Horvitz-Thompson total; design-unbiased for any positive-pi design")
}

#' Hajek ratio estimator of a population mean
#'
#' \eqn{\sum(y_i/\pi_i)/\sum(1/\pi_i)}: the Horvitz-Thompson total
#' divided by the ESTIMATED population size. Biased in finite samples
#' -- it is a ratio of two random quantities -- but the bias is
#' \eqn{O(1/n)} against a first-order variance reduction, because
#' numerator and denominator move together. That cancellation is the
#' whole argument, and it fails when y is uncorrelated with the
#' weights. Mirrors \code{morie.fn.hjkest}.
#'
#' @param y observed values.
#' @param pi inclusion probabilities in (0, 1].
#' @return list: mean, ht_mean_if_N_known, weight_sum,
#'   design_unbiased, bias_order, n, method.
#' @examples
#' morie_hajek_mean(c(3, 5, 9), c(0.1, 0.2, 0.3))$mean
#' @export
morie_hajek_mean <- function(y, pi) {
  yv <- as.numeric(y)
  p <- as.numeric(pi)
  if (length(yv) < 2L) {
    stop(sprintf("need at least 2 sampled units, got %d.", length(yv)),
         call. = FALSE)
  }
  if (length(p) != length(yv)) {
    stop(sprintf("pi has %d entries for %d observations.",
                 length(p), length(yv)), call. = FALSE)
  }
  if (any(p <= 0) || any(p > 1)) {
    stop("inclusion probabilities must lie in (0, 1].", call. = FALSE)
  }
  w <- 1 / p
  list(mean = sum(w * yv) / sum(w),
       ht_mean_if_N_known = sum(yv / p) / sum(w), weight_sum = sum(w),
       design_unbiased = FALSE,
       bias_order = "O(1/n), against a first-order variance reduction",
       cancellation_note = paste("numerator and denominator move together;",
                                 "the gain vanishes when y is uncorrelated",
                                 "with the weights"),
       n = length(yv),
       method = "Hajek ratio estimator; biased but usually far less variable than HT")
}

#' Ratio estimator with an auxiliary variable
#'
#' \eqn{\hat R = \sum w y/\sum w x}, \eqn{\hat{\bar Y}_R = \hat R
#' \bar X}. Beats the simple mean exactly when the correlation
#' exceeds \eqn{CV(x)/(2 CV(y))} -- the threshold is computed and
#' compared against the data rather than left as folklore. Biased to
#' order \eqn{1/n}. Mirrors \code{morie.fn.ratest}.
#'
#' @param y,x study and auxiliary variables.
#' @param weights design weights; equal when NULL.
#' @param X_total,X_mean the known population total or mean of x; one
#'   is required, since that is what the estimator borrows.
#' @return list: ratio, mean, total, improves_on_simple_mean,
#'   efficiency_threshold, correlation, cv_x, cv_y, biased, n, method.
#' @examples
#' x <- rexp(50, 0.1)
#' morie_ratio_estimator(2 * x + rnorm(50), x, X_mean = 10)$improves_on_simple_mean
#' @export
morie_ratio_estimator <- function(y, x, weights = NULL, X_total = NULL,
                                  X_mean = NULL) {
  yv <- as.numeric(y); xv <- as.numeric(x)
  if (length(xv) != length(yv)) {
    stop(sprintf("x has %d entries for %d of y.", length(xv), length(yv)),
         call. = FALSE)
  }
  n <- length(yv)
  if (n < 3L) stop(sprintf("need at least 3 observations, got %d.", n),
                   call. = FALSE)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n) {
    stop(sprintf("weights has %d entries for %d observations.", length(w), n),
         call. = FALSE)
  }
  if (any(w < 0)) stop("weights must be non-negative.", call. = FALSE)
  if (is.null(X_total) && is.null(X_mean)) {
    stop(paste("the ratio estimator needs the population total or mean of x;",
               "that is what it borrows strength from."), call. = FALSE)
  }
  denom <- sum(w * xv)
  if (denom == 0) stop("the weighted total of x is zero.", call. = FALSE)
  R <- sum(w * yv) / denom
  xbar_pop <- if (!is.null(X_mean)) as.numeric(X_mean) else
    as.numeric(X_total) / sum(w)
  cvx <- if (mean(xv) != 0) stats::sd(xv) / mean(xv) else Inf
  cvy <- if (mean(yv) != 0) stats::sd(yv) / mean(yv) else Inf
  rho <- stats::cor(xv, yv)
  thr <- if (is.finite(cvy) && cvy != 0) cvx / (2 * cvy) else Inf
  list(ratio = R, mean = R * xbar_pop,
       total = R * (if (!is.null(X_total)) as.numeric(X_total) else
         xbar_pop * sum(w)),
       improves_on_simple_mean = rho > thr, efficiency_threshold = thr,
       correlation = rho, cv_x = cvx, cv_y = cvy, biased = TRUE, n = n,
       method = "Ratio estimator; beats the simple mean when rho > CV(x)/(2 CV(y))")
}

#' Regression (difference) estimator
#'
#' \eqn{\bar y + b(\bar X - \bar x)}. Unlike the ratio estimator it
#' fits an INTERCEPT rather than forcing the line through the origin,
#' and its variance is \eqn{(1 - \rho^2)} times the simple-mean
#' variance, so the gain is exactly the squared correlation. Mirrors
#' \code{morie.fn.regest}.
#'
#' @param y,x study and auxiliary variables.
#' @param weights design weights.
#' @param X_mean known population mean of x; required.
#' @return list: mean, slope, intercept, correlation,
#'   variance_ratio_to_simple_mean, passes_through_origin, n, method.
#' @examples
#' x <- runif(60, 5, 15)
#' morie_regression_estimator(100 + 2 * x + rnorm(60), x, X_mean = 10)$slope
#' @export
morie_regression_estimator <- function(y, x, weights = NULL, X_mean = NULL) {
  yv <- as.numeric(y); xv <- as.numeric(x)
  if (length(xv) != length(yv)) {
    stop(sprintf("x has %d entries for %d of y.", length(xv), length(yv)),
         call. = FALSE)
  }
  n <- length(yv)
  if (n < 3L) stop(sprintf("need at least 3 observations, got %d.", n),
                   call. = FALSE)
  if (is.null(X_mean)) {
    stop("the regression estimator needs the known population mean of x.",
         call. = FALSE)
  }
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (any(w < 0)) stop("weights must be non-negative.", call. = FALSE)
  sw <- sum(w)
  xbar <- sum(w * xv) / sw
  ybar <- sum(w * yv) / sw
  sxx <- sum(w * (xv - xbar)^2)
  if (sxx == 0) stop("the auxiliary has no weighted variation.", call. = FALSE)
  b <- sum(w * (xv - xbar) * (yv - ybar)) / sxx
  rho <- stats::cor(xv, yv)
  list(mean = ybar + b * (as.numeric(X_mean) - xbar), slope = b,
       intercept = ybar - b * xbar, correlation = rho,
       variance_ratio_to_simple_mean = 1 - rho^2,
       passes_through_origin = abs(ybar - b * xbar) < 1e-8 * max(abs(ybar), 1),
       n = n,
       method = "Regression estimator; fits an intercept, variance (1 - rho^2) times simple")
}

#' Generalised regression (GREG) estimator
#'
#' \eqn{\hat T_{GREG} = \hat T_{HT} + (T_x - \hat T_x)'B}. GREG is
#' where design-based and model-based thinking meet: the coefficient
#' B comes from a working model, but the estimator stays
#' design-consistent whether or not that model is right. A wrong
#' model costs EFFICIENCY, never validity, which is why GREG is the
#' standard production estimator in national statistics. Mirrors
#' \code{morie.fn.genrgr}.
#'
#' @param y study variable.
#' @param x matrix of auxiliaries.
#' @param weights design weights.
#' @param totals known population totals of the auxiliaries.
#' @return list: total, ht_total, correction, B, residual_totals,
#'   design_consistent_regardless_of_model, n, p, method.
#' @examples
#' x <- cbind(1, runif(40, 0, 10))
#' morie_greg(rnorm(40), x, rep(20, 40),
#'            c(800, 4000))$design_consistent_regardless_of_model
#' @export
morie_greg <- function(y, x, weights, totals) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  n <- nrow(X); p <- ncol(X)
  w <- as.numeric(weights)
  if (length(w) != n) {
    stop(sprintf("weights has %d entries for %d observations.", length(w), n),
         call. = FALSE)
  }
  if (any(w < 0)) stop("weights must be non-negative.", call. = FALSE)
  T <- as.numeric(totals)
  if (length(T) != p) {
    stop(sprintf("totals has %d entries for %d auxiliaries.", length(T), p),
         call. = FALSE)
  }
  ht <- sum(w * yv)
  Tx_hat <- colSums(w * X)
  B <- solve(crossprod(X * w, X), crossprod(X * w, yv))
  corr <- as.numeric(crossprod(T - Tx_hat, B))
  list(total = ht + corr, ht_total = ht, correction = corr,
       B = as.numeric(B), residual_totals = T - Tx_hat,
       design_consistent_regardless_of_model = TRUE,
       model_role = paste("the working model sets B and therefore the",
                          "EFFICIENCY; it does not affect design consistency"),
       n = n, p = p,
       method = "GREG (Sarndal); design-consistent whether or not the working model holds")
}

#' Chi-square calibration to known margins
#'
#' Finds weights closest to the design weights, in chi-square
#' distance, that reproduce known population margins EXACTLY. The
#' chi-square solution is closed form and reproduces GREG: one
#' adjusts the estimate, the other the weights.
#'
#' Chi-square calibration can produce NEGATIVE weights, which are
#' awkward to defend; the count is returned rather than silently
#' clipped. The bounded alternative already exists in this package as
#' \code{\link{morie_calibration_weights}}, which rakes on
#' categorical margins by iterative proportional fitting and cannot
#' go negative. Mirrors \code{morie.fn.calibr}.
#'
#' @param y study variable.
#' @param X calibration variables.
#' @param weights design weights.
#' @param totals population totals to reproduce.
#' @return list: total, calibrated_weights, margins_reproduced,
#'   max_margin_error, n_negative, weight_ratio_range, distance,
#'   equals_greg, n, p, method.
#' @seealso \code{\link{morie_calibration_weights}} for raking.
#' @examples
#' X <- cbind(1, runif(50))
#' morie_calibration_chi2(rnorm(50), X, rep(10, 50),
#'                        c(500, 260))$margins_reproduced
#' @export
morie_calibration_chi2 <- function(y, X, weights, totals) {
  yv <- as.numeric(y)
  Xm <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1L)
  if (nrow(Xm) != length(yv)) Xm <- t(Xm)
  if (nrow(Xm) != length(yv)) {
    stop("X must have one row per entry of y.", call. = FALSE)
  }
  n <- nrow(Xm); p <- ncol(Xm)
  d <- as.numeric(weights)
  if (length(d) != n) {
    stop(sprintf("design weights has %d entries for %d observations.",
                 length(d), n), call. = FALSE)
  }
  if (any(d < 0)) stop("design weights must be non-negative.", call. = FALSE)
  T <- as.numeric(totals)
  if (length(T) != p) {
    stop(sprintf("totals has %d entries for %d columns.", length(T), p),
         call. = FALSE)
  }
  lam <- solve(crossprod(Xm * d, Xm), T - colSums(d * Xm))
  w <- as.numeric(d * (1 + Xm %*% lam))
  achieved <- colSums(w * Xm)
  ratio <- ifelse(d > 0, w / d, NA_real_)
  list(total = sum(w * yv), calibrated_weights = w,
       margins_reproduced = isTRUE(all.equal(achieved, T, tolerance = 1e-8)),
       max_margin_error = max(abs(achieved - T)),
       n_negative = sum(w < 0),
       weight_ratio_range = c(min(ratio, na.rm = TRUE),
                              max(ratio, na.rm = TRUE)),
       distance = "chi-square, which reproduces GREG exactly",
       equals_greg = TRUE,
       negative_weight_note = paste("chi-square calibration can produce",
                                    "negative weights; see",
                                    "morie_calibration_weights for raking"),
       n = n, p = p,
       method = "Calibration to known margins; adjusts the WEIGHTS where GREG adjusts the estimate")
}

#' Stratified estimator of a proportion
#'
#' \eqn{\hat p_{st} = \sum_h W_h \hat p_h} with variance
#' \eqn{\sum_h W_h^2 \hat p_h(1-\hat p_h)/(n_h - 1)}. The variance
#' has no between-stratum term at all: that variation is removed by
#' DESIGN rather than estimated, which is what stratification buys.
#' The weights must be POPULATION shares -- sample shares silently
#' reproduce the unstratified estimate. Mirrors
#' \code{morie.fn.straprp}.
#'
#' @param y binary 0/1 responses.
#' @param stratum stratum labels.
#' @param weights population share per stratum, in sorted label order.
#' @return list: proportion, variance, se, strata, p_h, n_h, W_h,
#'   weights_are_population_shares, n, method.
#' @examples
#' y <- c(rbinom(50, 1, 0.1), rbinom(50, 1, 0.9))
#' morie_stratified_proportion(y, rep(1:2, each = 50), c(0.5, 0.5))$se
#' @export
morie_stratified_proportion <- function(y, stratum, weights = NULL) {
  yv <- as.numeric(y)
  st <- stratum
  if (length(st) != length(yv)) {
    stop(sprintf("stratum has %d entries for %d of y.",
                 length(st), length(yv)), call. = FALSE)
  }
  if (!all(yv %in% c(0, 1))) {
    stop("y must be binary 0/1 for a proportion.", call. = FALSE)
  }
  labs <- sort(unique(st))
  if (length(labs) < 2L) stop("need at least 2 strata.", call. = FALSE)
  ph <- numeric(length(labs)); nh <- numeric(length(labs))
  for (i in seq_along(labs)) {
    sel <- st == labs[i]
    m <- sum(sel)
    if (m < 2L) {
      stop(sprintf("stratum %s has %d units; need at least 2.",
                   as.character(labs[i]), m), call. = FALSE)
    }
    nh[i] <- m
    ph[i] <- mean(yv[sel])
  }
  W <- if (is.null(weights)) nh / sum(nh) else as.numeric(weights)
  pop <- !is.null(weights)
  if (length(W) != length(labs)) {
    stop(sprintf("weights has %d entries for %d strata.",
                 length(W), length(labs)), call. = FALSE)
  }
  if (!isTRUE(all.equal(sum(W), 1))) {
    stop(sprintf("stratum weights must sum to 1, got %g.", sum(W)),
         call. = FALSE)
  }
  var <- sum(W^2 * ph * (1 - ph) / (nh - 1))
  list(proportion = sum(W * ph), variance = var, se = sqrt(max(var, 0)),
       strata = labs, p_h = ph, n_h = as.integer(nh), W_h = W,
       weights_are_population_shares = pop,
       variance_note = paste("within-stratum only: between-stratum variation",
                             "is removed by DESIGN, not estimated"),
       n = length(yv),
       method = "Stratified proportion; W_h must be POPULATION shares or the design is discarded")
}

#' Variance of a mean under cluster sampling
#'
#' \eqn{(1 - n/N)S_b^2/n} with n the number of CLUSTERS, not
#' elements. 1000 units in 25 villages is 25 degrees of freedom, and
#' treating it as 1000 independent draws understates the standard
#' error by the square root of the design effect
#' \eqn{1 + (\bar m - 1)\rho}. Both are computed, so the cost of
#' clustering is a number. Mirrors \code{morie.fn.cluvar}.
#'
#' @param y element-level values.
#' @param cluster cluster identifiers.
#' @param N number of clusters in the population, for the fpc.
#' @return list: mean, variance, se, n_clusters, n_elements,
#'   mean_cluster_size, icc, deff, effective_n, naive_se,
#'   se_inflation, method.
#' @examples
#' y <- rnorm(200) + rep(rnorm(20), each = 10)
#' morie_cluster_variance(y, rep(1:20, each = 10))$deff
#' @export
morie_cluster_variance <- function(y, cluster, N = NULL) {
  yv <- as.numeric(y)
  cl <- cluster
  if (length(cl) != length(yv)) {
    stop(sprintf("cluster has %d entries for %d of y.",
                 length(cl), length(yv)), call. = FALSE)
  }
  labs <- unique(cl)
  n <- length(labs)
  if (n < 2L) stop(sprintf("need at least 2 clusters, got %d.", n),
                   call. = FALSE)
  means <- vapply(labs, function(l) mean(yv[cl == l]), numeric(1))
  sizes <- vapply(labs, function(l) sum(cl == l), numeric(1))
  Sb2 <- stats::var(means)
  fpc <- if (is.null(N)) 1 else max(0, 1 - n / as.numeric(N))
  var <- fpc * Sb2 / n
  mbar <- mean(sizes)
  grand <- mean(yv)
  ssb <- sum(sizes * (means - grand)^2)
  ssw <- sum(vapply(seq_along(labs), function(i)
    sum((yv[cl == labs[i]] - means[i])^2), numeric(1)))
  msb <- ssb / (n - 1)
  msw <- ssw / max(length(yv) - n, 1)
  den <- msb + (mbar - 1) * msw
  icc <- if (den != 0) (msb - msw) / den else 0
  deff <- 1 + (mbar - 1) * icc
  naive <- stats::sd(yv) / sqrt(length(yv))
  list(mean = mean(means), variance = var, se = sqrt(max(var, 0)),
       n_clusters = n, n_elements = length(yv), mean_cluster_size = mbar,
       icc = icc, deff = deff,
       effective_n = if (deff > 0) length(yv) / deff else length(yv),
       naive_se = naive,
       se_inflation = if (naive > 0) sqrt(max(var, 0)) / naive else NA_real_,
       note = "n is the number of CLUSTERS, not elements",
       method = "Cluster variance from between-cluster spread; deff = 1 + (mbar - 1) rho")
}

#' Actuarial (grouped) life table
#'
#' \eqn{\hat q_j = d_j/(n_j - w_j/2)} and
#' \eqn{\hat S_j = \prod_{k \le j}(1 - \hat q_k)}. The \eqn{w_j/2} is
#' the method's only real content: withdrawals are assumed uniform
#' within the interval, so each contributes half an interval of
#' exposure on average. Kaplan-Meier is the limit as intervals
#' shrink and should be preferred whenever exact times exist.
#' Mirrors \code{morie.fn.smplts}.
#'
#' @param intervals increasing interval boundaries, length J+1.
#' @param entered number entering each interval.
#' @param died deaths in each interval.
#' @param withdrawn withdrawals in each interval.
#' @return list: intervals, q, survival, effective_n,
#'   actuarial_correction, assumes, prefer, J, method.
#' @examples
#' morie_actuarial_lifetable(c(0, 1, 2), c(100, 70), c(20, 20),
#'                           c(10, 10))$q
#' @export
morie_actuarial_lifetable <- function(intervals, entered, died,
                                      withdrawn = NULL) {
  edges <- as.numeric(intervals)
  if (length(edges) < 2L) {
    stop("need at least 2 interval boundaries.", call. = FALSE)
  }
  if (any(diff(edges) <= 0)) {
    stop("interval boundaries must be strictly increasing.", call. = FALSE)
  }
  J <- length(edges) - 1L
  nj <- as.numeric(entered); dj <- as.numeric(died)
  wj <- if (is.null(withdrawn)) numeric(J) else as.numeric(withdrawn)
  for (nm in c("entered", "died", "withdrawn")) {
    arr <- switch(nm, entered = nj, died = dj, withdrawn = wj)
    if (length(arr) != J) {
      stop(sprintf("%s has %d entries for %d intervals.", nm, length(arr), J),
           call. = FALSE)
    }
    if (any(arr < 0)) stop(sprintf("%s must be non-negative.", nm),
                           call. = FALSE)
  }
  eff <- nj - wj / 2
  if (any(eff <= 0)) {
    stop("effective sample size is non-positive in some interval.",
         call. = FALSE)
  }
  if (any(dj > eff)) {
    stop("more deaths than effective exposure in some interval.",
         call. = FALSE)
  }
  q <- dj / eff
  list(intervals = edges, q = q, survival = cumprod(1 - q), effective_n = eff,
       actuarial_correction = "n_j - w_j/2",
       assumes = paste("withdrawals uniform within each interval, so each",
                       "contributes half an interval of exposure on average"),
       prefer = "Kaplan-Meier whenever exact times exist", J = J,
       method = "Actuarial life table for interval-grouped data")
}
