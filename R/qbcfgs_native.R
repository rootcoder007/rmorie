# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of qbcfgs -- quantile-balanced score for forests. Mirrors
# src/morie/fn/qbcfgs.py operation for operation, on the honest forest
# in R/sdcfst_native.R and the shared numerics in
# R/aaa_helpers_w3num.R.
#
# Inverse-propensity weighting has one failure mode that dominates all
# others: a single observation with a propensity near zero or one gets a
# weight near infinity and quietly becomes the entire estimate. The
# usual patches -- trimming, truncation -- throw away exactly the
# observations the design was least able to match, which is honest about
# the symptom and silent about the cause.
#
# Balancing within QUANTILE STRATA is the alternative. Sort the fitted
# propensities, cut them into strata of equal size, and normalise the
# weights inside each stratum rather than across the whole sample. An
# extreme propensity is then extreme only relative to its own stratum,
# where every other unit is extreme too, so it can carry at most its
# stratum's share of the estimate. Nothing is discarded and no unit can
# run away with the answer.
#
# What comes out is a per-stratum effect, a size-weighted overall effect
# that is exactly the average of them, and -- the part that matters -- a
# BALANCE table. The claim a weighting scheme makes is that after
# weighting the arms look alike on the covariates, and the standardised
# mean difference before and after is how you check it rather than take
# it on faith. A weighting that did not improve balance would have no
# argument for itself, and this module reports both numbers so that
# argument can be lost.
#
# The propensity comes from an honest forest, so the fitted value at a
# point was not chosen using that point's own treatment. Without honesty
# the propensities are shrunk towards the observed treatment and the
# weights look better balanced than they are.
#
# References
#   Hsu, Y.-C., Huber, M., Lee, Y.-Y. and Pipoz, L. (2022) "Direct and
#     indirect effects of continuous treatments based on generalized
#     propensity score weighting." Journal of Applied Econometrics
#     37(2), 449-460.
#   Rosenbaum, P.R. and Rubin, D.B. (1983) "The central role of the
#     propensity score in observational studies for causal effects."
#     Biometrika 70(1), 41-55.
#   Rosenbaum, P.R. and Rubin, D.B. (1984) "Reducing bias in
#     observational studies using subclassification on the propensity
#     score." Journal of the American Statistical Association 79(387),
#     516-524.
#   Austin, P.C. (2009) "Balance diagnostics for comparing the
#     distribution of baseline covariates between treatment groups in
#     propensity-score matched samples." Statistics in Medicine 28(25),
#     3083-3107.
#   Wager, S. and Athey, S. (2018) "Estimation and inference of
#     heterogeneous treatment effects using random forests." JASA
#     113(523), 1228-1242.

.QBCFGS_WEIGHTS <- c("ate", "att")

#' Equal-size strata by the rank of the fitted propensity
#'
#' Ranks rather than values, so a propensity distribution piled up at
#' one end still gives strata of equal size -- which is the point of
#' stratifying by quantile rather than by a grid.
#'
#' @param e Fitted propensities.
#' @param n_strata Number of strata.
#' @return A zero-based stratum index per observation.
#' @export
morie_qbcfgs_strata <- function(e, n_strata) {
  n <- length(e)
  ord <- order(e, seq_len(n))
  s <- integer(n)
  for (pos in seq_len(n)) {
    k <- floor((pos - 1L) * n_strata / n)
    if (k >= n_strata) k <- n_strata - 1
    s[ord[pos]] <- as.integer(k)
  }
  s
}

#' Standardised mean difference of one covariate between arms
#'
#' The difference in means over the pooled standard deviation, so it is
#' unit-free and comparable across covariates. With weights it is the
#' weighted version, which is the number a weighting scheme is claiming
#' to shrink.
#'
#' @param x One covariate.
#' @param d Treatment indicators.
#' @param w Weights, or NULL for unweighted.
#' @return The standardised mean difference.
#' @export
morie_qbcfgs_smd <- function(x, d, w = NULL) {
  n <- length(x)
  if (is.null(w)) w <- rep(1, n)
  t <- which(d == 1L); c0 <- which(d == 0L)
  s1 <- .w3_csum(w[t]); s0 <- .w3_csum(w[c0])
  if (s1 <= 0 || s0 <= 0) return(NaN)
  m1 <- .w3_csum(w[t] * x[t]) / s1
  m0 <- .w3_csum(w[c0] * x[c0]) / s0
  v1 <- .w3_csum(w[t] * (x[t] - m1) * (x[t] - m1)) / s1
  v0 <- .w3_csum(w[c0] * (x[c0] - m0) * (x[c0] - m0)) / s0
  pool <- 0.5 * (v1 + v0)
  if (pool <= 0) return(if (m1 == m0) 0 else Inf)
  (m1 - m0) / sqrt(pool)
}

#' A stratum-balanced treatment effect and its balance table
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates.
#' @param quantile Which propensity quantile the focal stratum is read
#'   at.
#' @param n_strata Number of equal-size strata.
#' @param weight "ate" weights each unit towards the whole population,
#'   "att" towards the treated.
#' @param n_trees Trees in the propensity forest.
#' @param min_leaf Minimum leaf size.
#' @param max_depth Maximum depth.
#' @param seed The random stream.
#' @param clip The propensity is held inside the interval before it
#'   enters a denominator. Reported, not silent.
#' @return A list with the per-stratum effects and sizes, the overall
#'   effect, the balance before and after weighting, and the focal
#'   stratum.
#' @export
morie_qbcfgs <- function(y, D, X, quantile = 0.5, n_strata = 4L,
                         weight = "ate", n_trees = 8L, min_leaf = 3L,
                         max_depth = 3L, seed = 0, clip = 0.01) {
  if (!(weight %in% .QBCFGS_WEIGHTS))
    stop("weight must be one of ", paste(.QBCFGS_WEIGHTS, collapse = ", "))
  ys <- as.numeric(y)
  d <- as.integer(ifelse(as.numeric(D) != 0, 1L, 0L))
  xs <- as.matrix(X); storage.mode(xs) <- "double"
  n <- length(ys); p <- ncol(xs)
  if (length(d) != n || nrow(xs) != n)
    stop("y, D and X must agree in length")
  q <- as.integer(n_strata)
  if (q < 2L) stop("need at least two strata")
  if (n < 4L * q) stop("need at least four observations per stratum")
  quantile <- as.numeric(quantile)
  if (!(quantile > 0 && quantile < 1))
    stop("the quantile must lie strictly inside (0, 1)")
  clip <- as.numeric(clip)
  if (!(clip > 0 && clip < 0.5))
    stop("the clip must lie strictly inside (0, 0.5)")
  rows <- seq_len(n)

  e0 <- .ghc_rng(seed)
  fe <- morie_sdcfst_forest(xs, as.numeric(d), rows, n_trees, NULL,
                            min_leaf, max_depth, e0)
  raw <- vapply(seq_len(n), function(i)
    morie_sdcfst_predict(fe, xs[i, ]), numeric(1))
  n_clipped <- 0L
  e <- numeric(n)
  for (i in seq_len(n)) {
    if (raw[i] < clip) { e[i] <- clip; n_clipped <- n_clipped + 1L }
    else if (raw[i] > 1 - clip) { e[i] <- 1 - clip; n_clipped <- n_clipped + 1L }
    else e[i] <- raw[i]
  }

  s <- morie_qbcfgs_strata(e, q)
  # Balancing weights, normalised WITHIN the stratum. That single word
  # is the method: the same weights normalised globally let one extreme
  # propensity dominate everything.
  w <- numeric(n)
  for (k in 0:(q - 1L)) {
    mem <- which(s == k)
    if (!length(mem)) next
    raww <- vapply(mem, function(i) {
      if (weight == "ate") {
        if (d[i] == 1L) 1 / e[i] else 1 / (1 - e[i])
      } else {
        if (d[i] == 1L) 1 else e[i] / (1 - e[i])
      }
    }, numeric(1))
    tot <- .w3_csum(raww)
    if (tot <= 0) next
    w[mem] <- raww * (length(mem) / tot)
  }

  eff <- numeric(q); size <- integer(q)
  for (k in 0:(q - 1L)) {
    mem <- which(s == k)
    t <- mem[d[mem] == 1L]; c0 <- mem[d[mem] == 0L]
    size[k + 1L] <- length(mem)
    if (!length(t) || !length(c0)) { eff[k + 1L] <- NaN; next }
    w1 <- .w3_csum(w[t]); w0 <- .w3_csum(w[c0])
    if (w1 <= 0 || w0 <= 0) { eff[k + 1L] <- NaN; next }
    m1 <- .w3_csum(w[t] * ys[t]) / w1
    m0 <- .w3_csum(w[c0] * ys[c0]) / w0
    eff[k + 1L] <- m1 - m0
  }

  live <- which(!is.nan(eff))
  if (!length(live))
    stop("no stratum contains both arms; reduce the number of strata")
  tot <- .w3_csum(as.numeric(size[live]))
  overall <- .w3_csum(eff[live] * size[live]) / tot

  before <- vapply(seq_len(p), function(j)
    morie_qbcfgs_smd(xs[, j], d), numeric(1))
  after <- vapply(seq_len(p), function(j)
    morie_qbcfgs_smd(xs[, j], d, w), numeric(1))
  fb <- .w3_csum(abs(before)) / p
  fa <- .w3_csum(abs(after)) / p

  focal <- as.integer(floor(quantile * q))
  if (focal >= q) focal <- q - 1L
  list(stratum_effect = eff, stratum_size = size, stratum = s,
       propensity = e, weight_value = w, smd_before = before,
       smd_after = after, mean_abs_smd_before = fb,
       mean_abs_smd_after = fa, balance_improved = fa < fb,
       estimate = overall, se = NaN, focal_stratum = focal,
       focal_effect = eff[focal + 1L], n_clipped = n_clipped,
       n_live_strata = length(live), n = n, n_treated = sum(d),
       n_strata = q, quantile = quantile, clip = clip,
       weighting = weight, method = "quantile-balanced score for forests")
}

#' One-line summary of the qbcfgs module
#'
#' @return A character scalar.
#' @export
morie_qbcfgs_cheatsheet <- function()
  paste0("qbcfgs: quantile-balanced score for forests. weightings ",
         paste(.QBCFGS_WEIGHTS, collapse = ", "),
         "; balancing weights normalised within propensity strata")
