# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Causal matching, weighting and design-diagnostic shelf. R mirrors of
# the morie.fn modules causmm, causmtchcm, causovlap, covbal, causipsw0,
# causflnk, ebalw, causrosen and causdid3w.
#
# Every function here is deterministic, so the parity suite compares
# values rather than distributions. Ties in a distance matrix are broken
# by stable order in both languages, which is why the Python side pins
# kind = "stable" -- an unstable sort makes the chosen match depend on
# the partition rather than on the data.

#' Mahalanobis-distance matching
#'
#' Matches each treated unit to its nearest controls under
#' \eqn{\sqrt{(x_i-x_j)'S^{-1}(x_i-x_j)}}. The inverse covariance is
#' what makes the distance invariant to linear rescaling of the
#' covariates -- Euclidean matching silently weights by whatever units
#' the columns happen to be in.
#'
#' Past roughly eight covariates the distances concentrate: the nearest
#' control is barely nearer than an arbitrary one, and the matching
#' stops meaning anything. Propensity-score matching is the answer
#' there, and the function warns.
#'
#' @param X covariate matrix.
#' @param treat 0/1 treatment indicator.
#' @param k matches per treated unit.
#' @param replace allow a control to serve several treated units.
#' @param caliper optional maximum distance; unmatched units are
#'   reported rather than force-matched.
#' @return list with \code{matches} (0-based control indices, -1 where
#'   unmatched), \code{distances}, \code{n_unmatched}, \code{reuse_max},
#'   \code{mean_distance}.
#' @references Rubin, D. B. (1980). Bias reduction using
#'   Mahalanobis-metric matching. \emph{Biometrics}, 36(2), 293-298.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(200), ncol = 2)
#' tr <- rbinom(100, 1, 0.4)
#' morie_causal_mahalanobis_match(X, tr)$n_unmatched
#' @export
morie_causal_mahalanobis_match <- function(X, treat, k = 1, replace = TRUE,
                                           caliper = NULL) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  tr <- as.numeric(treat)
  if (nrow(X) != length(tr)) {
    stop(sprintf("X has %d rows but treat has %d", nrow(X), length(tr)),
         call. = FALSE)
  }
  if (!all(tr == 0 | tr == 1)) stop("treat must be 0/1", call. = FALSE)
  k <- as.integer(k)
  if (k < 1L) stop("k must be at least 1", call. = FALSE)
  ti <- which(tr == 1)
  ci <- which(tr == 0)
  if (length(ti) == 0L || length(ci) == 0L) {
    stop("both treatment groups must be non-empty", call. = FALSE)
  }
  if (!replace && length(ci) < k * length(ti)) {
    stop(sprintf(paste("matching without replacement needs %d controls but",
                       "only %d are available"), k * length(ti), length(ci)),
         call. = FALSE)
  }
  S <- stats::cov(X)
  Sinv <- tryCatch(solve(S), error = function(e) .morie_ginv(S))
  D <- matrix(0, length(ti), length(ci))
  for (a in seq_along(ti)) {
    d <- sweep(X[ci, , drop = FALSE], 2L, X[ti[a], ], "-")
    D[a, ] <- sqrt(pmax(rowSums((d %*% Sinv) * d), 0))
  }
  matches <- matrix(-1L, length(ti), k)
  dists <- matrix(NA_real_, length(ti), k)
  used <- integer(length(ci))
  for (a in seq_along(ti)) {
    ord <- order(D[a, ])          # stable, matching numpy kind = "stable"
    picked <- 0L
    for (j in ord) {
      if (!replace && used[j] > 0L) next
      if (!is.null(caliper) && D[a, j] > caliper) break
      picked <- picked + 1L
      matches[a, picked] <- as.integer(ci[j] - 1L)
      dists[a, picked] <- D[a, j]
      used[j] <- used[j] + 1L
      if (picked == k) break
    }
  }
  ok <- matches[, 1L] >= 0L
  warn <- character(0)
  if (ncol(X) > 8L) {
    warn <- c(warn, sprintf(paste("%d covariates: Mahalanobis distances",
                                  "concentrate in high dimension, so the",
                                  "nearest control is barely nearer than an",
                                  "arbitrary one; prefer propensity-score",
                                  "matching"), ncol(X)))
  }
  if (replace && length(used) && max(used) > max(3L, length(ti) %/% 10L)) {
    warn <- c(warn, sprintf(paste("one control is matched to %d treated units;",
                                  "the effective control sample is much",
                                  "smaller than it looks"), max(used)))
  }
  list(matches = matches, distances = dists,
       matched_treated = as.integer(ti[ok] - 1L),
       n_unmatched = as.integer(sum(!ok)),
       reuse_max = if (length(used)) max(used) else 0L,
       mean_distance = if (any(ok)) mean(dists[ok, , drop = FALSE],
                                         na.rm = TRUE) else NA_real_,
       treated_index = as.integer(ti - 1L),
       control_index = as.integer(ci - 1L),
       warnings = warn, method = "causal_mahalanobis_match")
}


#' Caliper matching on the propensity score
#'
#' Nearest-neighbour matching with a hard maximum distance, by default
#' on the LOGIT of the propensity score.
#'
#' The logit scale matters: the raw propensity score is compressed near
#' 0 and 1, so a fixed caliper there is far stricter in the middle of
#' the distribution than at the ends. On the logit the caliper means the
#' same thing everywhere.
#'
#' Treated units with no control inside the caliper are DROPPED, and
#' that changes the estimand: what remains is the ATT among matchable
#' units, not the ATT. The returned \code{estimand} field says which one
#' you actually have.
#'
#' @param ps propensity scores, strictly inside (0, 1).
#' @param treat 0/1 treatment indicator.
#' @param caliper maximum distance. Defaults to 0.2 standard deviations
#'   of the (logit) score, the Austin recommendation.
#' @param k matches per treated unit.
#' @param replace allow reuse of controls.
#' @param on_logit match on the logit scale.
#' @return list with \code{matches} (0-based), \code{distances},
#'   \code{n_unmatched}, \code{caliper_used}, \code{match_rate},
#'   \code{estimand}.
#' @references Austin, P. C. (2011). Optimal caliper widths for
#'   propensity-score matching. \emph{Pharmaceutical Statistics}, 10(2),
#'   150-161.
#' @examples
#' set.seed(1)
#' ps <- plogis(rnorm(200))
#' tr <- rbinom(200, 1, ps)
#' morie_causal_caliper_matching(ps, tr)$match_rate
#' @export
morie_causal_caliper_matching <- function(ps, treat, caliper = NULL, k = 1,
                                          replace = TRUE, on_logit = TRUE) {
  e <- as.numeric(ps)
  tr <- as.numeric(treat)
  if (length(e) != length(tr)) {
    stop(sprintf("ps has %d entries but treat has %d", length(e), length(tr)),
         call. = FALSE)
  }
  if (any(e <= 0) || any(e >= 1)) {
    stop("propensity scores must lie strictly inside (0, 1)", call. = FALSE)
  }
  k <- as.integer(k)
  if (k < 1L) stop("k must be at least 1", call. = FALSE)
  scale <- if (on_logit) log(e / (1 - e)) else e
  if (is.null(caliper)) caliper <- 0.2 * stats::sd(scale)
  ti <- which(tr == 1)
  ci <- which(tr == 0)
  if (length(ti) == 0L || length(ci) == 0L) {
    stop("both treatment groups must be non-empty", call. = FALSE)
  }
  matches <- matrix(-1L, length(ti), k)
  dists <- matrix(NA_real_, length(ti), k)
  used <- integer(length(ci))
  for (a in seq_along(ti)) {
    d <- abs(scale[ci] - scale[ti[a]])
    picked <- 0L
    for (j in order(d)) {
      if (!replace && used[j] > 0L) next
      if (d[j] > caliper) break
      picked <- picked + 1L
      matches[a, picked] <- as.integer(ci[j] - 1L)
      dists[a, picked] <- d[j]
      used[j] <- used[j] + 1L
      if (picked == k) break
    }
  }
  ok <- matches[, 1L] >= 0L
  n_un <- as.integer(sum(!ok))
  rate <- mean(ok)
  list(matches = matches, distances = dists, n_unmatched = n_un,
       caliper_used = caliper, match_rate = rate,
       estimand = if (n_un > 0L) "ATT among matchable units" else "ATT",
       matched_treated = as.integer(ti[ok] - 1L), on_logit = on_logit,
       reuse_max = if (length(used)) max(used) else 0L,
       warnings = if (n_un > 0L) {
         sprintf(paste("%d of %d treated units found no match within the",
                       "caliper and are dropped; the estimand is now the ATT",
                       "among matchable units, not the ATT"), n_un, length(ti))
       } else {
         character(0)
       },
       method = "causal_caliper_matching")
}


#' Common-support and positivity diagnostic
#'
#' The propensity overlap between arms: the common support interval, the
#' fraction of units outside it, the fraction with extreme scores, and
#' the overlap coefficient \eqn{\sum_b \min(p_{1b}, p_{0b})}.
#'
#' Positivity is an assumption about the POPULATION and cannot be
#' verified from a sample. An empty region may be structurally
#' impossible or merely unobserved, and no diagnostic distinguishes
#' those. What this function can do is show you that the estimator is
#' extrapolating; it will produce a number either way.
#'
#' @param ps propensity scores.
#' @param treat 0/1 treatment indicator.
#' @param bins histogram bins on `[0, 1]`.
#' @param eps threshold defining an extreme score.
#' @return list with \code{common_support}, \code{n_outside},
#'   \code{prop_extreme}, \code{overlap_coefficient},
#'   \code{hist_treated}, \code{hist_control}.
#' @references Crump, R. K. et al. (2009). Dealing with limited overlap
#'   in estimation of average treatment effects. \emph{Biometrika},
#'   96(1), 187-199.
#' @examples
#' set.seed(1)
#' ps <- plogis(rnorm(300))
#' morie_causal_overlap_diagnostic(ps, rbinom(300, 1, ps))$overlap_coefficient
#' @export
morie_causal_overlap_diagnostic <- function(ps, treat, bins = 20,
                                            eps = 0.05) {
  e <- as.numeric(ps)
  tr <- as.numeric(treat)
  if (length(e) != length(tr)) {
    stop(sprintf("ps has %d entries but treat has %d", length(e), length(tr)),
         call. = FALSE)
  }
  if (!all(tr == 0 | tr == 1)) stop("treat must be 0/1", call. = FALSE)
  t1 <- e[tr == 1]
  t0 <- e[tr == 0]
  if (length(t1) == 0L || length(t0) == 0L) {
    stop("both treatment groups must be non-empty", call. = FALSE)
  }
  lo <- max(min(t1), min(t0))
  hi <- min(max(t1), max(t0))
  outside <- as.integer(sum(e < lo | e > hi))
  extreme <- mean(e < eps | e > 1 - eps)
  nb <- as.integer(bins)
  edges <- seq(0, 1, length.out = nb + 1L)
  cnt <- function(v) {
    tabulate(pmin(pmax(findInterval(v, edges), 1L), nb), nbins = nb)
  }
  h1 <- cnt(t1)
  h0 <- cnt(t0)
  p1 <- h1 / max(sum(h1), 1)
  p0 <- h0 / max(sum(h0), 1)
  ovl <- sum(pmin(p1, p0))
  warn <- character(0)
  if (ovl < 0.5) {
    warn <- c(warn, sprintf(paste("the overlap coefficient is %.2f; the two",
                                  "arms barely occupy the same propensity",
                                  "region and the estimand is close to",
                                  "unidentified"), ovl))
  }
  if (extreme > 0.1) {
    warn <- c(warn, sprintf(paste("%.1f%% of units have propensity beyond %g;",
                                  "weights there are extreme and the estimate",
                                  "is fragile"), 100 * extreme, eps))
  }
  warn <- c(warn, paste("positivity is an assumption about the population and",
                        "cannot be verified from a sample; an empty region may",
                        "be structurally impossible or merely unobserved"))
  list(common_support = c(lo, hi), n_outside = outside,
       prop_extreme = extreme, min_treated_ps = min(t1),
       max_control_ps = max(t0), overlap_coefficient = ovl,
       hist_treated = p1, hist_control = p0, n = length(e),
       warnings = warn, method = "causal_overlap_diagnostic")
}


#' Covariate balance after weighting or matching
#'
#' Standardised mean differences, before and after weighting, plus the
#' variance ratio per covariate.
#'
#' The SMD rather than a t-test, deliberately: a hypothesis test
#' conflates imbalance with sample size, so a large study reports
#' "significant imbalance" on differences too small to matter and a
#' small one reports balance it has no power to reject. The SMD is a
#' magnitude and does not move with n.
#'
#' Balance on MEANS is necessary, not sufficient. Two distributions can
#' share a mean and differ everywhere else, which is what
#' \code{variance_ratio} is for -- it should sit near 1.
#'
#' @param X covariate matrix.
#' @param treat 0/1 treatment indicator.
#' @param weights optional weights, e.g. from
#'   \code{\link{morie_causal_iptw_atoweights}}.
#' @param threshold |SMD| above which a covariate counts as imbalanced.
#' @return list with \code{smd_before}, \code{smd_after},
#'   \code{variance_ratio}, \code{n_imbalanced}, \code{balanced},
#'   \code{worst} (0-based).
#' @references Austin, P. C. and Stuart, E. A. (2015). Moving towards
#'   best practice when using inverse probability of treatment
#'   weighting. \emph{Statistics in Medicine}, 34(28), 3661-3679.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(200), ncol = 2)
#' morie_covariate_balance_check(X, rbinom(100, 1, 0.5))$balanced
#' @export
morie_covariate_balance_check <- function(X, treat, weights = NULL,
                                          threshold = 0.1) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  tr <- as.numeric(treat)
  if (nrow(X) != length(tr)) {
    stop(sprintf("X has %d rows but treat has %d", nrow(X), length(tr)),
         call. = FALSE)
  }
  if (!all(tr == 0 | tr == 1)) stop("treat must be 0/1", call. = FALSE)
  t1 <- tr == 1
  t0 <- tr == 0
  if (!any(t1) || !any(t0)) {
    stop("both treatment groups must be non-empty", call. = FALSE)
  }
  v1 <- apply(X[t1, , drop = FALSE], 2L, stats::var)
  v0 <- apply(X[t0, , drop = FALSE], 2L, stats::var)
  pooled <- sqrt(pmax((v1 + v0) / 2, 1e-300))
  smd_before <- (colMeans(X[t1, , drop = FALSE]) -
                   colMeans(X[t0, , drop = FALSE])) / pooled
  if (is.null(weights)) {
    smd_after <- smd_before
    vr <- v1 / pmax(v0, 1e-300)
  } else {
    w <- as.numeric(weights)
    if (length(w) != length(tr)) {
      stop(sprintf("weights has %d entries but treat has %d",
                   length(w), length(tr)), call. = FALSE)
    }
    wmean <- function(m) {
      ww <- w[m]
      colSums(X[m, , drop = FALSE] * ww) / max(sum(ww), 1e-300)
    }
    wvar <- function(m) {
      ww <- w[m]
      mu <- wmean(m)
      colSums(sweep(X[m, , drop = FALSE], 2L, mu, "-")^2 * ww) /
        max(sum(ww), 1e-300)
    }
    smd_after <- (wmean(t1) - wmean(t0)) / pooled
    vr <- wvar(t1) / pmax(wvar(t0), 1e-300)
  }
  bad <- as.integer(sum(abs(smd_after) > threshold))
  list(smd_before = smd_before, smd_after = smd_after, variance_ratio = vr,
       n_imbalanced = bad, balanced = bad == 0L,
       worst = as.integer(which.max(abs(smd_after)) - 1L),
       threshold = threshold,
       warnings = c(paste("balance on means is necessary, not sufficient;",
                          "check variance_ratio, which should sit near 1"),
                    if (bad > 0L) {
                      sprintf("%d covariates exceed |SMD| = %g", bad, threshold)
                    }),
       method = "covariate_balance_check")
}


#' Propensity weights: ATO overlap, ATE, or ATT
#'
#' Overlap weights \eqn{e(1-e)} for the treated-vs-control contrast on
#' the overlap population, or the usual IPTW forms.
#'
#' The overlap weights are the interesting default. They are bounded by
#' 1/4 and go to zero exactly where the IPTW weights explode, so no unit
#' can dominate -- at the price of estimating an effect for a
#' population defined by the weights rather than one you chose.
#'
#' Trimming is not a robustness fix. Dropping units with extreme scores
#' changes the estimand from the ATE to the ATE among units that
#' survived the trim, and the function records how many were dropped.
#'
#' @param treat 0/1 treatment indicator.
#' @param ps propensity scores, strictly inside (0, 1).
#' @param estimand \code{"ato"}, \code{"ate"} or \code{"att"}.
#' @param trim optional threshold; units outside `[trim, 1-trim]` get
#'   weight 0.
#' @param stabilize stabilise the ATE/ATT weights.
#' @return list with \code{weights}, \code{ess},
#'   \code{max_weight_share}, \code{n_trimmed}, \code{kept}.
#' @references Li, F., Morgan, K. L. and Zaslavsky, A. M. (2018).
#'   Balancing covariates via propensity score weighting. \emph{JASA},
#'   113(521), 390-400.
#' @examples
#' set.seed(1)
#' ps <- plogis(rnorm(200))
#' morie_causal_iptw_atoweights(rbinom(200, 1, ps), ps)$ess
#' @export
morie_causal_iptw_atoweights <- function(treat, ps, estimand = c("ato", "ate",
                                                                 "att"),
                                         trim = NULL, stabilize = TRUE) {
  estimand <- match.arg(estimand)
  tr <- as.numeric(treat)
  e <- as.numeric(ps)
  if (length(tr) != length(e)) {
    stop(sprintf("treat has %d entries but ps has %d", length(tr), length(e)),
         call. = FALSE)
  }
  if (!all(tr == 0 | tr == 1)) stop("treat must be 0/1", call. = FALSE)
  if (any(e <= 0) || any(e >= 1)) {
    stop("propensity scores must lie strictly inside (0, 1)", call. = FALSE)
  }
  keep <- rep(TRUE, length(tr))
  warn <- character(0)
  if (!is.null(trim)) {
    keep <- e >= trim & e <= 1 - trim
    warn <- sprintf(paste("trimming dropped %d units, which changes the",
                          "estimand: this is no longer the ATE but the ATE",
                          "among units that survived the trim"), sum(!keep))
  }
  w <- switch(
    estimand,
    ato = ifelse(tr == 1, 1 - e, e),
    ate = {
      ww <- ifelse(tr == 1, 1 / e, 1 / (1 - e))
      if (stabilize) {
        p <- mean(tr)
        ww * ifelse(tr == 1, p, 1 - p)
      } else {
        ww
      }
    },
    att = {
      ww <- ifelse(tr == 1, 1, e / (1 - e))
      if (stabilize) ww / max(mean(ww), 1e-12) else ww
    }
  )
  w <- ifelse(keep, w, 0)
  tot <- sum(w)
  share <- if (tot > 0) max(w) / tot else NA_real_
  ess <- tot^2 / max(sum(w^2), 1e-300)
  if (is.finite(share) && share > 0.1) {
    warn <- c(warn, sprintf(paste("one unit carries %.1f%% of the total",
                                  "weight; the estimate is dominated by a",
                                  "handful of observations"), 100 * share))
  }
  list(weights = w, estimand = estimand, ess = ess, max_weight_share = share,
       n_trimmed = as.integer(sum(!keep)), kept = keep, n = length(tr),
       warnings = warn, method = "causal_iptw_atoweights")
}


#' Pre-treatment falsification (placebo) regression
#'
#' Regresses a PRE-treatment outcome on the treatment indicator. Since
#' treatment cannot have caused it, a non-zero coefficient is evidence
#' the design is broken.
#'
#' The asymmetry is the point and is routinely misread. FAILING is
#' informative -- it shows the groups already differed. PASSING is not:
#' a null here has only the power the sample gives it, and rules out
#' effects larger than about \code{min_detectable_effect} and nothing
#' smaller. It does not establish parallel trends.
#'
#' @param y_pre pre-treatment outcome.
#' @param treat 0/1 treatment indicator.
#' @param X_baseline optional baseline covariates.
#' @return list with \code{estimate}, \code{se}, \code{z},
#'   \code{p_value}, \code{passed}, \code{min_detectable_effect}.
#' @references Angrist, J. D. and Pischke, J.-S. (2009). \emph{Mostly
#'   Harmless Econometrics}, Sec. 5.2. Princeton University Press.
#' @examples
#' set.seed(1)
#' morie_causal_falsification_test(rnorm(200), rbinom(200, 1, 0.5))$passed
#' @export
morie_causal_falsification_test <- function(y_pre, treat, X_baseline = NULL) {
  y <- as.numeric(y_pre)
  tr <- as.numeric(treat)
  if (length(y) != length(tr)) {
    stop(sprintf("y_pre has %d entries but treat has %d",
                 length(y), length(tr)), call. = FALSE)
  }
  if (!all(tr == 0 | tr == 1)) stop("treat must be 0/1", call. = FALSE)
  if (!any(tr == 1) || !any(tr == 0)) {
    stop("both treatment groups must be non-empty", call. = FALSE)
  }
  A <- cbind(1, tr)
  if (!is.null(X_baseline)) {
    Xb <- as.matrix(X_baseline)
    if (nrow(Xb) != length(y)) Xb <- t(Xb)
    A <- cbind(A, Xb)
  }
  beta <- as.vector(qr.solve(A, y))
  resid <- y - as.vector(A %*% beta)
  dof <- max(length(y) - ncol(A), 1L)
  s2 <- sum(resid^2) / dof
  se <- tryCatch(sqrt(max((s2 * solve(crossprod(A)))[2L, 2L], 0)),
                 error = function(e) NA_real_)
  est <- beta[2L]
  z <- if (is.finite(se) && se > 0) est / se else NA_real_
  p <- if (is.finite(z)) 2 * stats::pnorm(abs(z), lower.tail = FALSE) else NA_real_
  mde <- if (is.finite(se) && se > 0) 2.8 * se else NA_real_
  list(estimate = est, se = se, z = z, p_value = p,
       passed = isTRUE(p > 0.05), min_detectable_effect = mde,
       power_note = sprintf(paste("a null result rules out effects larger",
                                  "than about %.3g, and nothing smaller"), mde),
       n = length(y),
       warnings = paste("failing is informative, passing is not: a null here",
                        "has only the power the sample gives it and does not",
                        "establish parallel trends"),
       method = "causal_falsification_test")
}


#' Entropy balancing
#'
#' Reweights controls so their covariate moments match the treated group
#' EXACTLY, subject to staying as close to uniform as possible.
#'
#' The difference from propensity weighting is where the modelling
#' happens. IPTW models the assignment mechanism and hopes the resulting
#' weights balance; entropy balancing imposes balance as a constraint
#' and solves for the weights. Balance is then exact on the moments
#' specified and says nothing whatever about the ones that were not, nor
#' about unmeasured confounders.
#'
#' Non-convergence is itself the finding: it means no reweighting of the
#' controls can reach the treated moments, which is a positivity
#' problem, not a numerical one.
#'
#' @param X covariate matrix.
#' @param treat 0/1 treatment indicator.
#' @param moments 1 for means, 2 for means and variances.
#' @param max_iter,tol Newton controls.
#' @return list with \code{weights} (on the controls, summing to 1),
#'   \code{lambda}, \code{max_imbalance}, \code{ess}, \code{converged}.
#' @references Hainmueller, J. (2012). Entropy balancing for causal
#'   effects. \emph{Political Analysis}, 20(1), 25-46.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 2)
#' tr <- rbinom(150, 1, 0.4)
#' morie_entropy_balancing(X, tr)$max_imbalance < 1e-6
#' @export
morie_entropy_balancing <- function(X, treat, moments = 1, max_iter = 200L,
                                    tol = 1e-8) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  tr <- as.numeric(treat)
  if (nrow(X) != length(tr)) {
    stop(sprintf("X has %d rows but treat has %d", nrow(X), length(tr)),
         call. = FALSE)
  }
  if (!all(tr == 0 | tr == 1)) stop("treat must be 0/1", call. = FALSE)
  moments <- as.integer(moments)
  if (!moments %in% c(1L, 2L)) stop("moments must be 1 or 2", call. = FALSE)
  t1 <- tr == 1
  t0 <- tr == 0
  if (!any(t1) || !any(t0)) {
    stop("both treatment groups must be non-empty", call. = FALSE)
  }
  design <- function(A) if (moments >= 2L) cbind(A, A^2) else A
  C <- design(X[t0, , drop = FALSE])
  target <- colMeans(design(X[t1, , drop = FALSE]))
  Cc <- sweep(C, 2L, target, "-")
  lam <- numeric(ncol(Cc))
  converged <- FALSE
  for (it in seq_len(max_iter)) {
    z <- as.vector(Cc %*% lam)
    z <- z - max(z)
    w <- exp(z)
    w <- w / sum(w)
    g <- as.vector(w %*% Cc)
    if (max(abs(g)) < tol) {
      converged <- TRUE
      break
    }
    Hm <- crossprod(Cc * w, Cc) - outer(g, g)
    step <- as.vector(tryCatch(solve(Hm, g),
                               error = function(e) .morie_ginv(Hm) %*% g))
    lam <- lam - step
  }
  z <- as.vector(Cc %*% lam)
  z <- z - max(z)
  w <- exp(z)
  w <- w / sum(w)
  imb <- max(abs(as.vector(w %*% Cc)))
  list(weights = w, lambda = lam, balance_achieved = imb < 1e-6,
       max_imbalance = imb, ess = 1 / sum(w^2), n_constraints = ncol(Cc),
       moments = moments, converged = converged, target = target,
       warnings = c(paste("balance is exact only on the moments you",
                          "specified, and says nothing about unmeasured",
                          "confounders"),
                    if (!converged) {
                      paste("the constraints could not be satisfied: the",
                            "treated group may occupy covariate regions the",
                            "controls do not, which is a positivity problem")
                    }),
       method = "entropy_balancing")
}


#' Rosenbaum sensitivity bound for matched pairs
#'
#' Sweeps the odds-ratio bound Gamma and reports the largest value at
#' which the Wilcoxon signed-rank result remains significant.
#'
#' What this does NOT do is test whether hidden bias exists -- nothing
#' can, from observational data. It converts a result into a statement
#' of the form "an unmeasured confounder would have to change treatment
#' odds by a factor of Gamma* within matched pairs to overturn this",
#' and that statement is only informative against the confounders that
#' are plausible in the application.
#'
#' @param paired_diff within-pair outcome differences; zeros dropped.
#' @param gamma_max largest Gamma to try.
#' @param n_gamma grid size.
#' @param alpha significance level.
#' @return list with \code{gamma_critical}, \code{gamma_grid},
#'   \code{p_upper}, \code{significant_at_gamma_1},
#'   \code{interpretation}.
#' @references Rosenbaum, P. R. (2002). \emph{Observational Studies},
#'   2nd ed., Ch. 4. Springer.
#' @examples
#' set.seed(1)
#' morie_causal_rosenbaum_bound(rnorm(60, 0.8))$gamma_critical
#' @export
morie_causal_rosenbaum_bound <- function(paired_diff, gamma_max = 3,
                                         n_gamma = 25, alpha = 0.05) {
  d <- as.numeric(paired_diff)
  d <- d[d != 0]
  n <- length(d)
  if (n < 5L) {
    stop("need at least 5 non-zero pair differences", call. = FALSE)
  }
  if (gamma_max < 1) stop("gamma_max must be at least 1", call. = FALSE)
  # rank(ties = "first") is order(order(.)) + 1, matching the Python's
  # double stable argsort exactly.
  ranks <- order(order(abs(d))) + 0
  ranks <- as.numeric(ranks)
  w_plus <- sum(ranks[d > 0])
  total <- n * (n + 1) / 2
  grid <- seq(1, gamma_max, length.out = as.integer(n_gamma))
  p_up <- vapply(grid, function(g) {
    p <- g / (1 + g)
    mu <- p * total
    var <- p * (1 - p) * sum(ranks^2)
    z <- (w_plus - mu) / sqrt(max(var, 1e-300))
    stats::pnorm(z, lower.tail = FALSE)
  }, numeric(1))
  sig <- p_up < alpha
  gcrit <- if (any(sig)) max(grid[sig]) else 1
  list(gamma_critical = gcrit, gamma_grid = grid, p_upper = p_up,
       significant_at_gamma_1 = p_up[1L] < alpha,
       interpretation = sprintf(paste("an unmeasured confounder would need to",
                                      "change treatment odds by a factor of",
                                      "%.2f within matched pairs to overturn",
                                      "this result"), gcrit),
       n_pairs = n, alpha = alpha,
       warnings = paste("this does not test whether hidden bias exists; it",
                        "states how large it would have to be, and is only",
                        "meaningful against the plausible confounders in the",
                        "application"),
       method = "causal_rosenbaum_bound")
}


#' Triple difference (DDD)
#'
#' A difference-in-differences within each of two groups, then the
#' difference between them.
#'
#' The point is to relax parallel trends: DiD needs the trend to be the
#' same across arms, while DDD only needs the VIOLATION of parallel
#' trends to be common to both groups. That is weaker, not absent --
#' inspect \code{did_placebo}, because a large value means the third
#' difference is doing heavy lifting and the identifying assumption has
#' moved rather than gone away.
#'
#' @param y outcome.
#' @param treated 0/1 treated indicator.
#' @param post 0/1 post-period indicator.
#' @param group 0/1 eligible-group indicator.
#' @return list with \code{ddd}, \code{did_eligible}, \code{did_placebo},
#'   \code{cell_means}, \code{se}, \code{z}, \code{p_value}.
#' @references Gruber, J. (1994). The incidence of mandated maternity
#'   benefits. \emph{American Economic Review}, 84(3), 622-641.
#' @examples
#' set.seed(1)
#' n <- 400
#' g <- rbinom(n, 1, 0.5); tt <- rbinom(n, 1, 0.5); p <- rbinom(n, 1, 0.5)
#' y <- 1 + g + tt + p + 2 * g * tt * p + rnorm(n)
#' round(morie_causal_did_three_way(y, tt, p, g)$ddd, 2)
#' @export
morie_causal_did_three_way <- function(y, treated, post, group) {
  y <- as.numeric(y)
  tr <- as.numeric(treated)
  po <- as.numeric(post)
  gr <- as.numeric(group)
  if (!(length(y) == length(tr) && length(y) == length(po) &&
          length(y) == length(gr))) {
    stop("y, treated, post and group must all have the same length",
         call. = FALSE)
  }
  for (nm in c("treated", "post", "group")) {
    v <- switch(nm, treated = tr, post = po, group = gr)
    if (!all(v == 0 | v == 1)) {
      stop(sprintf("%s must be 0/1", nm), call. = FALSE)
    }
  }
  means <- array(0, c(2L, 2L, 2L))
  vars <- array(0, c(2L, 2L, 2L))
  for (g in 0:1) for (t in 0:1) for (p in 0:1) {
    m <- gr == g & tr == t & po == p
    if (!any(m)) {
      stop(sprintf("cell group=%d treated=%d post=%d is empty", g, t, p),
           call. = FALSE)
    }
    means[g + 1L, t + 1L, p + 1L] <- mean(y[m])
    vars[g + 1L, t + 1L, p + 1L] <- stats::var(y[m]) / sum(m)
  }
  did <- function(g) {
    means[g + 1L, 2L, 2L] - means[g + 1L, 2L, 1L] -
      (means[g + 1L, 1L, 2L] - means[g + 1L, 1L, 1L])
  }
  d1 <- did(1L)
  d0 <- did(0L)
  ddd <- d1 - d0
  se <- sqrt(sum(vars))
  z <- if (se > 0) ddd / se else NA_real_
  list(ddd = ddd, did_eligible = d1, did_placebo = d0, cell_means = means,
       se = se, z = z,
       p_value = if (se > 0) {
         2 * stats::pnorm(abs(z), lower.tail = FALSE)
       } else {
         NA_real_
       },
       n = length(y),
       warnings = paste("DDD assumes the differential trend is the SAME across",
                        "groups; inspect did_placebo, since a large value means",
                        "the third difference is doing heavy lifting"),
       method = "causal_did_three_way")
}
