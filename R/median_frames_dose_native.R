# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native counterparts for morie.fn.mdvtr, morie.fn.smplep and
# morie.fn.ld50r.
#
# morie_median_voter_ci below is the general median-voter routine,
# returning the density-based standard error 1/(2 f(m) sqrt(n)) plus a
# distribution-free interval from the order statistics, with the
# normality-only 1.2533 s/sqrt(n) alongside for comparison. R/mdvtr.R
# is a thin front-end over it; it used to report the normal-theory
# value as `se` outright, which made its intervals wrong on every
# non-normal electorate without saying so. The two part company as soon as the electorate is not
# normal -- on a t(2) sample the normal formula runs over 1.3 times the
# density-based one -- so the difference is not cosmetic.

#' .morie_z
#'
#' Part of the median_frames_dose_native implementation; see the file
#' header for the source it follows.
#'
#' @param p See Usage.
#' @return The value of \code{stats::qnorm}.
#' @export
.morie_z <- function(p) stats::qnorm(p)

#' .morie_binom_cdf_half
#'
#' Part of the median_frames_dose_native implementation; see the file
#' header for the source it follows.
#'
#' @param k See Usage.
#' @param n Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_binom_cdf_half <- function(k, n) {
  if (k < 0) return(0)
  if (k >= n) return(1)
  min(sum(exp(lgamma(n + 1) - lgamma(seq.int(0, k) + 1) -
               lgamma(n - seq.int(0, k) + 1) - n * log(2))), 1)
}

#' .morie_kde_at
#'
#' Part of the median_frames_dose_native implementation; see the file
#' header for the source it follows.
#'
#' @param x A vector; its length is taken.
#' @param point Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_kde_at <- function(x, point) {
  n <- length(x)
  s <- stats::sd(x)
  iqr <- stats::IQR(x)
  spread <- if (iqr > 0) min(s, iqr / 1.349) else s
  if (!is.finite(spread) || spread <= 0) return(NA_real_)
  h <- 0.9 * spread * n^(-0.2)
  u <- (x - point) / h
  mean(exp(-0.5 * u * u) / sqrt(2 * pi)) / h
}

#' Median voter with a standard error that does not assume normality
#'
#' The familiar \eqn{1.2533\,s/\sqrt{n}} is
#' \eqn{\sqrt{\pi/2}\,\sigma/\sqrt{n}}, the asymptotic standard error of
#' the sample median ONLY under normality. The general result is
#' \eqn{1/(2 f(m)\sqrt{n})}, and for a heavy-tailed electorate the
#' normal formula badly overstates the uncertainty because it reads the
#' tails as spread when the median responds only to the density at the
#' centre. Both are returned, with a distribution-free order-statistic
#' interval that assumes neither.
#'
#' With an even electorate every point between the two central ideal
#' points is a Condorcet winner, so the winner is a set; `unique_winner`
#' says whether it collapses to a point.
#'
#' @param x Voter ideal points on one dimension.
#' @param alpha Two-sided level.
#' @return A list with `estimate`, `se` (density-based), `se_normal`,
#'   `ci_lower`, `ci_upper`, `ci_exact_lower`, `ci_exact_upper`,
#'   `exact_coverage`, `median_interval`, `unique_winner`, `warnings`.
#' @references Black D (1948) \emph{Journal of Political Economy}
#'   56(1):23-34, \doi{10.1086/256633}.
#' @export
morie_median_voter_ci <- function(x, alpha = 0.05) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0L) {
    return(list(estimate = NA_real_, se = NA_real_, n = 0L,
                warnings = character(0),
                method = "Median voter theorem (Black 1948)"))
  }
  if (alpha <= 0 || alpha >= 1) {
    stop(sprintf("alpha must lie in (0, 1); got %s", alpha), call. = FALSE)
  }
  xs <- sort(x)
  est <- stats::median(xs)
  if (n %% 2L == 1L) {
    interval <- c(est, est); unique_w <- TRUE
  } else {
    interval <- c(xs[n %/% 2L], xs[n %/% 2L + 1L])
    unique_w <- interval[1] == interval[2]
  }
  if (n > 1L) {
    se_normal <- 1.2533141373155003 * stats::sd(xs) / sqrt(n)
    f_m <- .morie_kde_at(xs, est)
    se <- if (is.finite(f_m) && f_m > 0) 1 / (2 * f_m * sqrt(n)) else NA_real_
  } else {
    se_normal <- NA_real_; se <- NA_real_; f_m <- NA_real_
  }
  zc <- .morie_z(1 - alpha / 2)
  lo <- if (is.finite(se)) est - zc * se else NA_real_
  hi <- if (is.finite(se)) est + zc * se else NA_real_

  elo <- NA_real_; ehi <- NA_real_; cover <- NA_real_
  if (n >= 2L) {
    kk <- 0L
    for (cand in seq_len(n %/% 2L)) {
      if (.morie_binom_cdf_half(cand - 1L, n) <= alpha / 2) kk <- cand else break
    }
    if (kk >= 1L) {
      elo <- xs[kk]; ehi <- xs[n - kk + 1L]
      cover <- 1 - 2 * .morie_binom_cdf_half(kk - 1L, n)
    }
  }
  warns <- character(0)
  if (!unique_w) {
    warns <- c(warns, sprintf(paste(
      "With an even electorate (n = %d) every point in [%g, %g] is a",
      "Condorcet winner. The reported estimate is the midpoint, which is a",
      "convention rather than a result."), n, interval[1], interval[2]))
  }
  if (is.finite(se) && is.finite(se_normal) && se > 0) {
    ratio <- se_normal / se
    if (ratio > 1.15 || ratio < 0.87) {
      warns <- c(warns, sprintf(paste(
        "The normality-based standard error is %.2f times the",
        "density-based one, so this electorate is far from normal and the",
        "1.2533 formula should not be quoted for it."), ratio))
    }
  }
  list(estimate = est, mean = mean(x), se = se, se_normal = se_normal,
       density_at_median = f_m, ci_lower = lo, ci_upper = hi,
       ci_exact_lower = elo, ci_exact_upper = ehi, exact_coverage = cover,
       median_interval = interval, unique_winner = unique_w, n = n,
       warnings = warns, method = "Median voter theorem (Black 1948)")
}

#' Variance-minimising overlap weight for a dual-frame estimator
#'
#' Hartley's estimator is unbiased for every \eqn{\theta} in \\[0, 1\\], so
#' the choice is purely about efficiency. With independent samples the
#' variance \eqn{\theta^2 V_A + (1-\theta)^2 V_B} is minimised at
#' \eqn{\theta^* = V_B/(V_A + V_B)} -- weight goes to the frame that
#' measures the overlap MORE precisely.
#'
#' @param var_a,var_b Domain variances from each frame.
#' @return The optimal weight.
#' @references Hartley HO (1962). Lohr SL, Rao JNK (2000) \emph{JASA}
#'   95(449):271-280, \doi{10.1080/01621459.2000.10473920}.
#' @export
morie_optimal_overlap_weight <- function(var_a, var_b) {
  va <- as.numeric(var_a); vb <- as.numeric(var_b)
  if (va < 0 || vb < 0) stop("variances must be non-negative.", call. = FALSE)
  if (va + vb <= 0) return(0.5)
  vb / (va + vb)
}

#' .morie_domain_total
#'
#' Part of the median_frames_dose_native implementation; see the file
#' header for the source it follows.
#'
#' @param y A vector; indexed elementwise.
#' @param w A vector; indexed elementwise.
#' @param mask See Usage.
#' @return A vector, from \code{c}.
#' @export
.morie_domain_total <- function(y, w, mask) {
  if (!any(mask)) return(c(0, 0))
  contrib <- w[mask] * y[mask]
  m <- length(contrib)
  if (m < 2L) return(c(sum(contrib), 0))
  c(sum(contrib), m * stats::var(contrib))
}

#' Dual-frame total with sample overlap (Hartley 1962)
#'
#' Two frames together cover the population but neither alone does, and
#' they overlap. Splitting into domains a (frame A only), ab (both) and
#' b (frame B only),
#' \deqn{\hat Y = \hat Y_a + \theta \hat Y_{ab}^A +
#'   (1-\theta)\hat Y_{ab}^B + \hat Y_b .}
#'
#' The failure this guards against is the one that looks like success:
#' POOLING THE TWO SAMPLES DOUBLE-COUNTS THE OVERLAP, and the inflated
#' total is stable across replications because it is bias, not noise.
#' More data makes it more precisely wrong. `naive_pooled_total` is
#' returned so the gap is visible rather than argued about.
#'
#' @param frame_a,frame_b Observed values in each frame's sample.
#' @param overlap_a,overlap_b 1 if that unit is also in the other frame.
#' @param weights_a,weights_b Design weights; default 1.
#' @param theta Overlap weight; default the variance-minimising value.
#' @param alpha Two-sided level.
#' @return A list with `estimate`, `theta`, `theta_optimal`, `se`,
#'   `naive_pooled_total`, `overlap_double_count`, domain totals.
#' @references Hartley HO (1962) \emph{Proc Soc Stat Sect ASA} 203-206.
#' @export
morie_dual_frame_total <- function(frame_a, frame_b, overlap_a, overlap_b,
                                   weights_a = NULL, weights_b = NULL,
                                   theta = NULL, alpha = 0.05) {
  ya <- as.numeric(frame_a); yb <- as.numeric(frame_b)
  da <- as.numeric(overlap_a); db <- as.numeric(overlap_b)
  if (length(da) != length(ya)) {
    stop(sprintf("overlap_a has length %d but frame_a has %d.",
                 length(da), length(ya)), call. = FALSE)
  }
  if (length(db) != length(yb)) {
    stop(sprintf("overlap_b has length %d but frame_b has %d.",
                 length(db), length(yb)), call. = FALSE)
  }
  if (length(ya) < 1L || length(yb) < 1L) {
    stop("both frames must contribute at least one unit.", call. = FALSE)
  }
  if (!all(da %in% c(0, 1)) || !all(db %in% c(0, 1))) {
    stop("overlap indicators must be binary 0/1.", call. = FALSE)
  }
  wa <- if (is.null(weights_a)) rep(1, length(ya)) else as.numeric(weights_a)
  wb <- if (is.null(weights_b)) rep(1, length(yb)) else as.numeric(weights_b)
  if (length(wa) != length(ya) || length(wb) != length(yb)) {
    stop("weights must match their frame's length.", call. = FALSE)
  }
  if (any(wa <= 0) || any(wb <= 0)) {
    stop("design weights must be positive.", call. = FALSE)
  }

  ta <- .morie_domain_total(ya, wa, da == 0)
  tb <- .morie_domain_total(yb, wb, db == 0)
  taba <- .morie_domain_total(ya, wa, da == 1)
  tabb <- .morie_domain_total(yb, wb, db == 1)

  th_opt <- morie_optimal_overlap_weight(taba[2], tabb[2])
  if (is.null(theta)) {
    th <- th_opt; src <- "optimal"
  } else {
    th <- as.numeric(theta)
    if (th < 0 || th > 1) {
      stop(sprintf("theta must lie in [0, 1]; got %s", theta), call. = FALSE)
    }
    src <- "user"
  }
  overlap_est <- th * taba[1] + (1 - th) * tabb[1]
  est <- ta[1] + overlap_est + tb[1]
  vr <- ta[2] + tb[2] + th^2 * taba[2] + (1 - th)^2 * tabb[2]
  vopt <- ta[2] + tb[2] + th_opt^2 * taba[2] + (1 - th_opt)^2 * tabb[2]
  se <- sqrt(max(vr, 0))
  naive <- ta[1] + taba[1] + tabb[1] + tb[1]
  zc <- .morie_z(1 - alpha / 2)

  warns <- character(0)
  if (sum(da == 1) == 0 || sum(db == 1) == 0) {
    warns <- c(warns, paste(
      "One frame contributed no overlap units, so the overlap domain rests",
      "on a single frame and theta has no effect. If the frames really do",
      "overlap, the indicators are wrong."))
  }
  if (!is.null(theta) && vopt > 0 && vr / vopt > 1.05) {
    warns <- c(warns, sprintf(paste(
      "The supplied theta = %g carries %.2f times the variance of the",
      "optimal theta = %.4g. The estimate is still unbiased; it is only",
      "less precise."), th, vr / vopt, th_opt))
  }
  list(estimate = est, theta = th, theta_optimal = th_opt,
       theta_source = src, se = se, variance = vr,
       variance_optimal = vopt,
       variance_ratio_vs_optimal = if (vopt > 0) vr / vopt else NA_real_,
       ci_lower = est - zc * se, ci_upper = est + zc * se,
       total_a_only = ta[1], total_b_only = tb[1],
       total_overlap_via_a = taba[1], total_overlap_via_b = tabb[1],
       overlap_estimate = overlap_est, naive_pooled_total = naive,
       overlap_double_count = naive - est,
       n_a = length(ya), n_b = length(yb),
       n = length(ya) + length(yb), warnings = warns,
       method = "Hartley dual-frame estimator with overlap weighting")
}

#' .morie_glm_quantal
#'
#' Part of the median_frames_dose_native implementation; see the file
#' header for the source it follows.
#'
#' @param X A matrix; passed to \code{ncol}.
#' @param k Numeric; passed to \code{sum}.
#' @param n Numeric; passed to \code{sum}.
#' @param link One of \code{"logit"}, \code{"probit"}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100L}.
#' @param tol Defaults to \code{1e-11}.
#' @return A list with \code{beta}, \code{cov}, \code{converged}, \code{fitted}.
#' @export
.morie_glm_quantal <- function(X, k, n, link, max_iter = 100L, tol = 1e-11) {
  mu <- function(eta) {
    e <- pmin(pmax(eta, -8), 8)
    p <- if (link == "probit") stats::pnorm(e) else 1 / (1 + exp(-pmin(pmax(eta, -30), 30)))
    pmin(pmax(p, 1e-12), 1 - 1e-12)
  }
  dmu <- function(eta) {
    e <- pmin(pmax(eta, -8), 8)
    if (link == "probit") pmax(stats::dnorm(e), 1e-10) else {
      p <- mu(eta); pmax(p * (1 - p), 1e-10)
    }
  }
  beta <- rep(0, ncol(X))
  p0 <- min(max(sum(k) / max(sum(n), 1), 0.02), 0.98)
  beta[1] <- if (link == "logit") log(p0 / (1 - p0)) else stats::qnorm(p0)
  converged <- FALSE
  for (i in seq_len(max_iter)) {
    eta <- as.vector(X %*% beta)
    p <- mu(eta); d <- dmu(eta)
    w <- n * d * d / (p * (1 - p))
    z <- eta + (k - n * p) / (n * d)
    XtW <- t(X) * rep(w, each = ncol(X))
    step <- tryCatch(solve(XtW %*% X, XtW %*% z),
                     error = function(e) .morie_ginv(XtW %*% X) %*% (XtW %*% z))
    step <- as.vector(step)
    if (max(abs(step - beta)) < tol) { beta <- step; converged <- TRUE; break }
    beta <- step
  }
  eta <- as.vector(X %*% beta)
  p <- mu(eta); d <- dmu(eta)
  w <- n * d * d / (p * (1 - p))
  A <- (t(X) * rep(w, each = ncol(X))) %*% X
  cov <- tryCatch(solve(A), error = function(e) .morie_ginv(A))
  list(beta = beta, cov = cov, converged = converged, fitted = p)
}

#' Effective dose with Fieller's interval
#'
#' The dose solving \eqn{\mu(\alpha + \beta x) = p} is
#' \eqn{(g_p - \alpha)/\beta}, a RATIO of estimated quantities, whose
#' sampling distribution is not normal. Fieller inverts the test
#' directly. The quantity that decides everything is
#' \eqn{g = t^2 V_{\beta\beta}/\beta^2}: when \eqn{g \ge 1} the leading
#' coefficient changes sign and the interval is UNBOUNDED. That is not a
#' failure to patch over -- if the slope is not distinguishable from
#' zero the data do not bound the median dose, and a delta-method
#' interval returning two tidy finite numbers there is lying.
#'
#' @param intercept,slope Fitted coefficients.
#' @param cov 2x2 covariance matrix.
#' @param level Response level to invert at.
#' @param alpha Two-sided level.
#' @param link "probit" or "logit".
#' @param log_scale Were the doses logged?
#' @return A list with `ed`, `lower`, `upper`, `fieller_g`, `bounded`,
#'   `se_delta`, and dose-scale versions when `log_scale`.
#' @references Fieller EC (1954) \emph{JRSS B} 16(2):175-185,
#'   \doi{10.1111/j.2517-6161.1954.tb00159.x}.
#' @export
morie_effective_dose <- function(intercept, slope, cov, level = 0.5,
                                 alpha = 0.05, link = c("probit", "logit"),
                                 log_scale = TRUE) {
  link <- match.arg(link)
  a <- as.numeric(intercept); b <- as.numeric(slope)
  V <- as.matrix(cov)
  if (!identical(dim(V), c(2L, 2L))) {
    stop(sprintf("cov must be 2x2; got %d x %d", nrow(V), ncol(V)),
         call. = FALSE)
  }
  if (level <= 0 || level >= 1) {
    stop(sprintf("level must lie in (0, 1); got %s", level), call. = FALSE)
  }
  g_p <- if (link == "probit") stats::qnorm(level) else log(level / (1 - level))
  if (b == 0 || !is.finite(b)) {
    return(list(ed = NA_real_, lower = NA_real_, upper = NA_real_,
                fieller_g = Inf, bounded = FALSE, se_delta = NA_real_))
  }
  x <- (g_p - a) / b
  t <- .morie_z(1 - alpha / 2)
  ap <- a - g_p
  g <- t * t * V[2, 2] / (b * b)
  se_delta <- sqrt(max(V[1, 1] + 2 * x * V[1, 2] + x * x * V[2, 2], 0)) / abs(b)

  if (g >= 1) {
    lo <- -Inf; hi <- Inf; bounded <- FALSE
  } else {
    A <- b * b - t * t * V[2, 2]
    B <- 2 * (ap * b - t * t * V[1, 2])
    C <- ap * ap - t * t * V[1, 1]
    disc <- B * B - 4 * A * C
    if (disc < 0) {
      lo <- NA_real_; hi <- NA_real_; bounded <- FALSE
    } else {
      r1 <- (-B - sqrt(disc)) / (2 * A); r2 <- (-B + sqrt(disc)) / (2 * A)
      lo <- min(r1, r2); hi <- max(r1, r2); bounded <- TRUE
    }
  }
  out <- list(ed = x, lower = lo, upper = hi, fieller_g = g,
              bounded = bounded, se_delta = se_delta)
  if (isTRUE(log_scale)) {
    out$ed_dose <- exp(x)
    out$lower_dose <- if (bounded) exp(lo) else NA_real_
    out$upper_dose <- if (bounded) exp(hi) else NA_real_
  }
  out
}

#' Median lethal dose from a quantal assay
#'
#' Groups are exposed at several doses and the number responding at each
#' is recorded; a tolerance-distribution model is fitted and inverted at
#' the 50 per cent point.
#'
#' Doses are logged by default because the tolerance distribution is
#' assumed SYMMETRIC on whatever scale the model is linear in, and
#' tolerances are right-skewed on the natural scale. Fitting on the
#' natural scale is not merely less convenient, it fits a different and
#' usually wrong model.
#'
#' Heterogeneity is tested by the chi-square tail, not by whether the
#' deviance-to-df ratio exceeds 1. A factor above 1 means nothing on its
#' own: the deviance has expectation df, so over 400 correctly specified
#' replications it averaged 0.69 and still exceeded 1 in 20.5 per cent
#' of them. The tail test is conservative on assays with saturated dose
#' groups, since a group fitted within 1e-5 of 0 or 1 contributes no
#' deviance while still spending a degree of freedom.
#'
#' @param dose,n_dead,n_total Assay data, one entry per dose group.
#' @param link "probit" or "logit".
#' @param level Response level to invert at; 0.5 gives the LD50.
#' @param alpha Two-sided level.
#' @param log_dose Fit against log(dose).
#' @return A list with `estimate`, `ci_lower`, `ci_upper`, `slope`,
#'   `fieller_g`, `bounded`, `deviance`, `heterogeneity_factor`,
#'   `heterogeneity_p`, `warnings`.
#' @references Finney DJ (1971) \emph{Probit Analysis}, 3rd ed., Ch 3-4.
#' @export
morie_ld50 <- function(dose, n_dead, n_total, link = c("probit", "logit"),
                       level = 0.5, alpha = 0.05, log_dose = TRUE) {
  link <- match.arg(link)
  d <- as.numeric(dose); k <- as.numeric(n_dead); n <- as.numeric(n_total)
  if (length(d) != length(k) || length(d) != length(n)) {
    stop(sprintf(paste("dose, n_dead and n_total must agree in length;",
                       "got %d, %d and %d"), length(d), length(k), length(n)),
         call. = FALSE)
  }
  if (length(d) < 2L) stop("need at least two dose groups.", call. = FALSE)
  if (any(n <= 0)) stop("n_total must be positive in every group.", call. = FALSE)
  if (any(k < 0) || any(k > n)) {
    stop("n_dead must lie between 0 and n_total.", call. = FALSE)
  }
  if (isTRUE(log_dose) && any(d <= 0)) {
    stop(paste("dose must be positive to fit on the log scale; pass",
               "log_dose = FALSE to fit on the natural scale."), call. = FALSE)
  }
  x <- if (isTRUE(log_dose)) log(d) else d
  X <- cbind(1, x)
  fit <- .morie_glm_quantal(X, k, n, link)
  ed <- morie_effective_dose(fit$beta[1], fit$beta[2], fit$cov, level = level,
                             alpha = alpha, link = link, log_scale = log_dose)

  p_obs <- k / n
  ph <- fit$fitted
  t1 <- ifelse(k > 0, k * log(pmax(p_obs, 1e-12) / ph), 0)
  t2 <- ifelse(n - k > 0, (n - k) * log(pmax(1 - p_obs, 1e-12) / (1 - ph)), 0)
  dev <- 2 * sum(t1 + t2)
  df <- length(d) - 2L
  het <- if (df > 0) dev / df else NA_real_
  het_p <- if (df > 0) stats::pchisq(dev, df, lower.tail = FALSE) else NA_real_

  est <- if (isTRUE(log_dose)) ed$ed_dose else ed$ed
  lo <- if (isTRUE(log_dose)) ed$lower_dose else ed$lower
  hi <- if (isTRUE(log_dose)) ed$upper_dose else ed$upper

  warns <- character(0)
  if (!fit$converged) {
    warns <- c(warns, paste("Fisher scoring did not converge. The estimate",
                            "and its interval should not be used."))
  }
  if (!ed$bounded) {
    warns <- c(warns, sprintf(paste(
      "Fieller's g = %.3g is at or above 1, so the slope is not",
      "distinguishable from zero at this level and the interval is",
      "unbounded. The data do not bound the median dose."), ed$fieller_g))
  }
  if (is.finite(het_p) && het_p < 0.05) {
    warns <- c(warns, sprintf(paste(
      "The residual deviance is %.3g on %d degrees of freedom",
      "(heterogeneity factor %.2f, p = %.4f). Subjects did not respond",
      "independently within dose groups."), dev, df, het, het_p))
  }
  if (any(k == 0 | k == n)) {
    warns <- c(warns, paste("One or more groups responded at 0 or 100 per",
                            "cent. These carry little information about the",
                            "slope and can make the fit unstable."))
  }
  list(estimate = est, ed_log = ed$ed, ci_lower = lo, ci_upper = hi,
       ci_lower_log = ed$lower, ci_upper_log = ed$upper,
       se_log = ed$se_delta, intercept = fit$beta[1], slope = fit$beta[2],
       cov = fit$cov, fitted = ph, fieller_g = ed$fieller_g,
       bounded = ed$bounded, deviance = dev, df_residual = df,
       heterogeneity_factor = het, heterogeneity_p = het_p,
       link = link, converged = fit$converged, level = level,
       n_groups = length(d), n = sum(n), warnings = warns,
       method = "Median lethal dose by probit/logit with Fieller limits")
}
