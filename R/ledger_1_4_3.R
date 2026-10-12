# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research ledger, the 2026-10-12 closures: eight identification results with their R side.
# Every statement is a machine-checked theorem (Lean 4 + Mathlib, 0 sorry, standard axioms
# only) in research/lean/Researchproofs/:
#
#   P2Veil.lean          Research.P2.Veil.odds_ratio_identifies_bias, share_sign,
#                        exposure_shift_confounds, bias_not_absolute
#   P1Interaction.lean   Research.P1.Interaction.missing_cell_identified,
#                        missing_cell_with_interaction, total_identified, interaction_not_identified
#   P19Error.lean        Research.P19Error.bstar_from_total, bhat_error, bhat_error_bound,
#                        fall_error, bhat_antitone
#   P7Bound.lean         Research.P7.Bound.gini_le_cv (pair_sq_identity, abs_sum_le_sqrt)
#   P8Instrument.lean    Research.P8.Instrument.logcost_linear, ratio_is_discount,
#                        wald_identifies, wald_undefined
#   P15RIF.lean          Research.P15.RIF.rif_quantile_avg, rif_quantile_avg_eq_iff, rif_gap
#   P10Moments.lean      Research.P10.Moments.second_moments_do_not_identify (witness laws)
#   P6Panel.lean         Research.P6.Panel.cross_moment_eq, one_type_cov_zero, two_type_cov,
#                        panel_separates_witness

#' The veil-of-darkness contrast as a sign test with an identified bias ratio
#'
#' Stops of each group are counted in daylight and in darkness within the
#' inter-twilight window, where the same people drive under both conditions.
#' With latent exposures and dark propensities common to both conditions, the
#' ratio of daylight to darkness odds of group \eqn{g} against the reference is
#' exactly the ratio of visible-race factors \eqn{(1+\delta_g)/(1+\delta_{ref})}
#' (\code{Research.P2.Veil.odds_ratio_identifies_bias}): the exposures cancel,
#' the relative bias is point-identified and its sign is the sign of the
#' daylight-minus-darkness share difference (\code{share_sign}). A differential
#' change of who drives by light condition, \eqn{\kappa_g}, is observationally a
#' bias (\code{exposure_shift_confounds}); passing it divides the ratio out.
#'
#'
#' @param day,night Named counts of stops by group in daylight and in darkness.
#'
#' @param reference Name of the reference group.
#'
#' @param exposure_shift Optional named factors \eqn{\kappa_g} by which daylight
#'   exposure differs from darkness exposure; the bias ratio is divided by
#'   \eqn{\kappa_g/\kappa_{ref}}.
#'
#' @return A data frame with one row per group: the two counts, the daylight and
#'   darkness shares, \code{bias_ratio} (relative visible-race factor against the
#'   reference), \code{sign} (\code{"higher"}, \code{"lower"} or \code{"equal"}) and,
#'   with \code{exposure_shift}, \code{bias_ratio_adjusted}.
#'
#' @references Grogger J, Ridgeway G (2006). Testing for racial profiling in traffic
#'   stops from behind a veil of darkness. JASA 101(475).
#' @examples
#' morie_veil_of_darkness(day = c(white = 400, black = 200), night = c(white = 300, black = 100))
#' @export
morie_veil_of_darkness <- function(day, night, reference = names(day)[1], exposure_shift = NULL) {
  if (is.null(names(day)) || !identical(sort(names(day)), sort(names(night)))) {
    stop("`day` and `night` must be named counts over the same groups", call. = FALSE)
  }
  if (!reference %in% names(day)) stop("`reference` is not a group", call. = FALSE)
  night <- night[names(day)]
  if (any(day < 0) || any(night <= 0)) stop("counts must be non-negative, darkness counts positive", call. = FALSE)
  share_day <- day / sum(day)
  share_night <- night / sum(night)
  ratio <- (day / day[[reference]]) / (night / night[[reference]])
  diff <- share_day - share_night
  out <- data.frame(group = names(day), day = as.numeric(day), night = as.numeric(night),
                    share_day = as.numeric(share_day), share_night = as.numeric(share_night),
                    bias_ratio = as.numeric(ratio),
                    sign = ifelse(abs(diff) < 1e-12, "equal", ifelse(diff > 0, "higher", "lower")),
                    stringsAsFactors = FALSE)
  if (!is.null(exposure_shift)) {
    if (is.null(names(exposure_shift)) || !all(names(day) %in% names(exposure_shift))) {
      stop("`exposure_shift` must be named over the groups", call. = FALSE)
    }
    k <- exposure_shift[names(day)]
    out$bias_ratio_adjusted <- as.numeric(ratio / (k / k[[reference]]))
  }
  attr(out, "theorem") <- "Research.P2.Veil.odds_ratio_identifies_bias"
  out
}

#' The never-listed cell of a three-list capture under no three-way interaction
#'
#' With the saturated log-linear model the never-listed cell is free
#' (\code{Research.P1.three_list_saturated_fits}); under the conventional restriction
#' \eqn{u_{123} = 0} it is the closed form
#' \eqn{m_{000} = m_{100} m_{010} m_{001} m_{111} / (m_{110} m_{101} m_{011})}
#' (\code{Research.P1.Interaction.missing_cell_identified}), and with a three-way
#' interaction \eqn{t} the same ratio equals \eqn{m_{000} e^{t}}
#' (\code{missing_cell_with_interaction}), so the dark figure moves by exactly
#' \eqn{e^{-t}} per unit of the interaction the analyst assumes away.
#'
#'
#' @param cells Named counts \code{n100, n010, n001, n110, n101, n011, n111} (list
#'   memberships as the three digits).
#'
#' @param interaction Optional vector of three-way interaction values \eqn{t} at which
#'   the implied never-listed cell is also reported.
#'
#' @return A list with \code{missing} (the never-listed cell under \eqn{u_{123} = 0}),
#'   \code{observed}, \code{total}, and, with \code{interaction}, a data frame
#'   \code{sensitivity} of \code{interaction}, \code{missing} and \code{total}.
#'
#' @references Fienberg SE (1972). The multiple recapture census for closed populations
#'   and incomplete 2^k contingency tables. Biometrika 59(3).
#' @examples
#' morie_dark_figure_no_interaction(c(n100 = 120, n010 = 80, n001 = 60, n110 = 30, n101 = 24,
#'                                    n011 = 16, n111 = 8), interaction = c(-0.5, 0, 0.5))
#' @export
morie_dark_figure_no_interaction <- function(cells, interaction = NULL) {
  need <- c("n100", "n010", "n001", "n110", "n101", "n011", "n111")
  if (is.null(names(cells)) || !all(need %in% names(cells))) {
    stop("`cells` must be named ", paste(need, collapse = ", "), call. = FALSE)
  }
  n <- as.numeric(cells[need])
  names(n) <- need
  if (any(n < 0)) stop("counts must be non-negative", call. = FALSE)
  if (any(n[c("n110", "n101", "n011")] == 0)) stop("the three pairwise cells must be positive", call. = FALSE)
  missing <- n[["n100"]] * n[["n010"]] * n[["n001"]] * n[["n111"]] / (n[["n110"]] * n[["n101"]] * n[["n011"]])
  observed <- sum(n)
  out <- list(missing = missing, observed = observed, total = observed + missing,
              theorem = "Research.P1.Interaction.missing_cell_identified")
  if (!is.null(interaction)) {
    t <- as.numeric(interaction)
    out$sensitivity <- data.frame(interaction = t, missing = missing * exp(-t),
                                  total = observed + missing * exp(-t))
  }
  out
}

#' Error of the empirical-Bayes shrinkage factor when the total mass is estimated
#'
#' The factor \eqn{B^* = S_e / S_y} needs the total mass \eqn{S_y = S_e + S_\theta}
#' (\code{Research.P19Error.bstar_from_total}), estimated from the same counts. The
#' plug-in \eqn{\hat B = S_e/\hat S_y} is off by \eqn{S_e (S_y - \hat S_y)/(S_y \hat S_y)}
#' (\code{bhat_error}), at most \eqn{B^* |\hat S_y - S_y| / \hat S_y}
#' (\code{bhat_error_bound}), and the predicted fall of a place inherits the same
#' error times its excess over the mean (\code{fall_error}). Given the counts, the
#' total mass is estimated by their weighted variance and its uncertainty by the
#' leave-one-out jackknife.
#'
#'
#' @param y Counts, one per place.
#'
#' @param noise_variance The stated noise mass \eqn{S_e} (for Poisson counts, the mean).
#'
#' @param weights Optional non-negative weights.
#'
#' @param total_variance Optional true total mass, when known, to evaluate the exact error.
#'
#' @return A list with \code{B_hat}, \code{S_y_hat}, \code{S_y_se} (jackknife),
#'   \code{B_interval} (the factor at \eqn{\hat S_y \pm 2} se, by \code{bhat_antitone}),
#'   \code{error_bound} (the bound at the 2-se relative error), and, with
#'   \code{total_variance}, \code{error} and \code{bound_holds}.
#' @examples
#' set.seed(1)
#' y <- rpois(40, 6)
#' morie_shrinkage_factor_error(y, noise_variance = mean(y))
#' @export
morie_shrinkage_factor_error <- function(y, noise_variance, weights = NULL, total_variance = NULL) {
  y <- as.numeric(y)
  n <- length(y)
  if (n < 3L || anyNA(y)) stop("`y` needs at least three non-missing counts", call. = FALSE)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n || any(w < 0) || sum(w) <= 0) stop("`weights` must be non-negative with a positive sum", call. = FALSE)
  if (!is.numeric(noise_variance) || length(noise_variance) != 1L || noise_variance < 0) {
    stop("`noise_variance` must be a single non-negative number", call. = FALSE)
  }
  wvar <- function(v, ww) {
    m <- sum(ww * v) / sum(ww)
    sum(ww * (v - m)^2) / sum(ww)
  }
  s_hat <- wvar(y, w)
  if (s_hat <= 0) stop("the counts have no spread; the total mass is zero", call. = FALSE)
  loo <- vapply(seq_len(n), function(i) wvar(y[-i], w[-i]), numeric(1))
  s_se <- sqrt((n - 1) / n * sum((loo - mean(loo))^2))
  b_hat <- noise_variance / s_hat
  lo <- max(s_hat - 2 * s_se, .Machine$double.eps)
  hi <- s_hat + 2 * s_se
  out <- list(B_hat = b_hat, S_y_hat = s_hat, S_y_se = s_se,
              B_interval = c(lower = noise_variance / hi, upper = noise_variance / lo),
              error_bound = b_hat * (2 * s_se) / s_hat,
              theorem = "Research.P19Error.bhat_error_bound")
  if (!is.null(total_variance)) {
    b_star <- noise_variance / total_variance
    out$B_star <- b_star
    out$error <- b_hat - b_star
    out$bound_holds <- abs(out$error) <= b_star * abs(s_hat - total_variance) / s_hat + 1e-12
  }
  out
}

#' How much Gini concentration chance alone can show
#'
#' For any count vector the Gini index is at most \eqn{\sqrt{v/2}/\bar x}, the
#' coefficient of variation over \eqn{\sqrt 2} (\code{Research.P7.Bound.gini_le_cv}).
#' Under a Poisson null the variance is the mean, so the null Gini is at most about
#' \eqn{1/\sqrt{2\bar x}}; the exact null distribution is simulated as well.
#'
#'
#' @param x Non-negative counts over places.
#'
#' @param n_sim Number of Poisson-null simulations.
#'
#' @param seed Seed for the simulation.
#'
#' @return A list with \code{gini}, \code{mean}, \code{variance}, \code{cv_bound}
#'   (\eqn{\sqrt{v/2}/\bar x}, which the Gini never exceeds), \code{null_bound}
#'   (\eqn{1/\sqrt{2\bar x}}), \code{null_mean}, \code{null_q95}, \code{excess}
#'   (observed minus null mean) and \code{p_value} (share of null Ginis at or above
#'   the observed).
#' @examples
#' morie_concentration_null_bound(c(0, 0, 1, 2, 9, 0, 3))
#' @export
morie_concentration_null_bound <- function(x, n_sim = 2000L, seed = 1L) {
  x <- as.numeric(x)
  if (length(x) < 2L || anyNA(x) || any(x < 0) || sum(x) <= 0) {
    stop("`x` must be non-negative counts with a positive total", call. = FALSE)
  }
  n <- length(x)
  m <- mean(x)
  v <- mean((x - m)^2)
  g <- morie_concentration_gini(x)
  cv_bound <- sqrt(v / 2) / m
  null_bound <- 1 / sqrt(2 * m)
  set.seed(seed)
  sims <- vapply(seq_len(n_sim), function(i) {
    s <- stats::rpois(n, m)
    if (sum(s) == 0) 0 else morie_concentration_gini(s)
  }, numeric(1))
  list(gini = g, mean = m, variance = v, cv_bound = cv_bound, null_bound = null_bound,
       null_mean = mean(sims), null_q95 = stats::quantile(sims, 0.95, names = FALSE),
       excess = g - mean(sims), p_value = mean(sims >= g), n = n,
       theorem = "Research.P7.Bound.gini_le_cv")
}

#' The expected-cost deterrence response on the log scale, and the discount it implies
#'
#' The expected cost of an offence under certainty \eqn{p}, severity \eqn{s} and
#' delay \eqn{c} with discount rate \eqn{\rho} is \eqn{p s e^{-\rho c}}, whose
#' logarithm is \eqn{\log p + \log s - \rho c}
#' (\code{Research.P8.Instrument.logcost_linear}). A response linear in the log
#' design \eqn{(\log p, \log s, c)} is this model with a common elasticity, and the
#' ratio of the celerity to the certainty coefficient identifies \eqn{-\rho}
#' (\code{ratio_is_discount}). Identification of the three coefficients is the rank
#' condition of \code{\link{morie_deterrence_design_check}} on the transformed design.
#'
#'
#' @param p,s,c Certainty (in (0, 1]), severity (positive) and delay of the sanction, one
#'   observation each.
#'
#' @param y Optional response; when given the log-design least-squares fit is returned.
#'
#' @param rho Optional discount rate at which the expected cost is evaluated.
#'
#' @return A list with the log design check (\code{identified}, \code{rank}), and, with
#'   \code{y}, \code{coefficients} and \code{implied_discount}; with \code{rho},
#'   \code{expected_cost} and \code{log_expected_cost}.
#' @examples
#' set.seed(2)
#' p <- runif(40, 0.1, 0.9); s <- runif(40, 1, 10); c <- runif(40, 0, 12)
#' y <- 2 - 0.8 * log(p) - 0.8 * log(s) + 0.8 * 0.1 * c + rnorm(40, sd = 0.1)
#' morie_deterrence_logcost(p, s, c, y)$implied_discount
#' @export
morie_deterrence_logcost <- function(p, s, c, y = NULL, rho = NULL) {
  p <- as.numeric(p)
  s <- as.numeric(s)
  c <- as.numeric(c)
  if (any(p <= 0) || any(s <= 0)) stop("`p` and `s` must be positive", call. = FALSE)
  n <- length(p)
  if (length(s) != n || length(c) != n) stop("`p`, `s` and `c` must have one value per observation", call. = FALSE)
  X <- cbind(1, log(p), log(s), c)
  colnames(X) <- c("(Intercept)", "log_p", "log_s", "c")
  rk <- qr(X)$rank
  out <- list(design = X, rank = rk, identified = rk == 4L, theorem = "Research.P8.Instrument.logcost_linear")
  if (!is.null(y)) {
    y <- as.numeric(y)
    if (length(y) != n) stop("`y` must have one value per observation", call. = FALSE)
    if (rk < 4L) stop("the log design is rank deficient: a dimension held fixed is not identified", call. = FALSE)
    b <- as.numeric(qr.coef(qr(X), y))
    names(b) <- colnames(X)
    out$coefficients <- b
    out$implied_discount <- if (abs(b[["log_p"]]) > 0) -b[["c"]] / b[["log_p"]] else NA_real_
  }
  if (!is.null(rho)) {
    out$expected_cost <- p * s * exp(-rho * c)
    out$log_expected_cost <- log(p) + log(s) - rho * c
  }
  out
}

#' The Wald instrumental-variable ratio on a finite population
#'
#' With \eqn{y = \alpha + \beta p + u}, an instrument \eqn{z} with \eqn{cov(z, u) = 0}
#' and \eqn{cov(z, p) \neq 0} gives \eqn{\beta = cov(z, y) / cov(z, p)}
#' (\code{Research.P8.Instrument.wald_identifies}); without relevance every slope
#' yields the same instrument covariance (\code{wald_undefined}).
#'
#'
#' @param y Outcome.
#' @param p The sanction dimension to be instrumented.
#'
#' @param z The instrument.
#'
#' @return A list with \code{beta}, \code{cov_zy}, \code{cov_zp}, \code{relevant}
#'   and \code{alpha} (the intercept from the means).
#' @examples
#' set.seed(3)
#' z <- rbinom(200, 1, 0.5); p <- 0.3 + 0.4 * z + rnorm(200, sd = 0.1)
#' y <- 1 - 2 * p + rnorm(200, sd = 0.2)
#' morie_wald_iv(y, p, z)$beta
#' @export
morie_wald_iv <- function(y, p, z) {
  y <- as.numeric(y)
  p <- as.numeric(p)
  z <- as.numeric(z)
  n <- length(y)
  if (length(p) != n || length(z) != n || n < 2L) stop("`y`, `p` and `z` must have the same length, at least 2", call. = FALSE)
  cv <- function(a, b) mean(a * b) - mean(a) * mean(b)
  czy <- cv(z, y)
  czp <- cv(z, p)
  if (abs(czp) < 1e-12) {
    return(list(beta = NA_real_, cov_zy = czy, cov_zp = czp, relevant = FALSE,
                note = "the instrument does not move p: the slope is not identified (wald_undefined)",
                theorem = "Research.P8.Instrument.wald_undefined"))
  }
  beta <- czy / czp
  list(beta = beta, cov_zy = czy, cov_zp = czp, relevant = TRUE, alpha = mean(y) - beta * mean(p),
       theorem = "Research.P8.Instrument.wald_identifies")
}

#' Recentered influence function of a quantile, and the gap it carries
#'
#' The RIF of the \eqn{\tau}-quantile at a point \eqn{q} with density \eqn{f} is
#' \eqn{q + (\tau - 1\{y \le q\})/f}; its weighted mean is \eqn{q + (\tau - F(q))/f}
#' (\code{Research.P15.RIF.rif_quantile_avg}) and equals \eqn{q} exactly when
#' \eqn{F(q) = \tau} (\code{rif_quantile_avg_eq_iff}). The gap between two groups'
#' RIF means is the quantile gap plus the two recentering terms (\code{rif_gap}).
#'
#'
#' @param y Outcomes.
#' @param tau Quantile level in (0, 1).
#'
#' @param weights Optional non-negative weights.
#'
#' @param q Evaluation point; the weighted \eqn{\tau}-quantile by default.
#'
#' @param f Density value at \code{q}; a kernel estimate by default.
#'
#' @return A list with \code{rif} (one value per outcome), \code{q}, \code{f},
#'   \code{F_q} (weighted share at or below \code{q}), \code{mean_rif} and
#'   \code{recentering} (\eqn{(\tau - F(q))/f}, zero at the sample quantile).
#' @examples
#' set.seed(4)
#' r <- morie_rif_quantile(rexp(300), 0.5)
#' c(r$q, r$mean_rif)
#' @export
morie_rif_quantile <- function(y, tau, weights = NULL, q = NULL, f = NULL) {
  y <- as.numeric(y)
  n <- length(y)
  if (n < 2L || anyNA(y)) stop("`y` needs at least two non-missing values", call. = FALSE)
  if (!is.numeric(tau) || length(tau) != 1L || tau <= 0 || tau >= 1) stop("`tau` must lie in (0, 1)", call. = FALSE)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n || any(w < 0) || sum(w) <= 0) stop("`weights` must be non-negative with a positive sum", call. = FALSE)
  if (is.null(q)) {
    o <- order(y)
    cw <- cumsum(w[o]) / sum(w)
    q <- y[o][which(cw >= tau)[1]]
  }
  if (is.null(f)) {
    d <- stats::density(y, weights = w / sum(w), bw = stats::bw.nrd0(y))
    f <- stats::approx(d$x, d$y, xout = q, rule = 2)$y
  }
  if (!is.numeric(f) || f <= 0) stop("the density at `q` must be positive", call. = FALSE)
  below <- as.numeric(y <= q)
  rif <- q + (tau - below) / f
  F_q <- sum(w * below) / sum(w)
  list(rif = rif, q = q, f = f, F_q = F_q, mean_rif = sum(w * rif) / sum(w),
       recentering = (tau - F_q) / f, theorem = "Research.P15.RIF.rif_quantile_avg")
}

#' @rdname morie_rif_quantile
#'
#' @param yA,yB Outcomes of the two groups.
#' @param wA,wB Their optional weights.
#'
#' @return For \code{morie_rif_gap}: a list with the two groups' RIF results, \code{gap}
#'   (difference of RIF means), \code{quantile_gap} and \code{recentering_gap}, with
#'   \code{gap = quantile_gap + recentering_gap}.
#' @examples
#' set.seed(5)
#' morie_rif_gap(rexp(200, 1), rexp(200, 0.7), 0.5)$gap
#' @export
morie_rif_gap <- function(yA, yB, tau, wA = NULL, wB = NULL) {
  a <- morie_rif_quantile(yA, tau, wA)
  b <- morie_rif_quantile(yB, tau, wB)
  list(A = a, B = b, gap = a$mean_rif - b$mean_rif, quantile_gap = a$q - b$q,
       recentering_gap = a$recentering - b$recentering, theorem = "Research.P15.RIF.rif_gap")
}

#' Two count laws with the same mean and variance: what a K-function cannot tell apart
#'
#' Contagion and heterogeneity can give the same first and second moments
#' (Bartlett 1964), which is all a dispersion index or a K-function sees. The
#' witness laws on \{0, 1, 2, 3\} are \eqn{p = (1/2, 0, 1/2, 0)} and
#' \eqn{q = (1/3, 1/2, 0, 1/6)}: mean 1 and variance 1 in both, dispersion index one,
#' yet \eqn{P(0)} of 1/2 against 1/3 (\code{Research.P10.Moments.second_moments_do_not_identify}).
#'
#'
#' @param statistic Optional function of (mean, variance) to evaluate on both laws.
#'
#' @return A list with the two laws, their moments, \code{dispersion} (both 1),
#'   \code{zero_probability} (1/2 and 1/3), and, with \code{statistic}, its two values.
#' @examples
#' morie_contagion_moment_witness(function(m, v) v / m)$statistic
#' @export
morie_contagion_moment_witness <- function(statistic = NULL) {
  p <- c(1 / 2, 0, 1 / 2, 0)
  q <- c(1 / 3, 1 / 2, 0, 1 / 6)
  k <- 0:3
  mom <- function(r, j) sum(r * k^j)
  out <- list(p = p, q = q, support = k,
              mean = c(p = mom(p, 1), q = mom(q, 1)),
              variance = c(p = mom(p, 2) - mom(p, 1)^2, q = mom(q, 2) - mom(q, 1)^2),
              zero_probability = c(p = p[1], q = q[1]),
              theorem = "Research.P10.Moments.second_moments_do_not_identify")
  out$dispersion <- out$variance / out$mean
  if (!is.null(statistic)) {
    out$statistic <- c(p = statistic(out$mean[["p"]], out$variance[["p"]]),
                       q = statistic(out$mean[["q"]], out$variance[["q"]]))
  }
  out
}

#' Cross-age covariance of a type mixture, and its test on panel counts
#'
#' With types of weight \eqn{w_k} and rates \eqn{\lambda_k(a), \lambda_k(b)} at two
#' ages, counts conditionally independent given type have cross-age covariance
#' \eqn{\sum_k w_k (\lambda_k(a) - m_a)(\lambda_k(b) - m_b)}
#' (\code{Research.P6.Panel.cross_moment_eq}): zero for one type
#' (\code{one_type_cov_zero}), \eqn{w_1 w_2 (\lambda_1(a)-\lambda_2(a))(\lambda_1(b)-\lambda_2(b))}
#' for two (\code{two_type_cov}). The aggregate curve is blind to it
#' (\code{aggregate_blind}); the panel is not (\code{panel_separates_witness}).
#'
#'
#' @param w Type weights summing to one.
#' @param rate_a,rate_b Type rates at the two ages.
#'
#' @return For \code{morie_age_crime_type_cov}: a list with \code{aggregate} (the two
#'   aggregate rates), \code{cross_cov} and \code{one_type} (whether the covariance is zero).
#' @examples
#' morie_age_crime_type_cov(c(0.5, 0.5), c(1, 3), c(3, 1))$cross_cov
#' @export
morie_age_crime_type_cov <- function(w, rate_a, rate_b) {
  w <- as.numeric(w)
  rate_a <- as.numeric(rate_a)
  rate_b <- as.numeric(rate_b)
  if (length(w) != length(rate_a) || length(w) != length(rate_b)) stop("one weight and two rates per type", call. = FALSE)
  if (abs(sum(w) - 1) > 1e-10 || any(w < 0)) stop("`w` must be non-negative weights summing to one", call. = FALSE)
  ma <- sum(w * rate_a)
  mb <- sum(w * rate_b)
  cc <- sum(w * (rate_a - ma) * (rate_b - mb))
  list(aggregate = c(a = ma, b = mb), cross_cov = cc, one_type = abs(cc) < 1e-12,
       theorem = "Research.P6.Panel.cross_moment_eq")
}

#' @rdname morie_age_crime_type_cov
#'
#' @param y_a,y_b Counts of the same individuals at the two ages.
#'
#' @param n_perm Number of permutations for the exact null (one type: the pairing carries no
#'   information, so permuting one age across individuals preserves the null law).
#'
#' @param seed Seed for the permutations.
#'
#' @return For \code{morie_age_crime_panel_cov}: a list with \code{cross_cov} (the sample
#'   cross-age covariance), \code{p_value} (permutation, two-sided) and \code{n}.
#' @examples
#' set.seed(6)
#' type <- rbinom(300, 1, 0.5)
#' ya <- rpois(300, ifelse(type == 1, 1, 3)); yb <- rpois(300, ifelse(type == 1, 3, 1))
#' morie_age_crime_panel_cov(ya, yb)$p_value
#' @export
morie_age_crime_panel_cov <- function(y_a, y_b, n_perm = 2000L, seed = 1L) {
  y_a <- as.numeric(y_a)
  y_b <- as.numeric(y_b)
  n <- length(y_a)
  if (length(y_b) != n || n < 3L || anyNA(y_a) || anyNA(y_b)) stop("`y_a` and `y_b` need the same individuals, at least three", call. = FALSE)
  cv <- function(a, b) mean(a * b) - mean(a) * mean(b)
  obs <- cv(y_a, y_b)
  set.seed(seed)
  perm <- vapply(seq_len(n_perm), function(i) cv(y_a, y_b[sample.int(n)]), numeric(1))
  list(cross_cov = obs, p_value = (1 + sum(abs(perm) >= abs(obs) - 1e-12)) / (n_perm + 1), n = n,
       theorem = "Research.P6.Panel.one_type_cov_zero")
}

#' Drift envelope of the discovery urn at any rates
#'
#' The stochastic discovery urn starts at counts \eqn{(r_0, b_0)}, adds \eqn{a} to
#' the region drawn, and draws region A with probability equal to its share. The
#' share is a martingale (\code{Research.P4.Envelope.mean_const}), one step adds
#' \eqn{s(1-s)(a/(T+a))^2 \le (a/(T+a))^2/4} to its second moment
#' (\code{step_var}), and the sum telescopes, so the variance of the share after
#' any number of steps is at most \eqn{a/(4(r_0+b_0))} (\code{var_le}) and the
#' share leaves a band of half-width \eqn{t} about its start with probability at
#' most \eqn{a/(4(r_0+b_0)t^2)} (\code{chebyshev}). The exact law after \code{n}
#' steps is computed by the urn's recursion for comparison.
#'
#'
#' @param r0,b0 Initial counts of the two regions (positive).
#'
#' @param a Reinforcement per discovery (positive).
#'
#' @param n Horizon at which the exact law is evaluated.
#'
#' @param t Half-width of the band about the initial share.
#'
#' @return A list with \code{share0}, \code{variance_bound} (\eqn{a/(4(r_0+b_0))}),
#'   \code{chebyshev_bound} (the band-exit probability bound at \code{t}),
#'   \code{variance_n} (the exact variance of the share after \code{n} steps),
#'   \code{mean_n} (its exact mean, equal to \code{share0}), \code{exit_n} (the
#'   exact band-exit probability) and \code{law} (the exact law of the number of
#'   A-draws).
#' @examples
#' morie_feedback_loop_envelope(1, 1, 1, n = 50, t = 0.25)[c("variance_bound", "variance_n")]
#' @export
morie_feedback_loop_envelope <- function(r0, b0, a = 1, n = 100L, t = 0.25) {
  if (!all(c(r0, b0, a) > 0) || length(c(r0, b0, a)) != 3L) stop("`r0`, `b0` and `a` must be positive numbers", call. = FALSE)
  if (!is.numeric(t) || length(t) != 1L || t <= 0) stop("`t` must be a positive half-width", call. = FALSE)
  n <- as.integer(n)
  if (n < 0L) stop("`n` must be a non-negative horizon", call. = FALSE)
  share <- function(k, j) (r0 + j * a) / (r0 + b0 + k * a)
  q <- 1
  for (k in seq_len(n)) {
    j <- 0:(k - 1L)
    s <- share(k - 1L, j)
    nq <- numeric(k + 1L)
    nq[1:k] <- nq[1:k] + q * (1 - s)
    nq[2:(k + 1L)] <- nq[2:(k + 1L)] + q * s
    q <- nq
  }
  j <- 0:n
  s_n <- share(n, j)
  share0 <- r0 / (r0 + b0)
  mean_n <- sum(q * s_n)
  var_n <- sum(q * (s_n - share0)^2)
  list(share0 = share0, variance_bound = a / (4 * (r0 + b0)),
       chebyshev_bound = a / (4 * (r0 + b0) * t^2), variance_n = var_n, mean_n = mean_n,
       exit_n = sum(q[abs(s_n - share0) >= t]), law = q, n = n,
       theorem = "Research.P4.Envelope.chebyshev")
}

#' Ring mis-specification bound for the spillover estimand
#'
#' The Horvitz-Thompson spillover estimator is design-unbiased for the estimand written with the
#' d-ring exposure map. If the outcome also moves by \eqn{\gamma_i} when unit \eqn{i}'s
#' \eqn{(d+1)}-ring is treated, the d-ring estimand misses \eqn{(1/N)\sum_i \gamma_i \pi_i} in
#' expectation, with \eqn{\pi_i} the outer-ring exposure share of unit \eqn{i} under the design
#' (\code{Research.P3.Ring.expected_gap_eq}), and the miss is at most
#' \eqn{G \cdot \bar\pi} when every \eqn{|\gamma_i| \le G}
#' (\code{Research.P3.Ring.expected_gap_le}).
#'
#' @param outer_share Per-unit probability that the \eqn{(d+1)}-ring is exposed under the
#'   design, for instance the exposed-class column of
#'   \code{\link{morie_spillover_exposure_probs}} run with the larger ring.
#' @param gamma_max Bound \eqn{G} on the outer-ring effects.
#' @param gamma Optional per-unit outer-ring effects; with them the expected gap itself is
#'   returned beside the bound.
#' @return A list with \code{mean_share}, \code{bound} (\eqn{G \bar\pi}), \code{expected_gap}
#'   (\code{NA} without \code{gamma}) and \code{theorems}.
#' @examples
#' morie_spillover_ring_bound(c(0.1, 0.3, 0.2), gamma_max = 2)
#' @export
morie_spillover_ring_bound <- function(outer_share, gamma_max, gamma = NULL) {
  s <- as.numeric(outer_share)
  if (!length(s) || anyNA(s) || any(s < 0 | s > 1)) stop("`outer_share` must be probabilities in [0, 1]", call. = FALSE)
  if (!is.numeric(gamma_max) || length(gamma_max) != 1L || is.na(gamma_max) || gamma_max < 0) {
    stop("`gamma_max` must be a single non-negative bound", call. = FALSE)
  }
  gap <- NA_real_
  if (!is.null(gamma)) {
    g <- as.numeric(gamma)
    if (length(g) != length(s) || anyNA(g)) stop("`gamma` must have one finite value per unit", call. = FALSE)
    if (any(abs(g) > gamma_max + 1e-12)) stop("`gamma` exceeds `gamma_max` somewhere", call. = FALSE)
    gap <- mean(g * s)
  }
  list(mean_share = mean(s), bound = gamma_max * mean(s), expected_gap = gap,
       theorems = c("Research.P3.Ring.expected_gap_eq", "Research.P3.Ring.expected_gap_le"))
}

#' Rank-persistence null: the overlap of two top-k sets by chance
#'
#' Under the null that this year's top-\eqn{k} places are a uniformly random \eqn{k}-subset of
#' the \eqn{n} places, independent of last year's, the number of places in both sets is
#' hypergeometric: \eqn{P(j) = C(k, j) C(n-k, k-j) / C(n, k)}
#' (\code{Research.P7.Persistence.card_overlapSets}, \code{overlap_prob}), the law sums to one
#' (\code{overlap_prob_sum}) and its mean is \eqn{k^2/n}.
#'
#' @param n Number of places.
#' @param k Size of the top set.
#' @param observed Optional observed overlap; its upper tail probability under the null is
#'   returned.
#' @return A list with \code{law} (a data frame of \code{overlap} and \code{probability}),
#'   \code{mean} (\eqn{k^2/n}), \code{p_value} (\eqn{P(\mathrm{overlap} \ge \mathrm{observed})},
#'   \code{NA} without \code{observed}) and \code{theorems}.
#' @examples
#' morie_rank_persistence_null(158, 16, observed = 9)$p_value
#' @export
morie_rank_persistence_null <- function(n, k, observed = NULL) {
  n <- as.integer(n)
  k <- as.integer(k)
  if (length(n) != 1L || length(k) != 1L || is.na(n) || is.na(k) || k < 0L || k > n) {
    stop("`k` must be between 0 and `n`", call. = FALSE)
  }
  j <- 0:k
  prob <- choose(k, j) * choose(n - k, k - j) / choose(n, k)
  p <- NA_real_
  if (!is.null(observed)) {
    observed <- as.integer(observed)
    if (length(observed) != 1L || is.na(observed) || observed < 0L || observed > k) {
      stop("`observed` must be an overlap between 0 and `k`", call. = FALSE)
    }
    p <- sum(prob[j >= observed])
  }
  list(law = data.frame(overlap = j, probability = prob), mean = k^2 / n, p_value = p,
       theorems = c("Research.P7.Persistence.card_overlapSets", "Research.P7.Persistence.overlap_prob",
                    "Research.P7.Persistence.overlap_prob_sum"))
}

#' Hot-spot threshold of the Short-Brantingham reaction-diffusion model
#'
#' The spatially uniform Short-Brantingham system
#' \eqn{B' = -\omega B + \epsilon D \rho (A_0 + B)}, \eqn{\rho' = -\rho (A_0 + B) + \gamma}
#' has exactly one homogeneous steady state, \eqn{\bar B = \epsilon D \gamma / \omega},
#' \eqn{\bar\rho = \gamma / \bar A} with \eqn{\bar A = A_0 + \bar B}
#' (\code{Research.P4.Hotspot.steady_iff}). Linearising at wavenumber-squared \eqn{k} gives a
#' \eqn{2 \times 2} matrix whose determinant is \eqn{P k^2 - Q k + R} with
#' \eqn{P = \eta D^2 / z}, \eqn{Q = D (3 \epsilon D \bar\rho - \omega - \eta \bar A / z)} and
#' \eqn{R = \omega \bar A > 0} (\code{det_eq}, \code{P_eq}, \code{Q_eq}, \code{R_eq}); its trace is
#' negative at every \eqn{k \ge 0} (\code{tr_neg}), so a mode grows iff the determinant is
#' negative (\code{unstable_iff}). Hence the threshold: with
#' \eqn{3 \epsilon D \bar\rho \le \omega + \eta \bar A / z} the uniform state is stable at every
#' wavelength (\code{no_hotspots_below_threshold}; in particular without repeat reinforcement,
#' \code{no_hotspots_without_reinforcement}), and with \eqn{Q > 0} and \eqn{Q^2 > 4 P R} the
#' critical mode \eqn{k^* = Q / (2P)} grows (\code{hotspots_above_threshold}).
#'
#' @param A0 Baseline attractiveness (positive).
#' @param omega Decay rate of the dynamic attractiveness (positive).
#' @param eps Repeat-victimisation reinforcement (non-negative).
#' @param D Diffusion rate of the criminal density (positive).
#' @param gamma Generation rate of criminals (positive).
#' @param eta Relative diffusion of attractiveness (non-negative).
#' @param z Lattice coordination number (positive; 4 on a square lattice, 1 in the continuum form).
#' @param k Optional wavenumbers-squared at which the trace, determinant and growth rate are tabulated.
#' @return A list with \code{steady} (\eqn{\bar B}, \eqn{\bar A}, \eqn{\bar\rho}), the coefficients
#'   \code{P}, \code{Q}, \code{R}, \code{threshold} (the two sides of
#'   \eqn{3 \epsilon D \bar\rho \lessgtr \omega + \eta \bar A / z}), \code{unstable}, the critical
#'   wavenumber-squared \code{k_crit} with \code{det_crit} and \code{growth_crit}, an optional
#'   \code{dispersion} table and \code{theorems}.
#' @examples
#' morie_hotspot_threshold(A0 = 1, omega = 1, eps = 2, D = 1, gamma = 1, eta = 0.01)$unstable
#' morie_hotspot_threshold(A0 = 1, omega = 1, eps = 0, D = 1, gamma = 1, eta = 0.01)$unstable
#' @export
morie_hotspot_threshold <- function(A0, omega, eps, D, gamma, eta, z = 1, k = NULL) {
  one <- function(x, nm, lower, strict = TRUE) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || (if (strict) x <= lower else x < lower)) {
      stop(sprintf("`%s` must be a single finite number %s %s", nm, if (strict) "above" else "at least", lower),
           call. = FALSE)
    }
    as.numeric(x)
  }
  A0 <- one(A0, "A0", 0)
  omega <- one(omega, "omega", 0)
  eps <- one(eps, "eps", 0, strict = FALSE)
  D <- one(D, "D", 0)
  gamma <- one(gamma, "gamma", 0)
  eta <- one(eta, "eta", 0, strict = FALSE)
  z <- one(z, "z", 0)
  Bbar <- eps * D * gamma / omega
  Abar <- A0 + Bbar
  rhobar <- gamma / Abar
  a <- -omega + eps * D * rhobar
  b <- eps * D * Abar
  c0 <- -rhobar
  c1 <- 2 * D * rhobar / Abar
  d <- -Abar
  d1 <- eta * D / z
  d2 <- D
  P <- d1 * d2
  Q <- a * d2 + d * d1 + b * c1
  R <- a * d - b * c0
  tr_k <- function(k) (a - d1 * k) + (d - d2 * k)
  det_k <- function(k) (a - d1 * k) * (d - d2 * k) - b * (c0 + c1 * k)
  growth_k <- function(k) {
    t <- tr_k(k)
    dd <- det_k(k)
    (t + sqrt(pmax(t^2 - 4 * dd, 0))) / 2   # the larger real part of the two eigenvalues
  }
  lhs <- 3 * eps * D * rhobar
  rhs <- omega + eta * Abar / z
  if (P > 0) {
    unstable <- Q > 0 && Q^2 > 4 * P * R
    k_crit <- if (Q > 0) Q / (2 * P) else NA_real_
  } else {
    # no attractiveness diffusion: the determinant is linear in k, negative past R / Q when Q > 0
    unstable <- Q > 0
    k_crit <- if (Q > 0) 2 * R / Q else NA_real_
  }
  det_crit <- if (is.na(k_crit)) NA_real_ else det_k(k_crit)
  growth_crit <- if (is.na(k_crit)) NA_real_ else growth_k(k_crit)
  dispersion <- NULL
  if (!is.null(k)) {
    if (!is.numeric(k) || anyNA(k) || any(k < 0)) stop("`k` must be non-negative wavenumbers-squared", call. = FALSE)
    dispersion <- data.frame(k = k, trace = tr_k(k), det = det_k(k), growth = growth_k(k))
  }
  list(steady = c(B = Bbar, A = Abar, rho = rhobar), P = P, Q = Q, R = R,
       threshold = c(reinforcement = lhs, decay_plus_diffusion = rhs), unstable = unstable,
       k_crit = k_crit, det_crit = det_crit, growth_crit = growth_crit, dispersion = dispersion,
       theorems = c("Research.P4.Hotspot.steady_iff", "Research.P4.Hotspot.unstable_iff",
                    "Research.P4.Hotspot.no_hotspots_below_threshold",
                    "Research.P4.Hotspot.hotspots_above_threshold"))
}

#' The dark figure from a survey that covers only some offences
#'
#' A victimisation survey covers an offence set A; the police record covers A and the rest, B.
#' With \eqn{V = V_A + V_B} true offences and \eqn{R = R_A + R_B} recorded ones, the survey
#' identifies \eqn{V_A} and so the recording rate on A, \eqn{r_A = R_A / V_A}
#' (\code{Research.P1.Comparable.rate_on_A}); \eqn{V_B} is never observed. The overall rate is
#' at most \eqn{R / (V_A + R_B)} (\code{total_rate_le}) and has no positive lower bound without
#' an assumption on B (\code{total_rate_unbounded_below}). Under \eqn{r_B \ge \lambda r_A}
#' (the non-comparable offences are recorded at least a fraction \eqn{\lambda} as well as the
#' comparable ones) the total count lies in \eqn{[V_A + R_B, V_A + R_B / (\lambda r_A)]}
#' (\code{total_interval}) and the dark figure in
#' \eqn{[V_A - R_A, V_A - R_A + R_B (1 / (\lambda r_A) - 1)]} (\code{dark_interval}), an
#' interval of width \eqn{R_B (1 - \lambda r_A) / (\lambda r_A)} (\code{dark_figure_width}).
#'
#' @param VA Survey estimate of the true offences in the comparable set.
#' @param RA Recorded offences in the comparable set.
#' @param RB Recorded offences outside it.
#' @param lambda The assumed floor on the non-comparable recording rate relative to the
#'   comparable one, in (0, 1]; 1 says B is recorded at least as well as A.
#' @return A list with \code{rate_A}, \code{rate_upper} (the total rate's upper bound),
#'   \code{total} and \code{dark_figure} (each a two-element interval), \code{width} and
#'   \code{theorems}.
#' @examples
#' morie_dark_figure_comparable(VA = 1000, RA = 400, RB = 300, lambda = 0.5)
#' @export
morie_dark_figure_comparable <- function(VA, RA, RB, lambda = 1) {
  num1 <- function(x, nm, lower = 0) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < lower) {
      stop(sprintf("`%s` must be a single finite number at least %s", nm, lower), call. = FALSE)
    }
    as.numeric(x)
  }
  VA <- num1(VA, "VA")
  RA <- num1(RA, "RA")
  RB <- num1(RB, "RB")
  lambda <- num1(lambda, "lambda")
  if (VA <= 0) stop("`VA` must be positive: the survey must see some comparable offences", call. = FALSE)
  if (RA > VA) stop("`RA` exceeds `VA`: a record is an offence, so the recorded count cannot exceed the true one",
                    call. = FALSE)
  if (lambda <= 0 || lambda > 1) stop("`lambda` must be in (0, 1]", call. = FALSE)
  rate_A <- RA / VA
  if (rate_A <= 0) stop("`RA` must be positive for the bound: with nothing recorded on A, lambda r_A = 0", call. = FALSE)
  k <- lambda * rate_A
  total <- c(lower = VA + RB, upper = VA + RB / k)
  dark <- c(lower = VA - RA, upper = VA - RA + RB * (1 / k - 1))
  list(rate_A = rate_A, rate_upper = (RA + RB) / (VA + RB), total = total, dark_figure = dark,
       width = RB * (1 - k) / k,
       theorems = c("Research.P1.Comparable.rate_on_A", "Research.P1.Comparable.total_rate_le",
                    "Research.P1.Comparable.total_interval", "Research.P1.Comparable.dark_interval",
                    "Research.P1.Comparable.dark_figure_width"))
}
