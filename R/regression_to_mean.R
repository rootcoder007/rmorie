# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P19: regression to the mean at selected hot spots (research/lean/P19Regression.lean;
# Galton 1886; Campbell & Stanley 1963; Sherman & Weisburd 1995).
#
#   Research.P19.exchange_mass / exchange_cross   under exchangeable periods the selected mass and the cross term reindex
#   Research.P19.indicator_bound                  x1 (1{x1>c} - 1{x2>c}) >= c (1{x1>c} - 1{x2>c})
#   Research.P19.selected_change_nonpos           sum_{x1 > c} w (x2 - x1) <= 0
#   Research.P19.low_selected_change_nonneg       the mirror for places selected for being low

#' Regression to the mean at selected hot spots
#'
#' Places selected because their first-period count exceeds \code{threshold}
#' show, under the null of exchangeable periods, a non-positive mean change in
#' the second period (\code{Research.P19.selected_change_nonpos}); places
#' selected for being low show a non-negative one
#' (\code{low_selected_change_nonneg}). The function reports the observed
#' change on the selected places, the mirror change (selection on the second
#' period, read backwards), and the symmetrised change, which is the same
#' statistic on the exchangeable population made of the data and its
#' period-swapped copy, and is therefore non-positive by the theorem. The
#' difference between the observed and the symmetrised change is the part of
#' the drop that selection symmetry does not deliver; a treatment effect is
#' identified only against a control group selected the same way.
#' @param x1,x2 Counts per place in the first and second period.
#' @param threshold Selection cut: places with \code{x1 > threshold} are the hot spots.
#' @param weights Optional non-negative weights.
#' @return A list with \code{n_selected}, \code{selected_change} (mean of
#'   \eqn{x_2 - x_1} on the selected places), \code{mirror_change},
#'   \code{symmetrised_change} (always \eqn{\le 0}), \code{excess_over_symmetry},
#'   \code{low_selected_change} (places with \code{x1 < threshold}, always
#'   \eqn{\ge 0} after symmetrisation: \code{low_symmetrised_change}) and
#'   \code{theorems}.
#' @examples
#' set.seed(5)
#' mu <- rgamma(300, 2, 0.5)                   # stable place means
#' x1 <- rpois(300, mu); x2 <- rpois(300, mu)  # no treatment, no trend
#' morie_regression_to_mean(x1, x2, threshold = quantile(x1, 0.9))[c("selected_change", "symmetrised_change")]
#' @export
morie_regression_to_mean <- function(x1, x2, threshold, weights = NULL) {
  n <- length(x1)
  if (length(x2) != n) stop("x1 and x2 must have equal length", call. = FALSE)
  if (anyNA(x1) || anyNA(x2)) stop("counts must not contain NA", call. = FALSE)
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold)) stop("threshold must be a single number", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0)) stop("weights must be non-negative", call. = FALSE)
  w <- weights
  sel1 <- x1 > threshold
  sel2 <- x2 > threshold
  low1 <- x1 < threshold
  low2 <- x2 < threshold
  if (!any(sel1)) stop("no place exceeds the threshold", call. = FALSE)
  sel_change <- sum(w[sel1] * (x2 - x1)[sel1]) / sum(w[sel1])
  mirror <- if (any(sel2)) sum(w[sel2] * (x1 - x2)[sel2]) / sum(w[sel2]) else NA_real_
  # the symmetrised population: the data plus its period-swapped copy (exchangeable by construction)
  sym_num <- sum(w[sel1] * (x2 - x1)[sel1]) + sum(w[sel2] * (x1 - x2)[sel2])
  sym_den <- sum(w[sel1]) + sum(w[sel2])
  sym <- sym_num / sym_den
  low_change <- if (any(low1)) sum(w[low1] * (x2 - x1)[low1]) / sum(w[low1]) else NA_real_
  low_sym <- if (any(low1) || any(low2)) (sum(w[low1] * (x2 - x1)[low1]) + sum(w[low2] * (x1 - x2)[low2])) / (sum(w[low1]) + sum(w[low2])) else NA_real_
  list(n_selected = sum(sel1), selected_change = sel_change, mirror_change = mirror,
       symmetrised_change = sym, excess_over_symmetry = sel_change - sym,
       low_selected_change = low_change, low_symmetrised_change = low_sym,
       theorems = c("Research.P19.exchange_mass", "Research.P19.exchange_cross", "Research.P19.indicator_bound",
                    "Research.P19.selected_change_nonpos", "Research.P19.low_selected_change_nonneg"))
}

#   Research.P19Shrinkage.loss_eq          loss(B) = (1-B)^2 S_e + B^2 S_theta under the noise law
#   Research.P19Shrinkage.loss_min         minimised at B* = S_e/(S_e + S_theta)
#   Research.P19Shrinkage.loss_bstar_eq    loss(B*) = S_e S_theta/(S_e + S_theta)
#   Research.P19Shrinkage.loss_bstar_le_raw  loss(B*) <= S_e = loss(0)
#   Research.P19Shrinkage.bstar_mem        0 <= B* <= 1
#   Research.P19Shrinkage.predicted_fall   y - shrunk = B (y - ybar)

#' Empirical-Bayes shrinkage of hot-spot counts: the expected size of the fall
#'
#' First-period counts \eqn{y = \theta + e} at places with true rates
#' \eqn{\theta} and noise of variance \code{noise_variance}. The shrinkage
#' estimator \eqn{(1-B)y + B\bar y} has, under a noise law with weighted
#' mean zero and zero weighted correlation with \eqn{\theta}, the loss
#' \eqn{(1-B)^2 S_e + B^2 S_\theta} (\code{Research.P19Shrinkage.loss_eq}),
#' minimised at \eqn{B^* = S_e/(S_e + S_\theta)} (\code{loss_min},
#' \code{bstar_mem}) where it never exceeds the raw loss
#' (\code{loss_bstar_le_raw}). The predicted fall of a place is
#' \eqn{B(y - \bar y)} (\code{predicted_fall}): the size the P19 sign theorem
#' left open. Here \eqn{S_\theta} is estimated as the excess of the weighted
#' scatter of \eqn{y} over the noise mass.
#' @param y First-period counts.
#' @param noise_variance Noise variance \eqn{\sigma^2} (for Poisson counts, the mean count).
#' @param weights Optional non-negative weights.
#' @param B Optional shrinkage factor in \eqn{[0, 1]}; the default is \eqn{B^*}.
#' @return A list with \code{mean}, \code{total_variance}, \code{noise_variance},
#'   \code{signal_variance} (\eqn{\max(\text{total} - \sigma^2, 0)}), \code{B},
#'   \code{shrunk}, \code{predicted_fall} and \code{theorems}.
#' @examples
#' y <- c(40, 12, 9, 25, 7, 31, 5, 18)
#' s <- morie_hotspot_shrinkage(y, noise_variance = mean(y))
#' round(c(B = s$B, fall_top = s$predicted_fall[1], shrunk_top = s$shrunk[1]), 6)
#' @export
morie_hotspot_shrinkage <- function(y, noise_variance, weights = NULL, B = NULL) {
  if (!is.numeric(y) || anyNA(y) || length(y) < 2L) stop("y must be at least two numbers without NA", call. = FALSE)
  if (length(noise_variance) != 1L || is.na(noise_variance) || noise_variance < 0) stop("noise_variance must be a non-negative number", call. = FALSE)
  w <- if (is.null(weights)) rep(1, length(y)) else weights
  if (length(w) != length(y) || anyNA(w) || any(w < 0) || sum(w) <= 0) stop("weights must be non-negative with positive total", call. = FALSE)
  W <- sum(w)
  ybar <- sum(w * y) / W
  total <- sum(w * (y - ybar)^2) / W
  signal <- max(total - noise_variance, 0)
  Se <- noise_variance * W
  St <- signal * W
  Bstar <- if (Se + St > 0) Se / (Se + St) else 0
  if (is.null(B)) B <- Bstar
  if (length(B) != 1L || is.na(B) || B < 0 || B > 1) stop("B must lie in [0, 1]", call. = FALSE)
  list(
    mean = ybar, total_variance = total, noise_variance = noise_variance, signal_variance = signal,
    B = B, B_star = Bstar,
    shrunk = (1 - B) * y + B * ybar,
    predicted_fall = B * (y - ybar),
    theorems = c("Research.P19Shrinkage.loss_min", "Research.P19Shrinkage.bstar_mem", "Research.P19Shrinkage.predicted_fall")
  )
}

#' Loss of the shrinkage estimator against a known truth
#'
#' For known true rates \eqn{\theta} and noise \eqn{e}, the weighted squared
#' error of \eqn{(1-B)(\theta+e) + B\bar y} against \eqn{\theta}, next to
#' the closed form \eqn{(1-B)^2 S_e + B^2 S_\theta}
#' (\code{Research.P19Shrinkage.loss_eq}), which holds exactly when the
#' noise law \eqn{\sum w e = 0}, \eqn{\sum w \theta e = 0} does; the
#' minimiser \eqn{B^*} and its loss (\code{loss_min}, \code{loss_bstar_eq}).
#' @param theta True rates.
#' @param noise Noise, same length.
#' @param weights Optional non-negative weights.
#' @param B Shrinkage factor to evaluate.
#' @return A list with \code{loss} (direct), \code{closed_form}, \code{noise_law}
#'   (whether both sums vanish to \eqn{10^{-10}}), \code{Se}, \code{Stheta},
#'   \code{B_star}, \code{loss_star}, \code{loss_raw} and \code{theorems}.
#' @examples
#' theta <- c(10, 20, 30, 40)
#' e <- c(3, -3, -3, 3)       # mean zero and uncorrelated with theta
#' l <- morie_shrinkage_loss(theta, e, B = 0.5)
#' c(loss = l$loss, closed = l$closed_form, B_star = l$B_star, loss_star = l$loss_star, raw = l$loss_raw)
#' @export
morie_shrinkage_loss <- function(theta, noise, weights = NULL, B) {
  if (!is.numeric(theta) || !is.numeric(noise) || length(theta) != length(noise) || anyNA(theta) || anyNA(noise)) stop("theta and noise must be numeric of equal length without NA", call. = FALSE)
  if (length(B) != 1L || is.na(B)) stop("B must be a single number", call. = FALSE)
  w <- if (is.null(weights)) rep(1, length(theta)) else weights
  if (length(w) != length(theta) || anyNA(w) || any(w < 0) || sum(w) <= 0) stop("weights must be non-negative with positive total", call. = FALSE)
  W <- sum(w)
  y <- theta + noise
  ybar <- sum(w * y) / W
  tbar <- sum(w * theta) / W
  Se <- sum(w * noise^2)
  St <- sum(w * (theta - tbar)^2)
  shrunk <- (1 - B) * y + B * ybar
  law <- abs(sum(w * noise)) <= 1e-10 * max(1, W) && abs(sum(w * theta * noise)) <= 1e-10 * max(1, sum(abs(w * theta)))
  Bstar <- if (Se + St > 0) Se / (Se + St) else 0
  list(
    loss = sum(w * (shrunk - theta)^2),
    closed_form = (1 - B)^2 * Se + B^2 * St,
    noise_law = law,
    Se = Se, Stheta = St,
    B_star = Bstar,
    loss_star = if (Se + St > 0) Se * St / (Se + St) else 0,
    loss_raw = Se,
    theorems = c("Research.P19Shrinkage.loss_eq", "Research.P19Shrinkage.loss_min",
                 "Research.P19Shrinkage.loss_bstar_eq", "Research.P19Shrinkage.loss_bstar_le_raw")
  )
}
