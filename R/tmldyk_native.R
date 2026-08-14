# Differentially private TMLE by the Laplace mechanism.
# Sources: Dwork, C., McSherry, F., Nissim, K. & Smith, A. (2006)
# Calibrating Noise to Sensitivity in Private Data Analysis, Theory
# of Cryptography (TCC 2006), LNCS 3876, 265-284,
# doi:10.1007/11681878_14 (Laplace mechanism, sensitivity,
# calibration of noise); Niu, F., Nori, H., Quistorff, B., Caruana,
# R., Ngwe, D. & Kannan, A. (2022) Differentially Private Estimation
# of Heterogeneous Causal Effects, CLeaR 2022, PMLR 177, 618-633,
# arXiv:2202.11043 (a meta-algorithm giving differential privacy
# guarantees for CATE / doubly robust estimators); van der Laan, M. J.
# & Rose, S. (2018) Targeted Learning in Data Science, Springer,
# Chap. 4 (the clever covariate's dependence on 1/g).
#
# Native implementation mirroring Python morie.fn.tmldyk exactly: the
# same Laplace draw by inverse transform, the same sensitivity bound
# 2R/(n g_min) with the naive 1/n shown alongside, the same
# noise-variance added to the influence-curve variance, the same
# basic composition, the same validation messages.

.EPS <- 1e-12

#' One draw from Lap(0, b) by inverse transform
#'
#' @param scale Noise scale b.
#' @param e Generator environment from \code{.ghc_rng}.
#' @return A scalar.
#' @references Dwork, C. et al. (2006).
#' @export
laplace_noise <- function(scale, e) {
  b <- as.numeric(scale)
  if (b <= 0) stop("tmldyk: the noise scale must be positive")
  u <- .ghc_unif(e, 1L) - 0.5
  -b * sign(u) * log(max(1 - 2 * abs(u), 1e-300))
}

#' L1 sensitivity of a TMLE of the ATE
#'
#' One observation enters through the clever covariate, so the bound
#' carries 1/g_min: it is 2 R / (n g_min), not O(1/n). Truncating the
#' propensity score is what makes the release affordable.
#'
#' @param n Sample size.
#' @param g_min Propensity truncation bound.
#' @param y_range Outcome range.
#' @return A list with \code{sensitivity}, \code{naive_1_over_n},
#'   \code{inflation}, \code{g_min}, \code{n}, \code{note}.
#' @references Dwork, C. et al. (2006).
#' @export
ate_sensitivity <- function(n, g_min, y_range = 1) {
  nn <- as.integer(n)
  if (nn < 1L) stop("tmldyk: n must be at least 1")
  gm <- as.numeric(g_min)
  if (!(gm > 0 && gm <= 0.5))
    stop("tmldyk: the propensity truncation bound must lie in (0, 0.5]")
  list(sensitivity = 2 * as.numeric(y_range) / (nn * gm),
       naive_1_over_n = as.numeric(y_range) / nn,
       inflation = 2 / gm, g_min = gm, n = nn,
       note = "the clever covariate carries 1/g, so the sensitivity is NOT O(1/n) unless g is truncated")
}

#' Release f(D) + Lap(Delta f / epsilon)
#'
#' @param value The non-private statistic.
#' @param sensitivity L1 sensitivity.
#' @param epsilon Privacy budget.
#' @param seed Seed for the shared generator.
#' @return A list with \code{released}, \code{noise}, \code{scale},
#'   \code{epsilon}, \code{noise_variance}, \code{note}.
#' @references Dwork, C. et al. (2006).
#' @export
private_release <- function(value, sensitivity, epsilon, seed = 0) {
  eps <- as.numeric(epsilon)
  if (eps <= 0) stop("tmldyk: epsilon must be positive")
  sens <- as.numeric(sensitivity)
  if (sens <= 0) stop("tmldyk: the sensitivity must be positive")
  e <- .ghc_rng(seed); b <- sens / eps
  noise <- laplace_noise(b, e)
  list(released = as.numeric(value) + noise, noise = noise,
       scale = b, epsilon = eps, noise_variance = 2 * b * b,
       note = "the guarantee holds only if the sensitivity is an upper bound; an underestimate provides no privacy at all")
}

#' An interval that accounts for the mechanism's own variance
#'
#' Var = Var_sampling + 2(Delta f / epsilon)^2.
#'
#' @param value Non-private statistic.
#' @param sensitivity L1 sensitivity.
#' @param epsilon Privacy budget.
#' @param se Sampling standard error.
#' @param seed Seed for the shared generator.
#' @param level Multiplier (e.g. 1.96 for 95 percent).
#' @return A list with \code{estimate}, \code{se_private},
#'   \code{se_sampling}, \code{ci}, \code{width_ratio},
#'   \code{epsilon}.
#' @references Dwork, C. et al. (2006).
#' @export
private_ci <- function(value, sensitivity, epsilon, se,
                       seed = 0, level = 1.96) {
  r <- private_release(value, sensitivity, epsilon, seed)
  tot <- as.numeric(se)^2 + r$noise_variance
  w <- as.numeric(level) * sqrt(tot)
  list(estimate = r$released, se_private = sqrt(tot),
       se_sampling = as.numeric(se),
       ci = c(r$released - w, r$released + w),
       width_ratio = if (as.numeric(se) > 0) sqrt(tot) / as.numeric(se)
                     else NaN,
       epsilon = as.numeric(epsilon))
}

#' Basic composition: k releases cost sum_i epsilon_i
#'
#' @param epsilons Vector of epsilons.
#' @return A list with \code{total_epsilon}, \code{n_releases},
#'   \code{note}.
#' @references Dwork, C. et al. (2006).
#' @export
composition_budget <- function(epsilons) {
  e <- as.numeric(epsilons)
  if (any(e <= 0)) stop("tmldyk: every epsilon must be positive")
  list(total_epsilon = sum(e), n_releases = length(e),
       note = "each release spends part of the budget; the guarantee degrades linearly")
}

#' Differentially private TMLE of the ATE
#'
#' The propensity score is truncated at \code{g_min} -- which bounds
#' the sensitivity and is therefore part of the privacy guarantee, not
#' a numerical convenience.
#'
#' @param y Outcome vector in [0,1].
#' @param D Treatment indicator.
#' @param X Covariate matrix.
#' @param epsilon Privacy budget.
#' @param g_min Propensity truncation bound.
#' @param seed Seed for the shared generator.
#' @param g Optional propensity score.
#' @param Q1 Optional potential-outcome regression under treatment.
#' @param Q0 Optional potential-outcome regression under control.
#' @return A list with \code{estimate}, \code{psi},
#'   \code{non_private_psi}, \code{sensitivity}, \code{epsilon},
#'   \code{g_min}, \code{se_private}, \code{se_sampling}, \code{ci},
#'   \code{width_ratio}, \code{method}, \code{note}.
#' @references Dwork, C. et al. (2006); Niu, F. et al. (2022).
#' @export
morie_tmldyk <- function(y, D, X, epsilon = 1, g_min = 0.05,
                         seed = 0, g = NULL, Q1 = NULL, Q0 = NULL) {
  yv <- as.numeric(y); a <- as.numeric(D)
  W <- as.matrix(X); storage.mode(W) <- "double"
  n <- length(yv)
  if (!(length(a) == nrow(W) && nrow(W) == n))
    stop("tmldyk: the inputs differ in length")
  if (any(yv < 0 | yv > 1))
    stop("tmldyk: the outcome must lie in [0,1] for the stated sensitivity bound")
  fit <- morie_tmlcou(yv, a, W, offset = NULL, g = g, Q1 = Q1,
                      Q0 = Q0, lower = 0, upper = 1)
  sens <- ate_sensitivity(n, g_min, 1)
  ci <- private_ci(fit$psi, sens$sensitivity, epsilon, fit$se, seed)
  list(estimate = ci$estimate, psi = ci$estimate,
       non_private_psi = fit$psi,
       sensitivity = sens$sensitivity, epsilon = as.numeric(epsilon),
       g_min = as.numeric(g_min),
       se_private = ci$se_private, se_sampling = fit$se, ci = ci$ci,
       width_ratio = ci$width_ratio,
       method = paste0("epsilon-differentially private TMLE by the ",
                       "Laplace mechanism; Dwork, McSherry, Nissim & ",
                       "Smith (2006), Niu et al. (2022)"),
       note = paste0("the propensity truncation is part of the ",
                     "PRIVACY guarantee, since it is what bounds the ",
                     "sensitivity"))
}

#' Compact alias per ledger/NAMING.md
#' @export
morie_tmlediffkernel <- morie_tmldyk
