# Inverse-variance weighted Mendelian randomization from summary data.
# Sources: Burgess, S. & Bowden, J. (2015) "Integrating summarized
# data from multiple genetic variants in Mendelian randomization:
# bias and coverage properties of inverse-variance weighted
# methods", arXiv:1512.04486 [stat.AP]. Sec. 2.1 for the ratio
# estimate, the delta-method variance with first- and second-order
# terms (2)-(5) and the IVW estimate (6)-(8); Sec. 2.2 for the
# equivalence to two-stage least squares and to weighted regression
# through the origin; Sec. 2.3 and 2.4 for the fixed-effect,
# additive and multiplicative random-effects models, the identity of
# the fixed and multiplicative point estimates, and the instruction
# to floor the multiplicative residual scale at one under
# under-dispersion; and the abstract for the recommendation of
# random-effects models and of second-order weights under sample
# overlap. DerSimonian, R. & Laird, N. (1986) "Meta-analysis in
# clinical trials", Controlled Clinical Trials 7(3), 177-188, for
# the method-of-moments heterogeneity estimator used by the additive
# model. Lawlor, D. A. et al. (2008) "Mendelian randomization:
# using genes as instruments for making causal inferences in
# epidemiology", Statistics in Medicine 27(8), 1133-1163, for the
# ratio estimate itself.
#
# Native implementation mirroring Python morie.fn.mtr2sx exactly:
# the same ratio estimates, the same delta-method variances with
# the first- and second-order terms, the same Cochran Q and
# DerSimonian-Laird tau^2, the same three meta-analysis models
# (fixed, multiplicative with the multiplicative scale floored at
# 1, additive DerSimonian-Laird) and the same payload keys.
# The function is fully deterministic: it draws no random numbers,
# so the shared RNG helpers are not consumed.

# Inverse-variance weighted Mendelian randomization
#
# Combines per-variant ratio estimates of a causal effect into a
# single inverse-variance weighted estimate under one of three
# meta-analysis models, using either first- or second-order
# delta-method weights on the ratio variance (Burgess & Bowden
# 2015).
#
# @param beta_x Numeric vector of variant--exposure associations.
# @param se_x Numeric vector of standard errors of \code{beta_x}.
# @param beta_y Numeric vector of variant--outcome associations.
# @param se_y Numeric vector of standard errors of \code{beta_y}.
# @param model One of \code{"multiplicative"} (default),
#   \code{"fixed"} or \code{"additive"}.
# @param weights One of \code{"first_order"} (default) or
#   \code{"second_order"}.
# @param theta Correlation between the two association estimates,
#   used only when \code{weights = "second_order"}; zero in a
#   genuine two-sample design.
# @return A named list with elements \code{estimate}, \code{se},
#   \code{z}, \code{p_value}, \code{ci} (length-2 numeric),
#   \code{ratio_estimates}, \code{variances}, \code{weights_used},
#   \code{model}, \code{phi_multiplicative}, \code{tau2},
#   \code{Q}, \code{df}, \code{I2}, \code{se_fixed},
#   \code{regression_estimate}, \code{regression_se_fixed},
#   \code{n_variants} and \code{method}.
# @references Burgess, S. & Bowden, J. (2015). Integrating
#   summarized data from multiple genetic variants in Mendelian
#   randomization. arXiv:1512.04486 [stat.AP].
# @export

# Base R has no erf/erfc; both are pnorm in disguise. Defined here so
# the arm stays base-R only, as the package requires.
#' Base R has no erf/erfc; both are pnorm in disguise. Defined here so
#'
#' the arm stays base-R only, as the package requires.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mtr2sx_erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
#' .mtr2sx_erfc
#'
#' A step of the mtr2sx_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mtr2sx_erfc <- function(x) 2 * pnorm(-x * sqrt(2))

#' Inverse-variance weighted Mendelian randomization
#'
#' Combines per-variant ratio estimates of a causal effect into a
#' single inverse-variance weighted estimate under one of three
#' meta-analysis models, using either first- or second-order
#' delta-method weights on the ratio variance (Burgess & Bowden
#' 2015).
#'
#' @param beta_x Numeric vector of variant--exposure associations.
#' @param se_x Numeric vector of standard errors of \code{beta_x}.
#' @param beta_y Numeric vector of variant--outcome associations.
#' @param se_y Numeric vector of standard errors of \code{beta_y}.
#' @param model One of \code{"multiplicative"} (default),
#'   \code{"fixed"} or \code{"additive"}.
#' @param weights One of \code{"first_order"} (default) or
#'   \code{"second_order"}.
#' @param theta Correlation between the two association estimates,
#'   used only when \code{weights = "second_order"}; zero in a
#'   genuine two-sample design.
#' @return A named list with elements \code{estimate}, \code{se},
#'   \code{z}, \code{p_value}, \code{ci} (length-2 numeric),
#'   \code{ratio_estimates}, \code{variances}, \code{weights_used},
#'   \code{model}, \code{phi_multiplicative}, \code{tau2},
#'   \code{Q}, \code{df}, \code{I2}, \code{se_fixed},
#'   \code{regression_estimate}, \code{regression_se_fixed},
#'   \code{n_variants} and \code{method}.
#' @references Burgess, S. & Bowden, J. (2015). Integrating
#'   summarized data from multiple genetic variants in Mendelian
#'   randomization. arXiv:1512.04486 [stat.AP].
#' @export
morie_mtr2sx <- function(beta_x, se_x, beta_y, se_y,
                        model = "multiplicative",
                        weights = "first_order", theta = 0.0) {
  MODELS <- c("multiplicative", "fixed", "additive")
  WEIGHTS <- c("first_order", "second_order")
  if (!(model %in% MODELS))
    stop(sprintf("mtr2sx: model must be one of %s, got %r",
                 paste(MODELS, collapse = ", "), model))
  if (!(weights %in% WEIGHTS))
    stop(sprintf("mtr2sx: weights must be one of %s, got %r",
                 paste(WEIGHTS, collapse = ", "), weights))
  bx <- as.numeric(beta_x)
  by <- as.numeric(beta_y)
  sx <- as.numeric(se_x)
  sy <- as.numeric(se_y)
  L <- length(bx)
  if (!(L == length(sx) && L == length(by) && L == length(sy)))
    stop("mtr2sx: the four summary vectors must have the same length")
  if (L == 0L)
    stop("mtr2sx: no genetic variants supplied")
  if (any(bx == 0))
    stop("mtr2sx: a variant with zero association to the risk factor has an undefined ratio estimate")
  if (any(sy <= 0) || any(sx < 0))
    stop("mtr2sx: standard errors must be positive")
  theta <- as.numeric(theta)

  # --- per-variant ratio estimates -----------------------------------
  ratios <- by / bx

  # --- per-variant delta-method variances ----------------------------
  var <- sy * sy / (bx * bx)
  if (weights == "second_order") {
    v2 <- by * by * sx * sx / (bx ^ 4) -
      2 * theta * by * sy * sx / (bx ^ 3)
    if (any(v2 <= 0))
      stop(sprintf("mtr2sx: the second-order variance for variant %d is non-positive; check theta",
                   which(v2 <= 0)[1L]))
    var <- var + v2
  }

  # --- Cochran Q and DerSimonian-Laird tau^2 ------------------------
  w <- 1 / var
  sw <- sum(w)
  se_fixed <- sqrt(1 / sw)
  est <- sum(ratios * w) / sw
  Q <- sum(w * (ratios - est) ^ 2)
  dof <- L - 1L
  if (dof > 0L) {
    sw2 <- sum(w * w)
    denom <- sw - sw2 / sw
    tau2 <- if (denom > 0) max((Q - dof) / denom, 0) else 0
    I2 <- if (Q > 0) max(0, (Q - dof) / Q) else 0
  } else {
    tau2 <- 0
    I2 <- 0
  }

  # --- model-specific estimate, SE and phi --------------------------
  # The Python arm always reports the regression with first-order
  # 1/se_y^2 weights as the "regression_estimate" anchor, even when
  # second-order weights are used for the IVW; mirror that exactly.
  w_reg <- 1 / (sy * sy)
  num_reg <- sum(w_reg * bx * by)
  den_reg <- sum(w_reg * bx * bx)
  if (den_reg <= 0)
    stop("mtr2sx: the weighted design is degenerate")
  reg_est <- num_reg / den_reg
  reg_se_fixed <- sqrt(1 / den_reg)

  if (model == "fixed") {
    se <- se_fixed
    phi <- 1
  } else if (model == "multiplicative") {
    phi <- if (dof > 0L) max(sqrt(Q / dof), 1) else 1
    se <- se_fixed * phi
  } else {  # additive: DerSimonian-Laird
    w2 <- 1 / (var + tau2)
    est <- sum(ratios * w2) / sum(w2)
    se <- sqrt(1 / sum(w2))
    phi <- 1
  }

  z <- if (se > 0) est / se else Inf
  # 2 * pnorm(-abs(z)) is the base-R equivalent of math.erfc(abs(z)/sqrt(2))
  p_value <- 2 * pnorm(-abs(z))
  ci <- c(est - 1.96 * se, est + 1.96 * se)

  list(estimate = est, se = se, z = z, p_value = p_value, ci = ci,
       ratio_estimates = ratios, variances = var,
       weights_used = weights, model = model,
       phi_multiplicative = phi, tau2 = tau2,
       Q = Q, df = dof, I2 = I2,
       se_fixed = se_fixed,
       regression_estimate = reg_est,
       regression_se_fixed = reg_se_fixed,
       n_variants = L,
       method = sprintf("inverse-variance weighted MR (%s model, %s weights); Burgess & Bowden (2015) Sec. 2",
                        model, weights))
}
