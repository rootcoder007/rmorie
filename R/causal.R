#' Causal inference estimators for MORIE
#'
#' Implements ATE, ATT, ATC, GATE, CATE, and LATE via IPW, AIPW,
#' T-learner, and 2SLS. All estimators require propensity scores that
#' can be supplied or estimated internally via logistic regression.
#'
#' The Phase 1.h rewrite thin-wraps each estimator over the canonical
#' CRAN causal-inference packages while preserving the inline
#' implementation as a fallback (dual-arm pattern):
#'
#' * \code{morie_estimate_propensity_scores()} -> \pkg{WeightIt}
#'   (\code{WeightIt::weightit(method = "glm")}) when installed.
#' * \code{morie_estimate_ate/att/atc()} -> \pkg{WeightIt} weights with
#'   the inline Hajek / influence-function estimator preserved.
#' * \code{morie_estimate_aipw()} -> \pkg{AIPW} when installed, else
#'   the inline doubly-robust estimator.
#' * \code{morie_estimate_dr_forest()} -> \pkg{grf} causal forest with
#'   doubly-robust (AIPW) averaging; honest random-forest nuisances.
#' * \code{morie_estimate_g_computation()} -> \pkg{stdReg}
#'   (\code{stdReg::stdGlm}) when installed, else inline G-formula.
#' * \code{morie_estimate_late()} -> native Wald / 2SLS k-class
#'   engine (module 17).
#' * \code{morie_estimate_double_ml() / morie_estimate_irm()} ->
#'   \pkg{DoubleML} when installed, else inline cross-fit ridge
#'   (unchanged from prior release).
#' * \code{morie_e_value()} -> \pkg{EValue} when installed, else
#'   inline closed-form E-value.
#' * \code{morie_sensitivity_rosenbaum()} -> \pkg{rbounds} /
#'   \pkg{sensitivitymv} when installed, else inline sign-score bounds.
#'
#' Phase 1.h also adds four new \emph{extender} functions exposing
#' value-add from CRAN packages that previously had no MORIE entry
#' point:
#'
#' * \code{morie_causal_impact()} -> \pkg{CausalImpact}
#'   (Bayesian structural time-series intervention analysis).
#' * \code{morie_causal_weighting()} -> \pkg{WeightIt}
#'   (full \code{weightit()} interface with method = glm / cbps /
#'   ebal / ps / energy / optweight).
#' * \code{morie_causal_robust_se()} -> \pkg{sandwich}
#'   (HC0-HC5 / cluster / HAC robust variance matrices).
#'
#' @name causal
#' @keywords internal
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Internal helper: Causal Have Weightit
#' @noRd
.causal_have_weightit     <- function() {
  requireNamespace("WeightIt",     quietly = TRUE)
}
#' Internal helper: Causal Have Aipw
#' @noRd
.causal_have_aipw         <- function() {
  requireNamespace("AIPW",         quietly = TRUE)
}
#' Internal helper: Causal Have Stdreg
#' @noRd
.causal_have_stdreg       <- function() {
  requireNamespace("stdReg",       quietly = TRUE)
}
#' Internal helper: Causal Have Doubleml
#' @noRd
.causal_have_doubleml     <- function() {
  requireNamespace("DoubleML",     quietly = TRUE) &&
    requireNamespace("mlr3",         quietly = TRUE) &&
    requireNamespace("mlr3learners", quietly = TRUE) &&
    requireNamespace("ranger",       quietly = TRUE)
}
#' Internal helper: Causal Have Evalue
#' @noRd
.causal_have_evalue       <- function() {
  requireNamespace("EValue",       quietly = TRUE)
}
#' Internal helper: Causal Have Rbounds
#' @noRd
.causal_have_rbounds      <- function() {
  requireNamespace("rbounds",      quietly = TRUE)
}
#' Internal helper: Causal Have Sensitivitymv
#' @noRd
.causal_have_sensitivitymv <- function() {
  requireNamespace("sensitivitymv", quietly = TRUE)
}
#' Internal helper: Causal Have Causalimpact
#' @noRd
.causal_have_causalimpact <- function() {
  requireNamespace("CausalImpact", quietly = TRUE)
}
#' Internal helper: Causal Have Aer
#' @noRd
.causal_have_aer          <- function() {
  requireNamespace("AER",          quietly = TRUE)
}
#' Internal helper: Causal Have Grf
#' @noRd
.causal_have_grf          <- function() {
  requireNamespace("grf",          quietly = TRUE)
}
#' Internal helper: Causal Have Ivreg
#' @noRd
.causal_have_ivreg        <- function() {
  requireNamespace("ivreg",        quietly = TRUE) ||
    requireNamespace("AER",          quietly = TRUE)
}

#' Internal helper: Fit Propensity
#' @noRd
# Two propensity ESTIMATORS, both available, matching Python
# morie.fn.ps_fit._ps_irls exactly so the arms cannot drift apart by
# inheriting whatever each ecosystem's logistic regression defaults to:
#   ps_model = "mle"    unpenalised logistic maximum likelihood on the
#                       RAW covariates -- the textbook propensity
#                       model, and the default;
#   ps_model = "ridge"  L2-penalised logistic on STANDARDISED
#                       covariates, the penalty applying to the
#                       non-intercept coefficients with strength
#                       `ridge_lambda`; useful under collinearity or
#                       near-separation.
# Before 2026-08-12 this package always used the MLE route while the
# Python arm always used the ridge route, which is why their AIPW
# estimates disagreed at ~2e-04.

#' .mor_ps_design
#'
#' A step of the causal implementation. Called by \code{.fit_propensity}, \code{morie_estimate_aipw}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param data A vector; indexed elementwise.
#' @param covariates Iterated over elementwise, with \code{lapply}.
#' @return The value of \code{cbind}.
#' @export
.mor_ps_design <- function(data, covariates) {
  cols <- lapply(covariates, function(cn) {
    v <- data[[cn]]
    if (is.numeric(v)) return(as.numeric(v))
    lv <- sort(unique(as.character(v)))
    as.numeric(match(as.character(v), lv) - 1L)
  })
  cbind(1, do.call(cbind, cols))
}

#' .mor_ps_standardize
#'
#' A step of the causal implementation. Called by \code{.fit_propensity}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @return The value of \code{X}, as built in the body.
#' @export
.mor_ps_standardize <- function(X) {
  n <- nrow(X)
  for (j in seq.int(2L, ncol(X))) {
    m <- mean(X[, j])
    s <- sqrt(sum((X[, j] - m)^2) / n)
    if (s <= 0) s <- 1
    X[, j] <- (X[, j] - m) / s
  }
  X
}

#' .mor_ps_irls
#'
#' A step of the causal implementation. Called by \code{.fit_propensity}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X A matrix; passed to \code{\%*\%}.
#' @param y Passed to \code{.mor_ps_irls_beta}.
#' @param lam Passed to \code{.mor_ps_irls_beta}. Defaults to \code{0}.
#' @param max_iter Passed to \code{.mor_ps_irls_beta}. Defaults to \code{200L}.
#' @param tol Passed to \code{.mor_ps_irls_beta}. Defaults to \code{1e-12}.
#' @return A numeric value.
#' @export
.mor_ps_irls <- function(X, y, lam = 0, max_iter = 200L, tol = 1e-12) {
  beta <- .mor_ps_irls_beta(X, y, lam = lam, max_iter = max_iter, tol = tol)
  eta <- pmin(pmax(as.numeric(X %*% beta), -30), 30)
  1 / (1 + exp(-eta))
}

#' .mor_ps_irls_beta
#'
#' A step of the causal implementation. Called by \code{.mor_om_fit_predict}, \code{.mor_ps_irls}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; combined arithmetically in the body.
#' @param lam Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-12}.
#' @return The value of \code{beta}, as built in the body.
#' @export
.mor_ps_irls_beta <- function(X, y, lam = 0, max_iter = 200L, tol = 1e-12) {
  n <- nrow(X); p <- ncol(X)
  beta <- numeric(p)
  pen <- c(0, rep(as.numeric(lam), p - 1L))
  for (it in seq_len(as.integer(max_iter))) {
    eta <- pmin(pmax(as.numeric(X %*% beta), -30), 30)
    mu <- 1 / (1 + exp(-eta))
    w <- pmax(mu * (1 - mu), 1e-10)
    z <- eta + (y - mu) / w
    A <- crossprod(X, X * w) + diag(pen, p)
    rhs <- crossprod(X, w * z)
    new <- as.numeric(solve(A, rhs))
    delta <- max(abs(new - beta))
    beta <- new
    if (delta < tol) break
  }
  beta
}


#' .fit_propensity
#'
#' A step of the causal implementation. Called by \code{.fit_propensity_weightit}, \code{morie_estimate_propensity_scores}, \code{morie_weight_ow} and 1 others in the module.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param data A vector; indexed elementwise.
#' @param treatment See Usage.
#' @param covariates Passed to \code{.mor_ps_design}.
#' @param ps_model One of \code{"mle"}, \code{"ridge"}. Defaults to \code{"mle"}.
#' @param ridge_lambda Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return One of two values, depending on the branch taken.
#' @export
.fit_propensity <- function(data, treatment, covariates,
                            ps_model = "mle", ridge_lambda = 1) {
  if (!(ps_model %in% c("mle", "ridge")))
    stop("ps_model must be 'mle' or 'ridge'")
  y <- as.numeric(data[[treatment]])
  X <- .mor_ps_design(data, covariates)
  if (ps_model == "ridge") {
    X <- .mor_ps_standardize(X)
    .mor_ps_irls(X, y, lam = as.numeric(ridge_lambda))
  } else {
    .mor_ps_irls(X, y, lam = 0)
  }
}

#' Internal helper: Fit Propensity Weightit
#' @noRd
.fit_propensity_weightit <- function(data, treatment, covariates) {
  # WeightIt's method = "glm" IS a logistic propensity fit -- the
  # native .fit_propensity computes the identical scores, so the
  # delegation was pure overhead (module 15).
  .fit_propensity(data, treatment, covariates)
}

#' Internal helper: Clip Ps
#' @noRd
.clip_ps <- function(ps, eps = 1e-6) {
  pmin(pmax(ps, eps), 1 - eps)
}

#' Internal helper: Hajek Diff
#' @noRd
.hajek_diff <- function(y1, w1, y0, w0) {
  sum(y1 * w1) / sum(w1) - sum(y0 * w0) / sum(w0)
}

#' Internal helper: Influence Score Aipw
#' @noRd
.influence_score_aipw <- function(y, t, ps, mu1, mu0) {
  (mu1 - mu0) +
    t * (y - mu1) / ps -
    (1 - t) * (y - mu0) / (1 - ps)
}


# ---------------------------------------------------------------------------
# Propensity scores
# ---------------------------------------------------------------------------

#' Estimate propensity scores via logistic regression
#'
#' Thin wrapper over \code{WeightIt::weightit(method = "glm",
#' estimand = "ATE")} when \pkg{WeightIt} is installed; falls back
#' to \code{stats::glm(family = binomial())} otherwise.
#'
#' @param data A data frame.
#' @param treatment Name of the binary treatment column.
#' @param covariates Character vector of covariate names.
#' @param trim Quantile pair used to winsorize extreme scores
#'   (default 0.01, 0.99).
#' @return Numeric vector of propensity scores (same length as
#'   \code{nrow(data)}).
#' @export
#' @examples
#' df <- data.frame(t = c(0, 1, 0, 1, 0, 1), x = rnorm(6))
#' ps <- morie_estimate_propensity_scores(df, "t", "x")
# Trim propensity scores.  BOTH routes in use are available and the
# choice is explicit, mirroring Python morie.fn.aipw._trim_ps:
#   trim_type = "value"    clamp to the absolute bounds `trim`.  Sample
#                          independent, so a stratified fit cannot be
#                          destabilised by a small stratum's own
#                          quantiles.  Default.
#   trim_type = "quantile" winsorise the SCORES at their own sample
#                          quantiles.  Percentile capping is the
#                          standard trimming device in this literature:
#                          Lee, B. K., Lessler, J. and Stuart, E. A.
#                          (2011), Weight trimming and propensity score
#                          weighting, PLoS ONE 6(3), e18174,
#                          doi:10.1371/journal.pone.0018174, "trimmed
#                          high weights downwards, with cutpoints
#                          ranging from the 99th to the 50th
#                          percentiles ... all weights with value above
#                          the [cutpoint] were set equal to the
#                          [cutpoint]"; see also Cole, S. R. and
#                          Hernan, M. A. (2008), Constructing inverse
#                          probability weights for marginal structural
#                          models, American Journal of Epidemiology
#                          168(6), 656-664.
#                          TWO DIFFERENCES from Lee et al., stated so
#                          the citation is not overclaimed: they cap
#                          the WEIGHTS, this caps the SCORES (which
#                          bounds the weights indirectly); and they cap
#                          the high side only, this caps both tails.
#                          A weights-side route is the natural next
#                          addition -- see NEEDED_SOURCES.md.
#                          It is also NOT the rule of Crump, Hotz,
#                          Imbens and Mitnik (2009), Biometrika 96(1),
#                          187-199 -- verified against that paper, they
#                          DISCARD units whose estimated propensity
#                          lies outside a range (rule of thumb
#                          [0.1, 0.9]), which changes the estimand to
#                          the ATE on the retained subpopulation.
#                          Neither route here discards.
#   trim = NULL            no trimming beyond the numerical guard that
#                          keeps the inverse-probability weights finite.
# NOTE: before 2026-08-12 this package always winsorised at the 1st and
# 99th sample quantiles while the Python arm clamped at the absolute
# values 0.01 / 0.99, so the two arms disagreed by ~1e-4 pooled and
# ~1e-3 within small strata.  The shared default is now
# trim_type = "value"; pass trim_type = "quantile" for the old
# behaviour.
.MOR_PS_EPS <- 1e-6

# Cap the IPW WEIGHTS at percentile cutpoints -- the operation Lee,
# B. K., Lessler, J. and Stuart, E. A. (2011), "Weight Trimming and
# Propensity Score Weighting", PLoS ONE 6(3), e18174, actually
# perform: "we trimmed high weights downwards, with cutpoints ranging
# from the 99th to the 50th percentiles ... all weights with value
# above the [cutpoint] were set equal to the [cutpoint]".  Note that
# they cap the HIGH side only, which is why side = "upper" is the
# default here.  Contrast .mor_trim_ps, which caps the SCORES.
# See also Cole, S. R. and Hernan, M. A. (2008), American Journal of
# Epidemiology 168(6), 656-664.
#' Cap the IPW WEIGHTS at percentile cutpoints -- the operation Lee,
#'
#' B. K., Lessler, J. and Stuart, E. A. (2011), "Weight Trimming and
#' Propensity Score Weighting", PLoS ONE 6(3), e18174, actually perform:
#' "we trimmed high weights downwards, with cutpoints ranging from the
#' 99th to the 50th percentiles ... all weights with value above the
#' [cutpoint] were set equal to the [cutpoint]".  Note that they cap the
#' HIGH side only, which is why side = "upper" is the default here.
#' Contrast .mor_trim_ps, which caps the SCORES. See also Cole, S. R.
#' and Hernan, M. A. (2008), American Journal of Epidemiology 168(6),
#' 656-664.
#'
#' @param w Passed to \code{return}.
#' @param weight_trim Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param side One of \code{"both"}, \code{"upper"}. Defaults to \code{"upper"}.
#' @return One of two values, depending on the branch taken.
#' @export
.mor_trim_weights <- function(w, weight_trim = NULL, side = "upper") {
  if (is.null(weight_trim)) return(w)
  if (!(side %in% c("upper", "both")))
    stop("weight_trim_side must be 'upper' or 'both'")
  q <- as.numeric(weight_trim)
  if (length(q) == 1L) q <- c(0, q)
  lo <- q[1]; hi <- q[2]
  if (!(lo >= 0 && lo < hi && hi <= 1))
    stop("weight_trim must satisfy 0 <= lo < hi <= 1")
  cuts <- stats::quantile(w, c(lo, hi), names = FALSE, type = 7)
  if (side == "both") pmin(pmax(w, cuts[1]), cuts[2]) else pmin(w, cuts[2])
}

# Which units SURVIVE trimming.  Only trim_type = "discard" drops
# anything; the winsorising routes keep every unit.
#
# "discard" is the rule of Crump, R. K., Hotz, V. J., Imbens, G. W. and
# Mitnik, O. A. (2009), "Dealing with limited overlap in estimation of
# average treatment effects", Biometrika 96(1), 187-199, verified
# verbatim against that paper: "a good approximation to the optimal
# rule is provided by the simple selection rule to drop all units with
# estimated propensity scores outside the range [0.1,0.9]".
#
# LOUD WARNING, because this is not a numerical detail: discarding
# CHANGES THE ESTIMAND.  The result is no longer the ATE on the whole
# sample but the ATE on the retained subpopulation -- which is the
# entire point of their Sec. 5, since that subpopulation is the one the
# data can actually identify.  The estimators report n_discarded and an
# estimand note whenever this route is taken.
#' LOUD WARNING, because this is not a numerical detail: discarding
#'
#' CHANGES THE ESTIMAND.  The result is no longer the ATE on the whole
#' sample but the ATE on the retained subpopulation -- which is the
#' entire point of their Sec. 5, since that subpopulation is the one the
#' data can actually identify.  The estimators report n_discarded and an
#' estimand note whenever this route is taken.
#'
#' @param ps A vector; its length is taken.
#' @param trim Optional; may be \code{NULL}. A vector; indexed elementwise. Defaults to \code{c(0.1, 0.9)}.
#' @param trim_type Passed to \code{identical}. Defaults to \code{"value"}.
#' @return A logical value.
#' @export
.mor_ps_keep <- function(ps, trim = c(0.1, 0.9), trim_type = "value") {
  if (!identical(trim_type, "discard") || is.null(trim))
    return(rep(TRUE, length(ps)))
  lo <- as.numeric(trim[1]); hi <- as.numeric(trim[2])
  if (!(lo >= 0 && lo < hi && hi <= 1))
    stop("trim must satisfy 0 <= lo < hi <= 1")
  ps >= lo & ps <= hi
}

#' .mor_trim_ps
#'
#' A step of the causal implementation. Called by \code{morie_estimate_aipw}, \code{morie_estimate_ate}, \code{morie_estimate_propensity_scores}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param ps Coerced to numeric by the body, with \code{as.numeric}.
#' @param trim Optional; may be \code{NULL}. A vector; indexed elementwise. Defaults to \code{c(0.01, 0.99)}.
#' @param trim_type One of \code{"discard"}, \code{"quantile"}, \code{"value"}. Defaults to \code{"value"}.
#' @return The value of \code{pmin}.
#' @export
.mor_trim_ps <- function(ps, trim = c(0.01, 0.99), trim_type = "value") {
  ps <- as.numeric(ps)
  if (!(trim_type %in% c("value", "quantile", "discard")))
    stop("trim_type must be 'value', 'quantile' or 'discard'")
  if (!is.null(trim) && trim_type != "discard") {
    lo <- as.numeric(trim[1]); hi <- as.numeric(trim[2])
    if (!(lo >= 0 && lo < hi && hi <= 1))
      stop("trim must satisfy 0 <= lo < hi <= 1")
    if (trim_type == "quantile") {
      qs <- stats::quantile(ps, c(lo, hi), names = FALSE, type = 7)
      lo <- qs[1]; hi <- qs[2]
    }
    ps <- pmin(pmax(ps, lo), hi)
  }
  pmin(pmax(ps, .MOR_PS_EPS), 1 - .MOR_PS_EPS)
}


#' morie_estimate_propensity_scores
#'
#' A step of the causal implementation. Called by \code{morie_estimate_aipw}, \code{morie_estimate_atc}, \code{morie_estimate_ate} and 1 others in the module.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param data Passed to \code{.fit_propensity}.
#' @param treatment Passed to \code{.fit_propensity}.
#' @param covariates Passed to \code{.fit_propensity}.
#' @param trim Passed to \code{.mor_trim_ps}. Defaults to \code{c(0.01, 0.99)}.
#' @param trim_type Passed to \code{.mor_trim_ps}. Defaults to \code{"value"}.
#' @param ps_model Passed to \code{.fit_propensity}. Defaults to \code{"mle"}.
#' @param ridge_lambda Passed to \code{.fit_propensity}. Defaults to \code{1}.
#' @return The value of \code{.mor_trim_ps}.
#' @export
morie_estimate_propensity_scores <- function(data, treatment, covariates,
                                             trim = c(0.01, 0.99),
                                             trim_type = "value",
                                             ps_model = "mle",
                                             ridge_lambda = 1) {
  # the native fit is used unconditionally now: WeightIt's method
  # = "glm" is only the MLE route, so delegating to it would silently
  # ignore ps_model = "ridge" and break the parity the routes exist to
  # guarantee.
  ps <- .fit_propensity(data, treatment, covariates,
                        ps_model = ps_model, ridge_lambda = ridge_lambda)
  .mor_trim_ps(ps, trim, trim_type)
}


# ---------------------------------------------------------------------------
# ATE -- Hajek IPW
# ---------------------------------------------------------------------------

#' Estimate the Average Treatment Effect (ATE) via Hajek IPW
#'
#' The Hajek estimator uses stabilised IPW weights:
#' \deqn{\widehat{ATE} = \bar{y}_1^{w} - \bar{y}_0^{w}}{ATE_hat = y_bar_1^w - y_bar_0^w}
#' where \eqn{\bar{y}_t^{w} = \sum_{T_i=t} w_i Y_i / \sum_{T_i=t} w_i}{y_bar_t^w = sum_T_i=t w_i Y_i / sum_T_i=t w_i}
#' and \eqn{w_i = T_i/\hat{e}(X_i) + (1-T_i)/(1-\hat{e}(X_i))}{w_i = T_i/e_hat(X_i) + (1-T_i)/(1-e_hat(X_i))}.
#'
#' When \pkg{WeightIt} is installed the propensity step delegates to
#' \code{WeightIt::weightit()}; otherwise the inline logistic
#' regression is used. The Hajek difference and influence-function SE
#' below are evaluated inline either way so the result list shape and
#' the closed-form variance preserved.
#'
#' @param data A data frame.
#' @param treatment Name of the binary treatment column.
#' @param outcome Name of the outcome column.
#' @param covariates Character vector of covariate names.
#' @param propensity_col Optional: name of a pre-computed propensity
#'   score column.
#' @return Named list: `ate`, `se`, `ci_lower`, `ci_upper`, `n`, `ess`.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   t = rbinom(200, 1, 0.4),
#'   y = rnorm(200),
#'   x = rnorm(200)
#' )
#' morie_estimate_ate(df, "t", "y", "x")
morie_estimate_ate <- function(data, treatment, outcome, covariates,
                               propensity_col = NULL,
                               trim = c(0.01, 0.99),
                               trim_type = "value",
                               ps_model = "mle",
                               ridge_lambda = 1,
                               weight_trim = NULL,
                               weight_trim_side = "upper") {
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  # same propensity routes as morie_estimate_aipw: leaving this
  # function on the old fixed .clip_ps path would mean the Hajek IPW
  # estimator and the AIPW estimator silently used different
  # propensity scores on the same data.
  ps <- if (!is.null(propensity_col)) {
    .mor_trim_ps(data[[propensity_col]], trim, trim_type)
  } else {
    morie_estimate_propensity_scores(data, treatment, covariates,
                                     trim = trim, trim_type = trim_type,
                                     ps_model = ps_model,
                                     ridge_lambda = ridge_lambda)
  }

  keep <- .mor_ps_keep(ps, trim, trim_type)
  n_discarded <- sum(!keep)
  if (n_discarded > 0L) {
    if (sum(keep) < 2L) stop("discard trimming removed almost every unit")
    t <- t[keep]; y <- y[keep]; ps <- ps[keep]
  }
  w <- t / ps + (1 - t) / (1 - ps)
  w <- .mor_trim_weights(w, weight_trim, weight_trim_side)
  ate <- .hajek_diff(y[t == 1], w[t == 1], y[t == 0], w[t == 0])
  # Standard IPW influence-function SE (Hernan-Robins, "What If" Ch 12.6):
  # psi_i = t*y/ps - (1-t)*y/(1-ps) - ATE. Divides by sqrt(total n).
  # NB: This is the "known propensity score" form. When ps is estimated
  # (the default path via morie_estimate_propensity_scores), this SE is
  # conservative -- it ignores the PS-estimation step and slightly
  # over-estimates variance. Standard for IPW packages; bootstrap or
  # WeightIt / survey for the efficient sandwich correction.
  if_vec <- t * y / ps - (1 - t) * y / (1 - ps) - ate
  se <- stats::sd(if_vec) / sqrt(length(y))
  ci <- .wald_ci(ate, se)
  ess <- (sum(w)^2) / sum(w^2)

  list(

    n_used = length(y),

    n_discarded = n_discarded,

    estimand = if (n_discarded > 0)

      "ATE on the retained subpopulation (Crump et al. 2009 discard)" else

      "ATE on the full sample",
    ate = ate, se = se, ci_lower = ci[1], ci_upper = ci[2],
    n = length(y), ess = ess
  )
}


# ---------------------------------------------------------------------------
# ATT -- Average Treatment Effect on the Treated
# ---------------------------------------------------------------------------

#' Estimate the Average Treatment Effect on the Treated (ATT)
#'
#' Treated units receive weight 1; controls receive
#' \eqn{w_i = \hat{e}(X_i)/(1-\hat{e}(X_i))}{w_i = e_hat(X_i)/(1-e_hat(X_i))}.
#'
#' Propensity-score estimation delegates to \pkg{WeightIt} when
#' installed (via \code{morie_estimate_propensity_scores}); the
#' weighted-difference and influence-function SE run inline.
#'
#' @inheritParams morie_estimate_ate
#' @return Named list: `att`, `se`, `ci_lower`, `ci_upper`, `n_treated`.
#' @export
#' @examples
#' set.seed(2)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_att(df, "t", "y", "x")
morie_estimate_att <- function(data, treatment, outcome, covariates,
                               propensity_col = NULL) {
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  ps <- if (!is.null(propensity_col)) {
    .clip_ps(data[[propensity_col]])
  } else {
    morie_estimate_propensity_scores(data, treatment, covariates)
  }

  # Control weights: e(X) / (1 - e(X))
  w_ctrl <- ps / (1 - ps)
  mean_t <- mean(y[t == 1])
  mean_c <- sum(y[t == 0] * w_ctrl[t == 0]) / sum(w_ctrl[t == 0])
  att <- mean_t - mean_c

  n1 <- sum(t == 1)
  n <- length(y)
  # Influence-function SE for IPW-ATT (Imbens & Wooldridge 2009 §5.5).
  # psi_i = [t*Y - (1-t)*Y*ps/(1-ps)] / E[t] - t*ATT / E[t].
  # Divides by sqrt(total n), not sqrt(n_treated).
  # NB: "known propensity score" form; conservative when ps is estimated
  # (slightly over-estimates variance). See morie_estimate_ate notes.
  p_t <- mean(t)
  if_vec <- (t * y - (1 - t) * y * w_ctrl) / p_t - t * att / p_t
  se <- stats::sd(if_vec) / sqrt(n)
  ci <- .wald_ci(att, se)

  list(att = att, se = se, ci_lower = ci[1], ci_upper = ci[2], n_treated = n1)
}


# ---------------------------------------------------------------------------
# ATC -- Average Treatment Effect on the Controls
# ---------------------------------------------------------------------------

#' Estimate the Average Treatment Effect on the Controls (ATC)
#'
#' Control units receive weight 1; treated units receive
#' \eqn{w_i = (1-\hat{e}(X_i))/\hat{e}(X_i)}{w_i = (1-e_hat(X_i))/e_hat(X_i)}.
#'
#' Propensity-score estimation delegates to \pkg{WeightIt} when
#' installed (via \code{morie_estimate_propensity_scores}); the
#' weighted-difference and influence-function SE run inline.
#'
#' @inheritParams morie_estimate_ate
#' @return Named list: `atc`, `se`, `ci_lower`, `ci_upper`, `n_control`.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_atc(df, "t", "y", "x")
#' @export
morie_estimate_atc <- function(data, treatment, outcome, covariates,
                               propensity_col = NULL) {
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  ps <- if (!is.null(propensity_col)) {
    .clip_ps(data[[propensity_col]])
  } else {
    morie_estimate_propensity_scores(data, treatment, covariates)
  }

  w_trt <- (1 - ps) / ps
  mean_treated_reweighted <-
    sum(y[t == 1] * w_trt[t == 1]) / sum(w_trt[t == 1])
  mean_c <- mean(y[t == 0])
  atc <- mean_treated_reweighted - mean_c

  n0 <- sum(t == 0)
  n <- length(y)
  # Influence-function SE for IPW-ATC (mirror of ATT, swapping roles).
  # psi_i = [t*Y*(1-ps)/ps - (1-t)*Y] / E[1-t] - (1-t)*ATC / E[1-t].
  # Divides by sqrt(total n), not sqrt(n_control).
  # NB: "known propensity score" form; conservative when ps is estimated.
  p_c <- mean(1 - t)
  if_vec <- (t * y * w_trt - (1 - t) * y) / p_c - (1 - t) * atc / p_c
  se <- stats::sd(if_vec) / sqrt(n)
  ci <- .wald_ci(atc, se)

  list(atc = atc, se = se, ci_lower = ci[1], ci_upper = ci[2], n_control = n0)
}


# ---------------------------------------------------------------------------
# AIPW -- Doubly Robust ATE
# ---------------------------------------------------------------------------

#' Augmented IPW (AIPW) doubly-robust ATE estimator
#'
#' Combines IPW and outcome regression corrections. Consistent if
#' \strong{either} the propensity model \strong{or} the outcome model
#' is correctly specified.
#'
#' The propensity step delegates to \pkg{WeightIt} when installed
#' (via \code{morie_estimate_propensity_scores}). The outcome
#' regression and the doubly-robust influence-function score are
#' evaluated inline to preserve the closed-form SE used downstream.
#' Where richer outputs are desired, \code{AIPW::AIPW} (with SuperLearner
#' nuisance learners) is the canonical CRAN counterpart.
#'
#' @inheritParams morie_estimate_ate
#' @param outcome_model Family for the outcome model: `"linear"` or
#'   `"logistic"`.
#' @return Named list: `ate`, `se`, `ci_lower`, `ci_upper`, `n`.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_aipw(df, "t", "y", "x")
#' @export
# Outcome-model routes for AIPW, matching Python
# morie.fn.aipw._om_fit_predict exactly.
#   outcome_fit = "separate" (default) fits E[Y | X, T = t] on each arm,
#                 so the covariate slopes may differ between treated and
#                 control -- the usual AIPW form;
#   outcome_fit = "pooled"   fits one regression Y ~ T + X and predicts
#                 with T set to 1 and to 0, imposing a common slope.
# Before 2026-08-12 this arm was pooled while the Python arm was
# separate, silently, which is the last of the three differences that
# made their AIPW estimates disagree.

#' .mor_om_fit_predict
#'
#' A step of the causal implementation. Called by \code{morie_estimate_aipw}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param y A vector; indexed elementwise.
#' @param rows See Usage.
#' @param Xpred A matrix; passed to \code{\%*\%}.
#' @param outcome_model Compared against \code{"logistic"}.
#' @return A vector, from \code{as.numeric}.
#' @export
.mor_om_fit_predict <- function(X, y, rows, Xpred, outcome_model) {
  Xs <- X[rows, , drop = FALSE]
  ys <- y[rows]
  if (outcome_model == "logistic") {
    beta <- .mor_ps_irls_beta(Xs, ys, lam = 0)
    eta <- pmin(pmax(as.numeric(Xpred %*% beta), -30), 30)
    return(1 / (1 + exp(-eta)))
  }
  beta <- as.numeric(solve(crossprod(Xs), crossprod(Xs, ys)))
  as.numeric(Xpred %*% beta)
}


#' morie_estimate_aipw
#'
#' A step of the causal implementation. Called by \code{morie_dag_estimate}, \code{morie_estimate_gate}, \code{morie_gate} and 1 others in the module.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param data A vector; indexed elementwise.
#' @param treatment Passed to \code{morie_estimate_propensity_scores}.
#' @param outcome See Usage.
#' @param covariates Passed to \code{morie_estimate_propensity_scores}.
#' @param propensity_col Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param outcome_model Passed to \code{.mor_om_fit_predict}. Defaults to \code{c("linear", "logistic")}.
#' @param trim Passed to \code{.mor_trim_ps}. Defaults to \code{c(0.01, 0.99)}.
#' @param trim_type Passed to \code{.mor_trim_ps}. Defaults to \code{"value"}.
#' @param ps_model Passed to \code{morie_estimate_propensity_scores}. Defaults to \code{"mle"}.
#' @param ridge_lambda Passed to \code{morie_estimate_propensity_scores}. Defaults to \code{1}.
#' @param outcome_fit One of \code{"pooled"}, \code{"separate"}. Defaults to \code{"separate"}.
#' @return A list with \code{n_used}, \code{n_discarded}, \code{estimand}, \code{ate}, \code{se}, \code{ci_lower}, \code{ci_upper}, \code{n}.
#' @export
morie_estimate_aipw <- function(data, treatment, outcome, covariates,
                                propensity_col = NULL,
                                outcome_model = c("linear", "logistic"),
                                trim = c(0.01, 0.99),
                                trim_type = "value",
                                ps_model = "mle",
                                ridge_lambda = 1,
                                outcome_fit = "separate") {
  outcome_model <- match.arg(outcome_model)
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  ps <- if (!is.null(propensity_col)) {
    .mor_trim_ps(data[[propensity_col]], trim, trim_type)
  } else {
    morie_estimate_propensity_scores(data, treatment, covariates,
                                     trim = trim, trim_type = trim_type,
                                     ps_model = ps_model,
                                     ridge_lambda = ridge_lambda)
  }

  if (!(outcome_fit %in% c("separate", "pooled")))
    stop("outcome_fit must be 'separate' or 'pooled'")
  Xc <- .mor_ps_design(data, covariates)
  # Crump et al. discard route: drop units outside the overlap range
  # BEFORE fitting anything, and remember how many went.
  keep <- .mor_ps_keep(ps, trim, trim_type)
  n_discarded <- sum(!keep)
  if (n_discarded > 0L) {
    if (sum(keep) < 2L) stop("discard trimming removed almost every unit")
    t <- t[keep]; y <- y[keep]; ps <- ps[keep]
    Xc <- Xc[keep, , drop = FALSE]
  }
  n <- nrow(Xc)
  if (outcome_fit == "pooled") {
    Xp <- cbind(Xc[, 1], t, Xc[, -1, drop = FALSE])
    X1 <- cbind(Xc[, 1], rep(1, n), Xc[, -1, drop = FALSE])
    X0 <- cbind(Xc[, 1], rep(0, n), Xc[, -1, drop = FALSE])
    rows <- seq_len(n)
    mu1 <- .mor_om_fit_predict(Xp, y, rows, X1, outcome_model)
    mu0 <- .mor_om_fit_predict(Xp, y, rows, X0, outcome_model)
  } else {
    mu1 <- .mor_om_fit_predict(Xc, y, which(t == 1), Xc, outcome_model)
    mu0 <- .mor_om_fit_predict(Xc, y, which(t == 0), Xc, outcome_model)
  }

  psi <- .influence_score_aipw(y, t, ps, mu1, mu0)
  # (n_discarded / estimand note are appended to the result below)
  ate <- mean(psi)
  se <- stats::sd(psi) / sqrt(length(psi))
  ci <- .wald_ci(ate, se)

  list(

    n_used = length(y),

    n_discarded = n_discarded,

    estimand = if (n_discarded > 0)

      "ATE on the retained subpopulation (Crump et al. 2009 discard)" else

      "ATE on the full sample",ate = ate, se = se, ci_lower = ci[1], ci_upper = ci[2], n = length(y))
}

#' Doubly-robust ATE via causal forest (grf)
#'
#' Native rmorie causal forest: the R-learner decomposition (Nie &
#' Wager 2021) with cross-fit nuisances and a weighted subsampled
#' regression forest for tau(x), combined through the AIPW orthogonal
#' score — the same estimand grf's
#' \code{average_treatment_effect(method = "AIPW")} targets
#' (cross-validated against grf in the package's cross tests). A
#' machine-learning alternative to the GLM-based
#' \code{morie_estimate_aipw()}. No grf at runtime.
#'
#' @inheritParams morie_estimate_aipw
#' @param target_sample Sample to average over, passed to grf: one of
#'   \code{"all"}, \code{"treated"}, \code{"control"}, \code{"overlap"}.
#' @return A list with \code{ate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{n}.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_dr_forest(df, "t", "y", "x")
#' @export
morie_estimate_dr_forest <- function(data, treatment, outcome, covariates,
                                     target_sample = c("all", "treated",
                                                       "control", "overlap")) {
  target_sample <- match.arg(target_sample)
  X <- stats::model.matrix(
    stats::as.formula(paste("~", paste(covariates, collapse = " + "))),
    data = data
  )[, -1, drop = FALSE]
  Y <- as.numeric(data[[outcome]])
  W <- as.numeric(data[[treatment]])
  nf <- .morie_causal_forest_native(X, Y, W)
  est <- .morie_causal_forest_ate(nf, Y, W, target_sample)
  ci <- .wald_ci(est$ate, est$se)
  list(ate = est$ate, se = est$se, ci_lower = ci[1], ci_upper = ci[2],
       n = length(Y))
}


# ---------------------------------------------------------------------------
# GATE -- Group Average Treatment Effect
# ---------------------------------------------------------------------------

#' Estimate Group Average Treatment Effects (GATE)
#'
#' Applies AIPW within each level of `group_col` to estimate
#' stratum-specific treatment effects.
#'
#' @inheritParams morie_estimate_aipw
#' @param group_col Name of the grouping variable (e.g. `"gender"`).
#' @return Data frame with columns: `group`, `ate`, `se`,
#'   `ci_lower`, `ci_upper`, `n`.
#' @export
#' @examples
#' set.seed(3)
#' df <- data.frame(
#'   t = rbinom(300, 1, 0.4),
#'   y = rnorm(300),
#'   x = rnorm(300),
#'   g = sample(c("A", "B"), 300, replace = TRUE)
#' )
#' morie_estimate_gate(df, "t", "y", "x", "g")
morie_estimate_gate <- function(data, treatment, outcome, covariates,
                                group_col, propensity_col = NULL,
                                outcome_model = c("linear", "logistic")) {
  outcome_model <- match.arg(outcome_model)
  groups <- unique(data[[group_col]])
  results <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    g <- groups[i]
    sub <- data[data[[group_col]] == g, , drop = FALSE]
    if (nrow(sub) < 10 || length(unique(sub[[treatment]])) < 2) {
      results[[i]] <- data.frame(
        group = g, ate = NA_real_, se = NA_real_,
        ci_lower = NA_real_, ci_upper = NA_real_, n = nrow(sub)
      )
      next
    }
    est <- tryCatch(
      morie_estimate_aipw(sub, treatment, outcome, covariates,
        propensity_col = propensity_col,
        outcome_model = outcome_model
      ),
      error = function(e) {
        list(
          ate = NA_real_, se = NA_real_,
          ci_lower = NA_real_, ci_upper = NA_real_
        )
      }
    )
    results[[i]] <- data.frame(
      group = g, ate = est$ate, se = est$se,
      ci_lower = est$ci_lower, ci_upper = est$ci_upper, n = nrow(sub)
    )
  }
  do.call(rbind, results)
}


# ---------------------------------------------------------------------------
# CATE -- Conditional (per-unit) treatment effects via T-learner
# ---------------------------------------------------------------------------

#' Estimate per-unit Conditional Average Treatment Effects (CATE)
#'
#' The \strong{T-learner} fits separate outcome models on treated and
#' control units, then predicts the counterfactual for each unit:
#' \eqn{\widehat{CATE}_i = \hat{\mu}_1(X_i) - \hat{\mu}_0(X_i)}{CATE_hat_i = mu_hat_1(X_i) - mu_hat_0(X_i)}.
#'
#' The \strong{S-learner} fits one model with treatment as a feature.
#'
#' For random-forest CATE estimation prefer \code{grf::causal_forest}
#' (richer heterogeneity, honest sample splitting).
#'
#' @inheritParams morie_estimate_aipw
#' @param meta_learner One of `"t_learner"` (default), `"s_learner"`,
#'   `"x_learner"` (Kuenzel et al. 2019 — two-stage with
#'   propensity-weighted combination; strong under arm imbalance), or
#'   `"dr_learner"` (Kennedy 2023 — cross-fit AIPW pseudo-outcome
#'   regressed with the native forest; doubly robust). All four are
#'   native; `outcome_model` applies to the T/S-learners only.
#' @return Numeric vector of per-unit CATE estimates.
#' @examples
#' morie_estimate_cate(
#'   data = data.frame(
#'     t = stats::rbinom(100, 1, 0.4),
#'     y = stats::rbinom(100, 1, 0.3), x1 = stats::rnorm(100),
#'     x2 = stats::rnorm(100)
#'   ), treatment = "t", outcome = "y",
#'   covariates = c("x1", "x2")
#' )
#' @export
morie_estimate_cate <- function(data, treatment, outcome, covariates,
                                propensity_col = NULL,
                                outcome_model = c("linear", "logistic"),
                                meta_learner = c("t_learner", "s_learner",
                                                 "x_learner", "dr_learner")) {
  outcome_model <- match.arg(outcome_model)
  meta_learner <- match.arg(meta_learner)
  if (meta_learner %in% c("x_learner", "dr_learner")) {
    df <- data[stats::complete.cases(
      data[, c(treatment, outcome, covariates)]), , drop = FALSE]
    X <- as.matrix(df[, covariates, drop = FALSE])
    y <- as.numeric(df[[outcome]])
    d <- as.numeric(df[[treatment]])
    return(if (meta_learner == "x_learner")
      .morie_cate_x_learner(X, y, d)
    else .morie_cate_dr_learner(X, y, d))
  }
  fam <- if (outcome_model == "logistic") {
    stats::binomial()
  } else {
    stats::gaussian()
  }
  t <- as.numeric(data[[treatment]])

  rhs <- paste(covariates, collapse = " + ")
  formula <- stats::as.formula(paste(outcome, "~", rhs))

  if (meta_learner == "t_learner") {
    fit1 <- stats::glm(formula, data = data[t == 1, , drop = FALSE],
                       family = fam)
    fit0 <- stats::glm(formula, data = data[t == 0, , drop = FALSE],
                       family = fam)
    mu1 <- as.numeric(stats::predict(fit1, newdata = data, type = "response"))
    mu0 <- as.numeric(stats::predict(fit0, newdata = data, type = "response"))
  } else {
    formula_s <- stats::as.formula(
      paste(outcome, "~", paste(c(treatment, covariates), collapse = " + "))
    )
    fit <- stats::glm(formula_s, data = data, family = fam)
    data1 <- data
    data1[[treatment]] <- 1
    data0 <- data
    data0[[treatment]] <- 0
    mu1 <- as.numeric(stats::predict(fit, newdata = data1, type = "response"))
    mu0 <- as.numeric(stats::predict(fit, newdata = data0, type = "response"))
  }

  mu1 - mu0
}


# ---------------------------------------------------------------------------
# LATE -- Local Average Treatment Effect via 2SLS (Wald estimator)
# ---------------------------------------------------------------------------

#' Estimate the Local Average Treatment Effect (LATE) via 2SLS / Wald
#'
#' Uses a binary instrument \eqn{Z} to identify the LATE
#' (Imbens & Angrist, 1994):
#' \deqn{LATE = \frac{Cov(Y, Z)}{Cov(T, Z)}}{LATE = (Cov(Y, Z))/(Cov(T, Z))}
#'
#' With covariates, the native 2SLS k-class engine is used
#' (module 17; validated against ivreg in \code{tests/cross/}).
#' Without covariates the closed-form Wald estimator and its
#' delta-method SE are used.
#'
#' @param data A data frame.
#' @param treatment Name of the binary endogenous treatment column.
#' @param outcome Name of the outcome column.
#' @param instrument Name of the binary instrument column.
#' @param covariates Optional character vector of exogenous
#'   covariates.
#' @return Named list: `late`, `se`, `ci_lower`, `ci_upper`,
#'   `first_stage_f`, `n`.
#' @export
#' @references
#'   Imbens GW, Angrist JD (1994). Identification and estimation of
#'   local average treatment effects. *Econometrica*, 62(2), 467-475.
#' @examples
#' set.seed(1)
#' n <- 300L
#' z <- rbinom(n, 1, 0.5)
#' t <- rbinom(n, 1, plogis(-0.2 + 1.5 * z))
#' y <- 0.8 * t + rnorm(n)
#' morie_estimate_late(data.frame(t = t, y = y, z = z), "t", "y", "z")
morie_estimate_late <- function(data, treatment, outcome, instrument,
                                covariates = NULL) {
  t <- as.numeric(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  z <- as.numeric(data[[instrument]])

  # First-stage F statistic (strength of instrument)
  fs_formula <- stats::as.formula(
    paste(
      treatment, "~", instrument,
      if (!is.null(covariates)) {
        paste("+", paste(covariates, collapse = " + "))
      } else {
        ""
      }
    )
  )
  fs_fit <- stats::lm(fs_formula, data = data)
  fs_f <- summary(fs_fit)$fstatistic[1]

  # Wald estimator (no covariates)
  if (is.null(covariates)) {
    num <- stats::cov(y, z)
    den <- stats::cov(t, z)
    if (abs(den) < 1e-10) stop("Weak instrument: Cov(T, Z) ~= 0")
    late <- num / den
    # Delta-method SE
    n <- length(y)
    var_num <- stats::var(z * (y - late * t)) / n
    se <- sqrt(var_num) / abs(den)
  } else {
    # Native 2SLS (k-class engine, module 17), homoskedastic SE to
    # match the ivreg default this wrapper historically reported.
    d <- .morie_iv_design(data, outcome, treatment, instrument,
                          covariates)
    fit_iv <- .morie_iv_kclass_native(d$y, d$X, d$Z, kappa = 1,
                                      robust = FALSE)
    late <- fit_iv$beta[[treatment]]
    se <- fit_iv$se[[treatment]]
  }

  ci <- .wald_ci(late, se)
  list(
    late = late, se = se, ci_lower = ci[1], ci_upper = ci[2],
    first_stage_f = as.numeric(fs_f), n = length(y)
  )
}


# ---------------------------------------------------------------------------
# E-value (VanderWeele & Ding 2017)
# ---------------------------------------------------------------------------

#' Compute E-value for unmeasured confounding
#'
#' The E-value quantifies the minimum strength of confounding
#' association needed to fully explain away an observed treatment
#' effect:
#' \deqn{E = RR + \sqrt{RR \cdot (RR - 1)}}{E = RR + sqrt(RR * (RR - 1))}
#'
#' For a risk ratio \eqn{RR < 1}, use \eqn{1/RR} before applying the
#' formula.
#'
#' Thin wrapper over \code{EValue::evalue()} when \pkg{EValue} is
#' installed; falls back to the inline closed-form computation
#' otherwise. Both arms produce numerically identical answers
#' (the formula above is the EValue closed-form for RR estimands).
#'
#' @param rr Risk ratio estimate (> 0). Supply > 1; if < 1, pass its
#'   reciprocal.
#' @param rr_lower Lower bound of the 95\% CI (used to compute
#'   E-value for CI).
#' @return Named list: `morie_e_value`, `e_value_ci` (for the CI
#'   bound).
#' @export
#' @references
#'   VanderWeele TJ, Ding P (2017). Sensitivity analysis in
#'   observational research: introducing the E-value. *Annals of
#'   Internal Medicine*, 167(4):268-274.
#' @examples
#' morie_e_value(rr = 3.9, rr_lower = 2.4)
morie_e_value <- function(rr, rr_lower = NULL) {
  # Module 26: the Ding-VanderWeele closed form is exact; a CI bound
  # whose interval covers 1 has E-value 1 (no confounding needed).
  compute_e <- function(r) {
    r <- if (r < 1) 1 / r else r
    if (r <= 1) 1 else r + sqrt(r * (r - 1))
  }
  ev <- compute_e(rr)
  ev_ci <- if (!is.null(rr_lower)) compute_e(rr_lower) else NA_real_
  list(morie_e_value = ev, e_value_ci = ev_ci)
}


# ---------------------------------------------------------------------------
# Sensitivity analysis -- Rosenbaum bounds
# ---------------------------------------------------------------------------

#' Rosenbaum bounds sensitivity analysis
#'
#' For a range of hidden-confounding levels \eqn{\Gamma}{Gamma},
#' tests whether the treatment effect remains significant. A large
#' \eqn{\Gamma}{Gamma} at which the result remains significant
#' indicates robustness.
#'
#' Delegates to \code{rbounds::psens()} when \pkg{rbounds} is
#' installed and pairs-of-equal-length data are supplied;
#' alternatively delegates to \code{sensitivitymv::senmv()} when
#' \pkg{sensitivitymv} is installed. Otherwise falls back to inline
#' sign-score bounds (Rosenbaum 2002, Section 4.3).
#'
#' @param treated Numeric vector of outcomes for treated units.
#' @param control Numeric vector of outcomes for control units
#'   (may differ in length from `treated` for unmatched designs).
#' @param gamma_range Numeric vector of \eqn{\Gamma}{Gamma} values to
#'   test.
#' @return Data frame with columns: `gamma`, `p_lower`, `p_upper`.
#' @examples
#' morie_sensitivity_rosenbaum(treated = rnorm(30, 0.5), control = rnorm(30))
#' @export
#' @references
#'   Rosenbaum PR (2002). *Observational Studies* (2nd ed.). Springer.
morie_sensitivity_rosenbaum <- function(treated, control,
                                        gamma_range = seq(1, 3, by = 0.2)) {
  n1 <- length(treated)
  n0 <- length(control)

  if (n1 == n0) {
    # Native Rosenbaum bounds for the Wilcoxon signed-rank statistic
    # on positionally-formed pairs -- the same quantity
    # rbounds::psens() reports (cross-validated in tests).
    tab <- lapply(gamma_range, function(gamma) {
      b <- .morie_psens_wilcoxon(treated, control, gamma)
      data.frame(gamma = gamma,
                 p_lower = as.numeric(b[["p_lower"]]),
                 p_upper = as.numeric(b[["p_upper"]]))
    })
    return(do.call(rbind, tab))
  }

  # Inline fallback: sign-score bounds (Rosenbaum 2002, Section 4.3).
  results <- lapply(gamma_range, function(gamma) {
    # Under null, each unit has outcome contribution +/-1
    y_diff <- outer(treated, control, "-")
    signs <- sign(y_diff)
    n_pairs <- n1 * n0

    # Upper bound: p-value under maximum assignment probability
    p_plus <- gamma / (1 + gamma)
    p_minus <- 1 / (1 + gamma)

    # Expected value and variance under gamma
    e_upper <- sum(p_plus * (signs > 0) + p_minus * (signs < 0))
    e_lower <- sum(p_minus * (signs > 0) + p_plus * (signs < 0))
    v <- n_pairs * p_plus * p_minus

    t_stat <- sum(signs > 0)
    p_upper <- 1 - stats::pnorm((t_stat - e_lower) / sqrt(v))
    p_lower <- 1 - stats::pnorm((t_stat - e_upper) / sqrt(v))

    data.frame(gamma = gamma, p_lower = p_lower, p_upper = p_upper)
  })

  do.call(rbind, results)
}


# ---------------------------------------------------------------------------
# G-computation (outcome regression ATE)
# ---------------------------------------------------------------------------

#' G-computation (outcome regression) ATE estimator
#'
#' Estimates the ATE by:
#' \deqn{\widehat{ATE} = \frac{1}{n}\sum_i \bigl\[\hat{\mu}_1(X_i) - \hat{\mu}_0(X_i)\bigr\]}{ATE_hat = (1)/(n)sum_i bigl\[mu_hat_1(X_i) - mu_hat_0(X_i)bigr\]}
#'
#' Delegates the standardisation step to \code{stdReg::stdGlm()} when
#' \pkg{stdReg} is installed; otherwise computes the contrast inline
#' from a single \code{stats::glm()} fit with treatment-flipped
#' counterfactual datasets.
#'
#' @inheritParams morie_estimate_aipw
#' @return Named list: `ate`, `se`, `ci_lower`, `ci_upper`.
#' @examples
#' set.seed(1)
#' df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
#' morie_estimate_g_computation(df, "t", "y", "x")
#' @export
morie_estimate_g_computation <- function(data, treatment, outcome,
                                         covariates,
                                         outcome_model = c("linear",
                                                           "logistic")) {
  outcome_model <- match.arg(outcome_model)
  fam <- if (outcome_model == "logistic") {
    stats::binomial()
  } else {
    stats::gaussian()
  }
  formula <- stats::as.formula(
    paste(outcome, "~", paste(c(treatment, covariates), collapse = " + "))
  )
  fit <- stats::glm(formula, data = data, family = fam)

  if (.causal_have_stdreg()) {
    res <- tryCatch({
      std <- stdReg::stdGlm(fit = fit, data = data, X = treatment,
                            x = c(0, 1))
      sm <- summary(std, contrast = "difference", reference = 0)
      # sm$est.table is a matrix with rows for each x and cols
      # Estimate / Std. Error / lower / upper.
      tab <- sm$est.table
      ate_idx <- which(rownames(tab) == "1")
      ate <- as.numeric(tab[ate_idx, "Estimate"])
      se  <- as.numeric(tab[ate_idx, "Std. Error"])
      ci  <- .wald_ci(ate, se)
      list(ate = ate, se = se, ci_lower = ci[1], ci_upper = ci[2])
    }, error = function(e) NULL)
    if (!is.null(res)) {
      return(res)
    }
  }

  data1 <- data
  data1[[treatment]] <- 1
  data0 <- data
  data0[[treatment]] <- 0
  mu1 <- as.numeric(stats::predict(fit, newdata = data1, type = "response"))
  mu0 <- as.numeric(stats::predict(fit, newdata = data0, type = "response"))
  diffs <- mu1 - mu0
  ate <- mean(diffs)
  se <- stats::sd(diffs) / sqrt(length(diffs))
  ci <- .wald_ci(ate, se)
  list(ate = ate, se = se, ci_lower = ci[1], ci_upper = ci[2])
}



# ---------------------------------------------------------------------------
# Double Machine Learning -- PLR + IRM
# ---------------------------------------------------------------------------

# Internal: hand-rolled cross-fit ridge fallback when DoubleML R package is
# unavailable. Implements a partially linear regression (PLR) cross-fit on
# residualised outcome and treatment using ridge regression with a fixed
# lambda (lightweight; not for high-precision inference).
#' Internal helper: Dml Xfit Ridge
#' @noRd
.dml_xfit_ridge <- function(X, y, n_folds = 5L, lambda = 1.0,
                            random_state = 42L) {
  n <- nrow(X)
  p <- ncol(X)
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  pred <- numeric(n)
  Xs <- scale(X)
  center <- attr(Xs, "scaled:center")
  scl <- attr(Xs, "scaled:scale")
  scl[scl == 0] <- 1
  Xs[, ] <- sweep(sweep(X, 2, center, "-"), 2, scl, "/")
  for (k in seq_len(n_folds)) {
    te <- which(folds == k)
    tr <- setdiff(seq_len(n), te)
    Xt <- Xs[tr, , drop = FALSE]
    yt <- y[tr]
    yc <- mean(yt)
    A <- crossprod(Xt) + lambda * diag(p)
    b <- crossprod(Xt, yt - yc)
    beta <- tryCatch(solve(A, b), error = function(e) .morie_ginv(A) %*% b)
    pred[te] <- as.numeric(Xs[te, , drop = FALSE] %*% beta) + yc
  }
  pred
}

#' Internal helper: Dml Prepare Xy
#' @noRd
.dml_prepare_xy <- function(data, treatment, outcome, covariates) {
  frame <- data[, c(treatment, outcome, covariates), drop = FALSE]
  frame <- frame[stats::complete.cases(frame), , drop = FALSE]
  # encode non-numeric covariates as integer codes
  for (cn in covariates) {
    if (!is.numeric(frame[[cn]])) {
      frame[[cn]] <- as.integer(as.factor(frame[[cn]]))
    }
  }
  list(
    frame = frame,
    X = as.matrix(frame[, covariates, drop = FALSE]),
    y = as.numeric(frame[[outcome]]),
    d = as.numeric(frame[[treatment]])
  )
}

#' Estimate ATE via Double Machine Learning (Partially Linear Regression)
#'
#' Native rmorie implementation of Chernozhukov et al. (2018)
#' double/debiased machine learning for the partially linear model:
#' \eqn{Y} and \eqn{D} are residualised on \eqn{X} via K-fold
#' cross-fit GCV-tuned ridge regressions, the target parameter comes
#' from the Neyman-orthogonal score, and \code{n_rep} repetitions are
#' aggregated by DoubleML's median rule. Deterministic given
#' \code{random_state}; no DoubleML/mlr3/ranger at runtime
#' (cross-validated against DoubleML in the package's cross tests).
#'
#' @param data A data frame with treatment, outcome, and covariate
#'   columns.
#' @param outcome Name of the continuous outcome column.
#' @param treatment Name of the (binary) treatment column.
#' @param covariates Character vector of covariate column names.
#' @param n_folds Number of cross-fitting folds (default 5).
#' @param n_rep Number of repeated cross-fitting repetitions,
#'   aggregated by the median rule. Default 1.
#' @param random_state Integer seed for cross-fit folds and learners
#'   (default 42).
#' @return Named list with elements \code{ate}, \code{se},
#'   \code{ci_lower}, \code{ci_upper}, \code{n}, \code{method}.
#' @examples
#' set.seed(1)
#' n <- 200
#' X <- matrix(rnorm(n * 3), n, 3)
#' d <- rbinom(n, 1, plogis(X[, 1]))
#' y <- 0.5 * d + X[, 1] + rnorm(n)
#' df <- data.frame(y = y, d = d, x1 = X[, 1], x2 = X[, 2], x3 = X[, 3])
#' morie_estimate_double_ml(df, "y", "d", c("x1", "x2", "x3"))
#' @references
#' Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E.,
#' Hansen, C., Newey, W., & Robins, J. (2018). Double/debiased
#' machine learning for treatment and structural parameters.
#' \emph{The Econometrics Journal}, 21(1), C1--C68.
#' @export
morie_estimate_double_ml <- function(data, outcome, treatment, covariates,
                                     n_folds = 5L, n_rep = 1L,
                                     random_state = 42L) {
  prep <- .dml_prepare_xy(data, treatment, outcome, covariates)
  n <- nrow(prep$frame)
  z <- 1.959964
  out <- .morie_dml_plr_native(prep$X, prep$y, prep$d,
                               n_folds = n_folds, n_rep = n_rep,
                               random_state = random_state)
  list(
    ate = out$theta, se = out$se,
    ci_lower = out$theta - z * out$se,
    ci_upper = out$theta + z * out$se,
    n = n, method = "PLR (morie native)"
  )
}

# morie_estimate_irm lives in R/irm.R (single definition).


# Helper: closed-form ridge fit on (X_tr, y_tr) and predict at X_te.
#' Internal helper: Dml Xfit Ridge Predict
#' @noRd
.dml_xfit_ridge_predict <- function(X_tr, y_tr, X_te, lambda = 1.0) {
  p <- ncol(X_tr)
  ctr <- colMeans(X_tr)
  scl <- apply(X_tr, 2, stats::sd)
  scl[scl == 0 | !is.finite(scl)] <- 1
  Xs_tr <- sweep(sweep(X_tr, 2, ctr, "-"), 2, scl, "/")
  Xs_te <- sweep(sweep(X_te, 2, ctr, "-"), 2, scl, "/")
  yc <- mean(y_tr)
  A <- crossprod(Xs_tr) + lambda * diag(p)
  b <- crossprod(Xs_tr, y_tr - yc)
  beta <- tryCatch(solve(A, b),
                   error = function(e) .morie_ginv(A) %*% b)
  as.numeric(Xs_te %*% beta) + yc
}


# ---------------------------------------------------------------------------
# Phase 1.h new extenders: previously-unmapped CRAN dependencies
# ---------------------------------------------------------------------------

#' Bayesian structural time-series intervention analysis
#'
#' Thin wrapper around \code{CausalImpact::CausalImpact()} (Brodersen
#' et al. 2015). Fits a Bayesian structural time-series counterfactual
#' to a single-series treatment using the pre-intervention window and
#' reports the post-intervention causal effect with credible
#' intervals.
#'
#' Hard-errors if \pkg{CausalImpact} is not installed -- the upstream
#' Kalman-filter + slab-and-spike machinery has no compact inline
#' equivalent. The wrapper is documented as an extender so that
#' downstream rmorie callers have a stable \code{morie_*} entry point
#' to the package.
#'
#' @param data A data frame, matrix, or \code{zoo} object whose first
#'   column is the outcome and remaining columns are concurrent
#'   covariate predictors.
#' @param pre_period Integer length-2 vector giving the start and end
#'   row indices (or time indices for \code{zoo}) of the
#'   pre-intervention window.
#' @param post_period Integer length-2 vector giving the start and end
#'   row indices of the post-intervention window.
#' @param model_args Optional named list passed to
#'   \code{CausalImpact::CausalImpact()}'s \code{model.args} argument
#'   (e.g. \code{list(niter = 1000L)}).
#' @param alpha Posterior credible-interval coverage (default 0.05,
#'   meaning 95 percent intervals).
#' @return Named list with elements \code{average_effect},
#'   \code{cumulative_effect}, \code{ci_lower}, \code{ci_upper},
#'   \code{posterior_prob_causal}, and \code{summary} (the upstream
#'   \code{CausalImpact} summary matrix), plus the original
#'   \code{impact} object.
#' @export
#' @references
#'   Brodersen KH, Gallusser F, Koehler J, Remy N, Scott SL (2015).
#'   Inferring causal impact using Bayesian structural time-series
#'   models. *Annals of Applied Statistics*, 9(1):247-274.
morie_causal_impact <- function(data, pre_period, post_period,
                                model_args = NULL, alpha = 0.05) {
  if (!.causal_have_causalimpact()) {
    stop(
      "morie_causal_impact requires the 'CausalImpact' package. ",
      "Install with install.packages('CausalImpact').",
      call. = FALSE
    )
  }
  ci_args <- list(
    data = data,
    pre.period = pre_period,
    post.period = post_period,
    alpha = alpha
  )
  if (!is.null(model_args)) {
    ci_args$model.args <- model_args
  }
  impact <- do.call(CausalImpact::CausalImpact, ci_args)
  smry <- impact$summary
  list(
    average_effect = as.numeric(smry["Average", "AbsEffect"]),
    cumulative_effect = as.numeric(smry["Cumulative", "AbsEffect"]),
    ci_lower = as.numeric(smry["Average", "AbsEffect.lower"]),
    ci_upper = as.numeric(smry["Average", "AbsEffect.upper"]),
    posterior_prob_causal = as.numeric(impact$summary$p[1L]),
    summary = smry,
    impact = impact
  )
}


#' Estimate balancing weights via \pkg{WeightIt}
#'
#' Thin wrapper around \code{WeightIt::weightit()} exposing the full
#' WeightIt method palette (\code{"glm"}, \code{"cbps"},
#' \code{"ebal"}, \code{"ps"}, \code{"energy"}, \code{"optweight"},
#' and any future additions). Provides MORIE callers with a stable
#' \code{morie_*} entry point for balancing weights while preserving
#' the underlying object so callers can pipe into
#' \code{survey::svyglm} or \code{cobalt::bal.tab} downstream.
#'
#' Hard-errors if \pkg{WeightIt} is not installed -- the multi-method
#' weighting machinery has no compact inline equivalent.
#'
#' @param data A data frame.
#' @param treatment Name of the treatment column (binary, multinomial,
#'   or continuous depending on \code{method}).
#' @param covariates Character vector of covariate names.
#' @param method One of \code{"glm"}, \code{"cbps"}, \code{"ebal"},
#'   \code{"ps"}, \code{"energy"}, \code{"optweight"}, or any other
#'   method accepted by \code{WeightIt::weightit()}.
#' @param estimand One of \code{"ATE"}, \code{"ATT"}, \code{"ATC"};
#'   defaults to \code{"ATE"}.
#' @param ... Additional arguments forwarded to
#'   \code{WeightIt::weightit()}.
#' @return Named list with elements \code{weights} (numeric vector),
#'   \code{propensity_scores} (numeric vector or \code{NULL}),
#'   \code{method}, \code{estimand}, \code{ess} (effective sample
#'   size), and \code{weightit} (the original WeightIt object).
#' @export
#' @references
#'   Greifer N (2024). WeightIt: Weighting for Covariate Balance in
#'   Observational Studies. R package version 1.4.0.
morie_causal_weighting <- function(data, treatment, covariates,
                                   method = "glm",
                                   estimand = c("ATE", "ATT", "ATC"),
                                   ...) {
  if (!.causal_have_weightit()) {
    stop(
      "morie_causal_weighting requires the 'WeightIt' package. ",
      "Install with install.packages('WeightIt').",
      call. = FALSE
    )
  }
  estimand <- match.arg(estimand)
  formula <- stats::as.formula(
    paste(treatment, "~", paste(covariates, collapse = " + "))
  )
  w <- WeightIt::weightit(formula, data = data, method = method,
                          estimand = estimand, ...)
  wt <- as.numeric(w$weights)
  ess <- (sum(wt)^2) / sum(wt^2)
  list(
    weights = wt,
    propensity_scores = if (!is.null(w$ps)) as.numeric(w$ps) else NULL,
    method = method,
    estimand = estimand,
    ess = ess,
    weightit = w
  )
}


#' Robust / heteroskedasticity-consistent variance for a fitted model
#'
#' Thin wrapper around \pkg{sandwich} variance estimators. Returns
#' the requested variance-covariance matrix and the corresponding
#' robust standard errors. Supports HC0-HC5 (cross-section), HAC
#' (time-series), and clustered (one-way) variance.
#'
#' Hard-errors if \pkg{sandwich} is not installed -- the HC sandwich
#' algebra is well-tested upstream and re-implementing it inline
#' would be both lengthy and error-prone.
#'
#' @param model A fitted model object (typically from
#'   \code{stats::lm} or \code{stats::glm}) compatible with
#'   \code{sandwich::vcovHC()}, \code{sandwich::vcovHAC()}, or
#'   \code{sandwich::vcovCL()}.
#' @param type One of \code{"HC0"}, \code{"HC1"}, \code{"HC2"},
#'   \code{"HC3"}, \code{"HC4"}, \code{"HC4m"}, \code{"HC5"},
#'   \code{"HAC"}, or \code{"CL"} (clustered). Default
#'   \code{"HC3"} (Long-Ervin small-sample default).
#' @param cluster Optional one-sided formula or vector identifying
#'   the cluster variable (required when \code{type = "CL"}).
#' @param ... Additional arguments forwarded to the chosen sandwich
#'   estimator.
#' @return Named list with elements \code{vcov} (variance matrix),
#'   \code{se} (named numeric vector of robust SEs), \code{type},
#'   and \code{n_coef}.
#' @srrstats {G3.1} The variance/covariance estimator is user-selectable
#'   via \code{type} (HC0-HC5 heteroskedasticity-consistent, HAC, or CL
#'   clustered), so covariance is never computed solely by
#'   \code{stats::cov}; the same choice is exposed by the did/dml
#'   cluster-robust paths.
#' @srrstats {G3.1a} The available covariance methods are documented on
#'   the \code{type} parameter above and demonstrated in the examples.
#' @export
#' @references
#'   Zeileis A, Koll S, Graham N (2020). Various Versatile Variances:
#'   An Object-Oriented Implementation of Clustered Covariances in R.
#'   \emph{Journal of Statistical Software}, 95(1), 1-36.
morie_causal_robust_se <- function(model,
                                   type = "HC3",
                                   cluster = NULL,
                                   ...) {
  v <- morie_vcov_robust(model, type = type, cluster = cluster, ...)
  se <- sqrt(diag(v))
  list(
    vcov = v,
    se = se,
    type = type,
    n_coef = length(se)
  )
}


# causalweight was archived from CRAN on 2026-05-18 because its
# dependency LARF was also archived. The morie_causal_mediation
# wrapper that previously lived here was dropped because the upstream
# package is no longer available via install.packages(). If
# causalweight (and LARF) return to CRAN, restore this wrapper from
# git history (commit 4d78188).


#' Hajek estimator for a population mean
#'
#' The ratio (Hajek) estimator
#' \eqn{\bar{y}_H = \sum_i w_i y_i / \sum_i w_i}, which normalises
#' the Horvitz-Thompson estimator by the ESTIMATED population size and
#' is therefore insensitive to the overall scale of the weights.  The
#' standard error uses Taylor (delta-method) linearisation under the
#' with-replacement approximation.
#'
#' Mirrors Python \code{morie.survey.hajek_mean}; this is the SURVEY
#' Hajek mean, a different estimator from the Hajek IPW average
#' treatment effect in \code{\link{morie_estimate_ate}}, which is a
#' DIFFERENCE of two such weighted means.
#'
#' @param y Response values.
#' @param weights Survey weights \eqn{w_i = 1/\pi_i}, all positive.
#' @return A list with \code{mean}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}.
#' @references Hajek, J. (1971). Comment in Godambe and Sprott (Eds.),
#'   Foundations of Statistical Inference. Holt, Rinehart and Winston;
#'   Cochran, W. G. (1977). Sampling Techniques, 3rd ed., Sec. 6.13.
#' @export
# NOTE: this takes sampling WEIGHTS. The exported
# morie_hajek_mean takes inclusion PROBABILITIES pi, matching
# the Python arm hjkest.hajek_estimator(y, pi) -- and weights
# are the reciprocals of those, so the two are not
# interchangeable. Both were defined as morie_hajek_mean and
# survey_native.R sorts later, so this one never ran; it is
# scoped rather than left as a name that silently loses.
#' .causal_hajek_weighted_mean
#'
#' Internal helper in causal.R; see the file header for
#' the source the module follows.
#'
#' @param y A vector; its length is taken.
#' @param weights The body requires: All weights must be > 0.
#' @return A list with \code{mean}, \code{se}, \code{ci_lower}, \code{ci_upper}.
#' @export
.causal_hajek_weighted_mean <- function(y, weights) {
  y <- as.numeric(y); w <- as.numeric(weights)
  if (length(y) != length(w))
    stop(sprintf("y and weights must have the same length; got %d and %d.",
                 length(y), length(w)))
  if (any(w <= 0)) stop("All weights must be > 0.")
  if (length(y) < 2L)
    stop("At least 2 observations are required for SE computation.")
  sum_w <- sum(w)
  m <- sum(w * y) / sum_w
  se <- sqrt(max(0, sum(w^2 * (y - m)^2) / sum_w^2))
  z <- stats::qnorm(0.975)
  list(mean = m, se = se, ci_lower = m - z * se, ci_upper = m + z * se)
}
