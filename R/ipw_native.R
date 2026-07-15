# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native design-based weighted GLM (feat/native-specializations,
# module 8). Replaces survey::svydesign(ids = ~1) + survey::svyglm for
# the independent-sampling case: coefficients come from a weighted
# stats::glm fit; variance is the Horvitz-Thompson linearization —
# a sandwich over the centred weighted score contributions with the
# survey package's n/(n-1) small-sample factor — so results reproduce
# svyglm to numerical precision (cross-validated in tests/cross/).

#' Internal helper: svyglm-equivalent weighted GLM with linearized SEs
#' @srrstats {G1.0} Primary reference: Binder (1983, Int. Stat. Rev.
#'   51) — design-based variance for GLM parameter estimates via
#'   Taylor linearization; Lumley (2004, JSS 9(1)) for the reference
#'   implementation (survey) this is cross-validated against.
#' @srrstats {G3.1} The variance estimator (centred score sandwich,
#'   n/(n-1) factor, inverse expected information bread) is stated
#'   here and asserted equal to survey::svyglm in tests/cross/.
#' @noRd
.morie_svyglm_native <- function(formula, data, weights,
                                 family = stats::gaussian()) {
  # prior weights enter through the environment so glm() treats them
  # as sampling weights (same as svyglm's internal call)
  env <- new.env(parent = environment(formula))
  assign(".morie_w", weights, envir = env)
  environment(formula) <- env
  fit <- eval(bquote(stats::glm(.(formula), data = .(quote(data)),
                                weights = .morie_w,
                                family = .(family))),
              list(data = data), env)
  X <- stats::model.matrix(fit)
  mu <- stats::fitted(fit)
  y <- fit$y
  w <- weights
  n <- nrow(X)
  p <- ncol(X)
  # working score contributions u_i = w_i (y_i - mu_i) x_i for the
  # canonical links used here (identity / logit); general case uses
  # the IRLS working residuals
  vmu <- fit$family$variance(mu)
  eta_mu <- fit$family$mu.eta(stats::predict(fit, type = "link"))
  r_work <- (y - mu) / vmu * eta_mu
  U <- X * (w * r_work)
  # bread: inverse expected information of the weighted fit
  B <- chol2inv(chol(crossprod(X, X * (w * eta_mu^2 / vmu))))
  Uc <- sweep(U, 2L, colMeans(U))
  meat <- crossprod(Uc) * n / (n - 1)
  V <- B %*% meat %*% B
  se <- sqrt(diag(V))
  cf <- stats::coef(fit)
  df_resid <- n - p
  tval <- cf / se
  pval <- 2 * stats::pt(-abs(tval), df = df_resid)
  ci <- cbind(cf - stats::qt(0.975, df_resid) * se,
              cf + stats::qt(0.975, df_resid) * se)
  colnames(ci) <- c("2.5 %", "97.5 %")
  list(
    coefficients = cbind(Estimate = cf, `Std. Error` = se,
                         `t value` = tval, `Pr(>|t|)` = pval),
    confint = ci,
    vcov = V,
    fit = fit
  )
}
