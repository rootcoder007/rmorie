# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native design-based weighted GLM (feat/native-specializations,
# module 8). Replaces survey::svydesign(ids = ~1) + survey::svyglm for
# the independent-sampling case: coefficients come from a weighted
# stats::glm fit; variance is the Horvitz-Thompson linearization --
# a sandwich over the centred weighted score contributions with the
# survey package's n/(n-1) small-sample factor -- so results reproduce
# svyglm to numerical precision (cross-validated in tests/cross/).

#' Internal helper: svyglm-equivalent weighted GLM with linearized SEs
#' @srrstats {G1.0} Primary reference: Binder (1983, Int. Stat. Rev.
#'   51) -- design-based variance for GLM parameter estimates via
#'   Taylor linearization; Lumley (2004, JSS 9(1)) for the reference
#'   implementation (survey) this is cross-validated against.
#' @srrstats {G3.1} The variance estimator (centred score sandwich,
#'   n/(n-1) factor, inverse expected information bread) is stated
#'   here and asserted equal to survey::svyglm in tests/cross/.
#' @noRd
.morie_svyglm_native <- function(formula, data, weights,
                                 family = stats::gaussian(), design = NULL) {
  # prior weights enter through the environment so glm() treats them
  # as sampling weights (same as svyglm's internal call)
  env <- new.env(parent = environment(formula))
  # svyglm rescales the sampling weights to mean 1 before its IRLS:
  # the weighted MLE is invariant to the scale, but glm()'s binomial
  # start point (w y + 1/2) / (w + 1) sits at the boundary for raw
  # weights in the hundreds and the iterations run off from there
  w <- as.numeric(weights)
  w <- w / mean(w)
  assign(".morie_w", w, envir = env)
  # sampling weights make the binomial / Poisson "successes" non-integer: glm() warns on every fit.
  # The quasi families give the same IRLS (same estimates, same sandwich) without that warning,
  # which is what survey::svyglm users are told to pass.
  if (identical(family$family, "binomial")) family <- stats::quasibinomial(link = family$link)
  if (identical(family$family, "poisson")) family <- stats::quasipoisson(link = family$link)
  environment(formula) <- env
  fit <- eval(bquote(stats::glm(.(formula), data = .(quote(data)),
                                weights = .morie_w,
                                family = .(family))),
              list(data = data), env)
  X <- stats::model.matrix(fit)
  mu <- stats::fitted(fit)
  y <- fit$y
  # the weights of the rows the fit kept: rows with a missing covariate leave the model
  # frame, and the full-length vector recycled over the shorter one (wrong standard errors)
  w <- as.numeric(stats::weights(fit, type = "prior"))
  n <- nrow(X)
  p <- ncol(X)
  # working score contributions u_i = w_i (y_i - mu_i) x_i for the
  # canonical links used here (identity / logit); general case uses
  # the IRLS working residuals
  vmu <- fit$family$variance(mu)
  eta_mu <- fit$family$mu.eta(stats::predict(fit, type = "link"))
  r_work <- (y - mu) / vmu * eta_mu
  U <- X * (w * r_work)
  # a row the fit dropped (a missing value) stays in the sample with a zero score: survey keeps
  # the stratum's PSU count of the full design and pads the missing PSU totals with zeros
  n_all <- NROW(data)
  naa <- stats::na.action(fit)
  kept <- if (is.null(naa)) seq_len(n_all) else seq_len(n_all)[-as.integer(naa)]
  Ufull <- matrix(0, n_all, p)
  Ufull[kept, ] <- U
  if (is.null(design)) {
    design <- list(strata = rep("1", n_all), cluster = as.character(seq_len(n_all)),
                   n_psu = rep(n_all, n_all), popsize = NULL)
  }
  # bread: inverse expected information of the weighted fit
  B <- chol2inv(chol(crossprod(X, X * (w * eta_mu^2 / vmu))))
  meat <- .morie_svy_recvar(Ufull, design)
  V <- B %*% meat %*% B
  se <- sqrt(diag(V))
  cf <- stats::coef(fit)
  # survey's degrees of freedom: PSUs minus strata among the rows in the fit, minus the
  # coefficients beyond the intercept
  inset <- kept[as.numeric(weights)[kept] != 0]
  df_resid <- length(unique(design$cluster[inset])) - length(unique(design$strata[inset])) + 1 - p
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
