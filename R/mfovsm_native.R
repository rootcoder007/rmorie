# morie.fn -- function file (rootcoder007/morie)
# Marginal structural model with effect modification by a baseline feature.
#
# Robins & Hernan (2009) Sec. 4 gives the marginal structural model for a
# time-varying treatment conditional on a baseline covariate V, which
# Hernan & Robins (2020) Sec. 12.5 states as
#
#   E[Y^bar_a | V] = beta_0 + beta_1 * bar_a + beta_2 * V + beta_3 * bar_a * V
#
# so that beta_3 is the effect modification: how much the causal effect of
# exposure differs across levels of V. The distinction the sources insist on,
# and the reason this is not the same as adding V to an outcome regression, is
# that V must be a baseline feature. It is measured before treatment, so
# conditioning on it cannot induce collider bias, and the model still refers to
# counterfactual means rather than to observed conditional means.
#
# Two things follow, and both are implemented rather than assumed.
#
# V belongs in the weight numerator. Sec. 12.5: when the MSM conditions on V,
# the stabilized weights use f(A | V) / f(A | L) rather than f(A) / f(A | L).
# Putting V in the numerator makes the weights smaller and the estimate more
# precise, and leaves the estimand unchanged. v_in_numerator=FALSE is available
# for comparison and the anchor checks that the two agree on the effect while
# differing on the variance -- which is exactly the claim.
#
# V must not be post-treatment. There is no way to verify that from the data,
# so it is stated in the docstring and the argument is named `feature` rather
# than `covariate` to keep it distinct from the time-varying H that goes in
# the weight denominator.
#
# References
# ----------
# Robins, J. M. & Hernan, M. A. (2009) "Estimation of the causal effects
# of time-varying exposures", in Fitzmaurice, G., Davidian, M.,
# Verbeke, G. & Molenberghs, G. (eds.), Longitudinal Data Analysis,
# Chapman & Hall/CRC Handbooks of Modern Statistical Methods, 553-599,
# doi:10.1201/9781420011579.ch23.
#
# Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
# Chapman & Hall/CRC, Sec. 12.5 (effect modification and marginal
# structural models) and Sec. 21.2 (the time-varying weights).

# Private helpers (prefixed .mfovsm_)

.mfovsm_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.list(x)) {
    return(as.numeric(unlist(x)))
  }
  as.numeric(x)
}

.mfovsm_mat <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.matrix(x)) return(x)
  if (is.list(x)) {
    return(do.call(cbind, lapply(x, as.numeric)))
  }
  matrix(as.numeric(x), ncol = 1)
}

.mfovsm_hist <- function(obj, allow_one = FALSE) {
  if (is.null(obj)) {
    if (allow_one) return(list(NULL))
    return(list())
  }
  if (is.list(obj)) return(obj)
  return(list(obj))
}

.mfovsm_quantile7 <- function(x, q) {
  x <- sort(as.numeric(x))
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (n == 1) return(x[1])
  h <- (n - 1) * q
  lo <- max(1, min(n, floor(h) + 1L))
  hi <- max(1, min(n, ceiling(h) + 1L))
  if (lo == hi) return(x[lo])
  frac <- h - floor(h)
  x[lo] + frac * (x[hi] - x[lo])
}

.mfovsm_logreg_fit <- function(y, X, max_iter = 25, tol = 1e-8) {
  n <- length(y)
  p <- ncol(X)
  beta <- rep(0, p)
  for (iter in seq_len(max_iter)) {
    eta <- as.numeric(X %*% beta)
    mu <- ifelse(eta > 0,
                 1 / (1 + exp(-eta)),
                 exp(eta) / (1 + exp(eta)))
    mu <- pmin(pmax(mu, 1e-12), 1 - 1e-12)
    W_vec <- mu * (1 - mu)
    z <- eta + (y - mu) / W_vec
    XtWX <- crossprod(X, W_vec * X)
    XtWz <- crossprod(X, W_vec * z)
    beta_new <- solve(XtWX, XtWz)
    if (max(abs(beta_new - beta)) < tol) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }
  eta <- as.numeric(X %*% beta)
  mu <- ifelse(eta > 0,
               1 / (1 + exp(-eta)),
               exp(eta) / (1 + exp(eta)))
  list(coef = as.numeric(beta), fitted = mu)
}

.mfovsm_ip_weights <- function(ak, den, num, kind = "binary", stabilize = TRUE) {
  n <- length(ak)

  if (is.null(den) && is.null(num)) {
    return(list(weights = rep(1.0, n), fitted = rep(NA_real_, n)))
  }

  if (is.null(den)) {
    pden <- rep(0.5, n)
  } else {
    Xden <- cbind(1, den)
    den_fit <- .mfovsm_logreg_fit(ak, Xden)
    pden <- den_fit$fitted
    pden <- pmin(pmax(pden, 1e-12), 1 - 1e-12)
  }

  if (is.null(num)) {
    if (stabilize) {
      pnum <- rep(mean(ak), n)
    } else {
      pnum <- rep(0.5, n)
    }
    pnum <- pmin(pmax(pnum, 1e-12), 1 - 1e-12)
  } else {
    Xnum <- cbind(1, num)
    num_fit <- .mfovsm_logreg_fit(ak, Xnum)
    pnum <- num_fit$fitted
    pnum <- pmin(pmax(pnum, 1e-12), 1 - 1e-12)
  }

  w <- ifelse(ak == 1, pnum / pden, (1 - pnum) / (1 - pden))
  list(weights = w, fitted = pden)
}

.mfovsm_wls <- function(X, y, w) {
  X <- cbind(1, X)
  n <- length(y)
  p <- ncol(X)

  XtWX <- crossprod(X, w * X)
  XtWy <- crossprod(X, w * y)
  beta <- solve(XtWX, XtWy)

  residuals <- y - as.numeric(X %*% beta)
  df <- n - p
  if (df > 0) {
    sigma2 <- sum(w * residuals^2) / df
  } else {
    sigma2 <- 0
  }
  if (!is.finite(sigma2) || sigma2 < 0) sigma2 <- 0

  vcov <- sigma2 * solve(XtWX)
  se <- sqrt(diag(vcov))

  list(coef = as.numeric(beta), se = se, vcov = vcov)
}

# Main entry point

#' morie_mfovsm
#'
#' Part of the mfovsm_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param feature See Usage.
#' @param A See Usage.
#' @param H See Usage.
#' @param v_in_numerator Defaults to \code{TRUE}.
#' @param contrast Defaults to \code{"cumulative"}.
#' @param trim Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{se}, \code{main_effect}, \code{main_effect_se}, \code{feature_effect}, \code{intercept}, \code{coef}, \code{vcov}, \code{weights}, \code{mean_weight}, \code{max_weight}, \code{effective_sample_size}, \code{exposure}, \code{v_in_numerator}, \code{n}, \code{n_times}, \code{contrast}, \code{method}.
#' @export
morie_mfovsm <- function(y, feature, A, H, v_in_numerator = TRUE,
                          contrast = "cumulative", trim = NULL) {
  A_hist <- .mfovsm_hist(A)
  L_hist <- .mfovsm_hist(H, allow_one = TRUE)

  if (length(L_hist) != length(A_hist)) {
    stop(sprintf("mfo_vsm: %d treatment times but %d covariate blocks",
                 length(A_hist), length(L_hist)))
  }

  yv <- .mfovsm_vec(y)
  vv <- .mfovsm_vec(feature)
  n <- length(yv)

  if (length(vv) != n) {
    stop(sprintf("mfo_vsm: outcome has %d rows but the feature has %d",
                 n, length(vv)))
  }

  # Sec. 21.2's product, with V added to the numerator model when
  # asked -- Sec. 12.5's refinement for a V-conditional MSM.
  w <- rep(1.0, n)
  lbar <- list()
  past <- list()

  for (kk in seq_along(A_hist)) {
    ak <- .mfovsm_vec(A_hist[[kk]])

    if (!is.null(L_hist[[kk]])) {
      block <- .mfovsm_mat(L_hist[[kk]])
      if (!is.null(block) && ncol(block) > 0) {
        for (c in seq_len(ncol(block))) {
          lbar[[length(lbar) + 1L]] <- block[, c]
        }
      }
    }

    den_cols <- c(lbar, past)
    if (length(den_cols) > 0) {
      den <- do.call(cbind, den_cols)
    } else {
      den <- NULL
    }

    num_cols <- list()
    if (isTRUE(v_in_numerator)) num_cols[[1L]] <- vv
    num_cols <- c(num_cols, past)
    if (length(num_cols) > 0) {
      num <- do.call(cbind, num_cols)
    } else {
      num <- NULL
    }

    res_w <- .mfovsm_ip_weights(ak, den, num, kind = "binary", stabilize = TRUE)
    w <- w * res_w$weights

    past[[length(past) + 1L]] <- ak
  }

  if (!is.null(trim)) {
    q <- as.numeric(trim)
    if (!(q > 0.5 && q < 1.0)) {
      stop("mfo_vsm: trim must be in (0.5, 1)")
    }
    hi <- .mfovsm_quantile7(w, q)
    lo <- .mfovsm_quantile7(w, 1.0 - q)
    w <- pmin(pmax(w, lo), hi)
  }

  # Compute exposure contrast
  cum <- rep(0, n)
  for (a in A_hist) {
    cum <- cum + .mfovsm_vec(a)
  }

  if (contrast == "cumulative") {
    e <- cum
  } else if (contrast == "final") {
    e <- .mfovsm_vec(A_hist[[length(A_hist)]])
  } else if (contrast == "everexposed") {
    e <- as.numeric(cum > 0)
  } else {
    stop(sprintf("mfo_vsm: contrast must be 'cumulative', 'final' or 'everexposed', got %s",
                 contrast))
  }

  # Design matrix: [e, v, e*v] (intercept added by WLS)
  X <- cbind(e, vv, e * vv)

  # Fit WLS
  fit <- .mfovsm_wls(X, yv, w)

  s1 <- sum(w)
  s2 <- sum(w * w)

  list(
    estimate = fit$coef[4],
    se = fit$se[4],
    main_effect = fit$coef[2],
    main_effect_se = fit$se[2],
    feature_effect = fit$coef[3],
    intercept = fit$coef[1],
    coef = fit$coef,
    vcov = fit$vcov,
    weights = w,
    mean_weight = s1 / n,
    max_weight = max(w),
    effective_sample_size = if (s2 > 0) s1 * s1 / s2 else 0.0,
    exposure = e,
    v_in_numerator = as.logical(v_in_numerator),
    n = n,
    n_times = length(A_hist),
    contrast = contrast,
    method = "V-conditional marginal structural model, Robins & Hernan (2009); Hernan & Robins (2020) Sec. 12.5"
  )
}

# Compact alias per ledger/NAMING.md
mfovsm_ <- morie_mfovsm

# Public names resolved by fn/_lazy_map.json
mfovsm <- morie_mfovsm

# Cheatsheet
#' Cheatsheet
#'
#' Part of the mfovsm_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_mfovsm_cheatsheet <- function() {
  paste0("mfovsm: V-conditional MSM E[Y^abar|V] = b0 + b1 abar + ",
         "b2 V + b3 abar V (Robins-Hernan 2009; H&R Sec.12.5). ",
         "estimate = b3, the effect modification. V goes in the ",
         "weight NUMERATOR: f(A|V)/f(A|L).")
}
