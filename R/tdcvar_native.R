# morie.fn -- function file (rootcoder007/morie)
# Time-dependent covariate adjustment.
#
# Hernan & Robins (2020) Ch. 20 and Sec. 21.2. The chapter's point is a
# negative one, and it is what this module is built around: when a
# time-varying covariate L_k both confounds later treatment and
# is itself affected by earlier treatment, no conditioning strategy
# works. Adjust for it and you block the part of the effect that runs
# through it; leave it out and the later treatment stays confounded. The
# covariate is a confounder and a mediator at once.
#
# Weighting escapes the trap. The IP weights of Sec. 21.2,
#
#   SW^Abar = prod_k f(A_k | Abar_{k-1}) / f(A_k | Abar_{k-1}, Lbar_k),
#
# create a pseudo-population in which Lbar no longer predicts
# treatment, so a marginal structural model fitted there estimates the
# effect of the whole treatment history without ever conditioning on L.
#
# So this function does not return one number. It returns three, and the
# comparison is the analysis:
#
#   msm         The IP-weighted marginal structural model. The estimate.
#   adjusted    The same outcome model with Lbar entered as regressors --
#               the naive fix, biased through over-adjustment.
#   unadjusted  No adjustment at all, biased through confounding.
#
# Ch. 20 predicts the two naive estimators fall on opposite sides of
# the truth when the treatment effect and the confounding run the same
# way, and the anchor checks exactly that. Reporting only the first
# number would hide the finding the chapter exists to make.
#
# References
# ----------
# Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
# Raton: Chapman & Hall/CRC. Ch. 20 (treatment-confounder feedback),
# Sec. 21.2 (IP weighting for time-varying treatments), Sec. 12.3 (the
# stabilized weights and the mean-1 diagnostic).
#
# Robins, J. (1986) "A new approach to causal inference in mortality
# studies with a sustained exposure period", Mathematical Modelling
# 7(9-12), 1393-1512, doi:10.1016/0270-0255(86)90088-6 -- where the
# problem and the weighted solution were first set out.

#' .tdcvar_vec
#'
#' A step of the tdcvar_native implementation. Called by
#' \code{.tdcvar_ip_weights_history}, \code{morie_tdcvar}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tdcvar_vec(x = x)
#' res
.tdcvar_vec <- function(x) {
  as.numeric(x)
}

#' .tdcvar_mat
#'
#' A step of the tdcvar_native implementation. Called by
#' \code{.tdcvar_ip_weights_history}, \code{morie_tdcvar}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return The value of \code{M}, as built in the body.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tdcvar_mat(X = x)
#' res
.tdcvar_mat <- function(X) {
  if (is.null(X)) {
    return(matrix(0, nrow = 0, ncol = 0))
  }
  if (is.matrix(X)) {
    storage.mode(X) <- "double"
    return(X)
  }
  if (is.numeric(X)) {
    return(matrix(X, ncol = 1L))
  }
  n <- length(X)
  if (n == 0L) {
    return(matrix(0, nrow = 0, ncol = 0))
  }
  p <- length(X[[1]])
  M <- matrix(0, nrow = n, ncol = p)
  for (i in seq_len(n)) {
    M[i, ] <- as.numeric(X[[i]])
  }
  M
}

#' .tdcvar_wls
#'
#' A step of the tdcvar_native implementation. Called by \code{morie_tdcvar}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; combined arithmetically in the body.
#' @param w Numeric; combined arithmetically in the body.
#' @return A list with \code{coef}, \code{se}, \code{vcov}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .tdcvar_wls(X = x, y = y, w = x)
#' res
.tdcvar_wls <- function(X, y, w) {
  n <- nrow(X)
  p <- ncol(X)
  WX <- X * w
  XtWX <- crossprod(X, WX)
  XtWy <- crossprod(X, w * y)
  beta <- solve(XtWX, XtWy)
  resid <- y - X %*% beta
  sigma2 <- sum(w * resid^2) / (n - p)
  vcov <- sigma2 * solve(XtWX)
  se <- sqrt(diag(vcov))
  list(coef = as.numeric(beta), se = as.numeric(se), vcov = vcov)
}

#' .tdcvar_logreg
#'
#' A step of the tdcvar_native implementation. Called by \code{.tdcvar_ip_weights_history}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; combined arithmetically in the body.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{25L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-08}.
#' @return The value of \code{beta}, as built in the body.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .tdcvar_logreg(X = X, y = y)
#' res
.tdcvar_logreg <- function(X, y, max_iter = 25L, tol = 1e-8) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)
  for (iter in seq_len(max_iter)) {
    eta <- as.numeric(X %*% beta)
    eta <- pmin(pmax(eta, -30), 30)
    mu <- 1 / (1 + exp(-eta))
    mu <- pmin(pmax(mu, 1e-10), 1 - 1e-10)
    Wv <- mu * (1 - mu)
    z <- eta + (y - mu) / Wv
    XtWX <- crossprod(X, X * Wv)
    XtWz <- crossprod(X, Wv * z)
    beta_new <- solve(XtWX, XtWz)
    if (max(abs(beta_new - beta)) < tol) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }
  beta
}

#' .tdcvar_logreg_pred
#'
#' A step of the tdcvar_native implementation. Called by \code{.tdcvar_ip_weights_history}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{\%*\%}.
#' @param beta A matrix; passed to \code{\%*\%}.
#' @return The value of \code{pmin}.
#' @export
.tdcvar_logreg_pred <- function(X, beta) {
  eta <- as.numeric(X %*% beta)
  eta <- pmin(pmax(eta, -30), 30)
  p <- 1 / (1 + exp(-eta))
  pmin(pmax(p, 1e-10), 1 - 1e-10)
}

#' .tdcvar_ip_weights_history
#'
#' A step of the tdcvar_native implementation. Called by \code{morie_tdcvar}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A_hist A vector; its length is taken and its elements indexed.
#' @param L_hist A vector; indexed elementwise.
#' @param kind Accepted by the signature and not used anywhere in the body. Defaults to
#' \code{"binary"}.
#' @param stabilize A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param trim Optional; may be \code{NULL}. A vector; its length is taken.
#' @return A list with \code{weights}, \code{per_time}.
#' @export
.tdcvar_ip_weights_history <- function(A_hist, L_hist, kind = "binary",
                                       stabilize = TRUE, trim = NULL) {
  K <- length(A_hist)
  n <- length(A_hist[[1]])

  A_mat <- matrix(0, nrow = n, ncol = K)
  for (k in seq_len(K)) {
    A_mat[, k] <- .tdcvar_vec(A_hist[[k]])
  }

  per_time <- list()
  cum_w <- rep(1, n)

  for (k in seq_len(K)) {
    a_k <- A_mat[, k]

    if (k == 1L) {
      X_num <- matrix(1, nrow = n, ncol = 1L)
    } else {
      X_num <- cbind(1, A_mat[, seq_len(k - 1L), drop = FALSE])
    }

    L_parts <- list()
    for (kk in seq_len(k)) {
      if (is.null(L_hist[[kk]])) next
      L_block <- .tdcvar_mat(L_hist[[kk]])
      if (ncol(L_block) > 0L) {
        L_parts[[length(L_parts) + 1L]] <- L_block
      }
    }
    if (length(L_parts) > 0L) {
      L_combined <- do.call(cbind, L_parts)
      X_den <- cbind(X_num, L_combined)
    } else {
      X_den <- X_num
    }

    beta_num <- .tdcvar_logreg(X_num, a_k)
    p_num <- .tdcvar_logreg_pred(X_num, beta_num)

    beta_den <- .tdcvar_logreg(X_den, a_k)
    p_den <- .tdcvar_logreg_pred(X_den, beta_den)

    if (isTRUE(stabilize)) {
      w_k <- p_num / p_den
    } else {
      w_k <- 1 / p_den
    }

    cum_w <- cum_w * w_k

    per_time[[k]] <- list(time = (k - 1L), weight = w_k)
  }

  if (!is.null(trim) && is.numeric(trim) && length(trim) > 0L &&
    trim > 0 && trim < 0.5) {
    q_low <- as.numeric(quantile(cum_w, probs = trim, na.rm = TRUE))
    q_high <- as.numeric(quantile(cum_w, probs = 1 - trim, na.rm = TRUE))
    cum_w <- pmin(pmax(cum_w, q_low), q_high)
  }

  list(weights = cum_w, per_time = per_time)
}

#' morie_tdcvar
#'
#' A step of the tdcvar_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{.tdcvar_vec}.
#' @param A A list; the body checks with \code{is.list}.
#' @param L_t A list; the body checks with \code{is.list}.
#' @param time Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @param contrast One of \code{"cumulative"}, \code{"everexposed"}, \code{"final"}.
#' Defaults to \code{"cumulative"}.
#' @param kind Passed to \code{.tdcvar_ip_weights_history}. Defaults to \code{"binary"}.
#' @param stabilize Passed to \code{.tdcvar_ip_weights_history}. Defaults to \code{TRUE}.
#' @param trim Passed to \code{.tdcvar_ip_weights_history}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_tdcvar <- function(y, A, L_t, time = NULL, contrast = "cumulative",
                         kind = "binary", stabilize = TRUE, trim = NULL) {
  if (!contrast %in% c("cumulative", "final", "everexposed")) {
    stop(
      "time_dep_covariate: contrast must be one of c('cumulative', 'final', 'everexposed'), got ",
      contrast
    )
  }

  if (is.list(A)) {
    A_hist <- A
  } else {
    A_hist <- list(A)
  }

  if (is.list(L_t)) {
    L_hist <- L_t
  } else {
    L_hist <- list(L_t)
  }

  K <- length(A_hist)
  if (K == 0L) {
    stop("time_dep_covariate: need at least one time point")
  }
  if (length(L_hist) != K) {
    stop(
      "time_dep_covariate: ", K, " treatment times but ",
      length(L_hist), " covariate blocks; Sec. 21.2 needs L-bar_k at every k"
    )
  }

  yv <- .tdcvar_vec(y)
  n <- length(yv)

  for (kk in seq_len(K)) {
    if (length(.tdcvar_vec(A_hist[[kk]])) != n) {
      stop(
        "time_dep_covariate: outcome has ", n, " rows but treatment at time ",
        kk, " has ", length(.tdcvar_vec(A_hist[[kk]]))
      )
    }
  }

  ipw <- .tdcvar_ip_weights_history(A_hist, L_hist,
    kind = kind,
    stabilize = stabilize, trim = trim
  )
  w <- ipw$weights
  per_time <- ipw$per_time

  cum <- rep(0, n)
  for (kk in seq_len(K)) {
    cum <- cum + .tdcvar_vec(A_hist[[kk]])
  }

  if (contrast == "cumulative") {
    expo <- cum
  } else if (contrast == "final") {
    expo <- .tdcvar_vec(A_hist[[K]])
  } else {
    expo <- ifelse(cum > 0, 1.0, 0.0)
  }

  X <- cbind(1, expo)

  msm <- .tdcvar_wls(X, yv, w)
  unadj <- .tdcvar_wls(X, yv, rep(1.0, n))

  Lcols <- list()
  for (kk in seq_len(K)) {
    if (is.null(L_hist[[kk]])) next
    block <- .tdcvar_mat(L_hist[[kk]])
    if (ncol(block) > 0L) {
      for (c in seq_len(ncol(block))) {
        Lcols[[length(Lcols) + 1L]] <- block[, c]
      }
    }
  }

  if (length(Lcols) > 0L) {
    L_mat <- do.call(cbind, Lcols)
    Xadj <- cbind(1, expo, L_mat)
  } else {
    Xadj <- X
  }

  adj <- .tdcvar_wls(Xadj, yv, rep(1.0, n))

  s1 <- sum(w)
  s2 <- sum(w * w)

  result <- list(
    estimate = msm$coef[2],
    se = msm$se[2],
    msm = msm$coef[2],
    msm_se = msm$se[2],
    adjusted = adj$coef[2],
    adjusted_se = adj$se[2],
    unadjusted = unadj$coef[2],
    unadjusted_se = unadj$se[2],
    coef = msm$coef,
    vcov = msm$vcov,
    weights = w,
    mean_weight = s1 / n,
    max_weight = max(w),
    effective_sample_size = if (s2 > 0) (s1 * s1 / s2) else 0,
    cumulative_exposure = cum,
    exposure = expo,
    per_time = lapply(seq_along(per_time), function(t) {
      list(
        time = if (!is.null(time) && t <= length(time)) time[t] else (t - 1L),
        mean_weight = sum(per_time[[t]]$weight) / n
      )
    }),
    n_times = K,
    n = n,
    contrast = contrast,
    method = "IP-weighted MSM for a time-varying treatment, Hernan & Robins (2020) Sec. 21.2, with the over-adjusted and unadjusted comparators of Ch. 20"
  )

  result
}

#' morie_tdcvar_cheatsheet
#'
#' A step of the tdcvar_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_tdcvar_cheatsheet <- function() {
  "tdcvar: time-varying IPTW MSM (H&R Ch.21). Returns the weighted MSM plus the two biased comparators -- adjusting for a treatment-affected confounder over-adjusts, omitting it under-adjusts, and Ch.20 says they straddle the truth."
}
