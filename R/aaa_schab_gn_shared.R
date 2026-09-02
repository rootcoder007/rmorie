# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Gauss-Newton fitting of a semivariogram model.
#
# Schabenberger & Gotway (2005), Sec. 4.5.1 and Sec. 4.5.3. The book names
# the algorithm rather than leaving it open:
#
#   "GEE estimates can thus be calculated as the ordinary (nonlinear) least
#    squares estimates in the model T = 2 gamma(h, theta) + delta ... with a
#    Gauss-Newton algorithm."                                   (after 4.43)
#
#   "Composite likelihood estimates can thus be calculated by (nonlinear)
#    weighted least squares in the model T = 2 gamma(h,theta) + delta,
#    delta ~ (0, 8 gamma(h,theta)^2), with a Gauss-Newton algorithm."
#                                                               (after 4.44)
#
# and for the weighted case, Sec. 4.5.1: "an iterative re-weighting scheme is
# employed since updates to theta should be followed by updates to R(theta)".
# So the weights are recomputed every outer iteration, not frozen at the
# start.
#
# The estimating equation (4.42)/(4.43) is written in terms of
# d gamma(h, theta) / d theta, so those derivatives are analytic here. That is
# what makes the two language arms run the same arithmetic: an analytic
# Jacobian has no finite-difference step to choose, and no library solver is
# involved on either side.
#
# Internal. The `aaa_` prefix collates this before its callers -- R never
# sources a file whose name starts with a non-alphanumeric character.

#' D gamma(h; c0, sigma0^2, a) / d(c0, sigma0^2, a), one row per lag
#'
#' d gamma / d c0 = 1 d gamma / d sigma0^2 = 1 - R(h; a) d gamma / d a =
#' -sigma0^2 dR/da Every row is zero at h = 0, where gamma(0) = 0 by
#' definition whatever the nugget (Sec. 4.3.6).
#'
#' @param h A vector; its length is taken and its elements indexed.
#' @param nugget Accepted by the signature and not used anywhere in the body.
#' @param sill Numeric; combined arithmetically in the body.
#' @param rng Numeric; combined arithmetically in the body.
#' @param model Passed to \code{.sp_correlogram}.
#' @return The value of \code{jac}, as built in the body.
#' @export
.schab_semivariogram_jacobian <- function(h, nugget, sill, rng, model) {
  # d gamma(h; c0, sigma0^2, a) / d(c0, sigma0^2, a), one row per lag.
  #   d gamma / d c0       = 1
  #   d gamma / d sigma0^2 = 1 - R(h; a)
  #   d gamma / d a        = -sigma0^2 dR/da
  # Every row is zero at h = 0, where gamma(0) = 0 by definition whatever the
  # nugget (Sec. 4.3.6).
  h <- as.numeric(h)
  # .sp_correlogram spells the practical-range constant inline as 3
  # (exp(-3) = 0.049787, the book's convention, Sec. 4.3); match it.
  cc <- 3
  r <- .sp_correlogram(h, rng, model)
  dr_da <- switch(model,
    exponential = r * (cc * h / rng^2),
    gaussian = r * (2 * cc * h^2 / rng^3),
    spherical = {
      out <- numeric(length(h))
      inside <- h <= rng
      hi <- h[inside]
      out[inside] <- 1.5 * hi / rng^2 - 1.5 * hi^3 / rng^4
      out
    },
    stop(sprintf("unknown model '%s'", model), call. = FALSE)
  )
  jac <- cbind(rep(1, length(h)), 1 - r, -sill * dr_da)
  jac[h == 0, ] <- 0
  jac
}

#' OLS is R = phi I, so the weights are 1. WLS uses Cressie\'s (1985)
#'
#' approximation (4.33), Var\[gamma_hat(h_m)\] = 2 gamma^2 / |N(h_m)|,
#' whose reciprocal is the weight in (4.34).
#'
#' @param kind Passed to \code{identical}.
#' @param fitted A vector; its length is taken.
#' @param counts Numeric; combined arithmetically in the body.
#' @return The value of \code{ifelse}.
#' @export
.schab_gn_weights <- function(kind, fitted, counts) {
  # OLS is R = phi I, so the weights are 1. WLS uses Cressie's (1985)
  # approximation (4.33), Var[gamma_hat(h_m)] = 2 gamma^2 / |N(h_m)|, whose
  # reciprocal is the weight in (4.34).
  if (identical(kind, "ols")) {
    return(rep(1, length(fitted)))
  }
  denom <- 2 * fitted^2
  ifelse(denom > 0, counts / ifelse(denom > 0, denom, 1), 0)
}

#' Onto the parameter space of Sec. 4.3: variances >= 0, a range > 0.
#' This is
#'
#' the constraint the model imposes, not a search box.
#'
#' @param theta A vector; indexed elementwise.
#' @return A vector, from \code{c}.
#' @export
.schab_gn_project <- function(theta) {
  # Onto the parameter space of Sec. 4.3: variances >= 0, a range > 0. This is
  # the constraint the model imposes, not a search box.
  c(max(theta[1], 0), max(theta[2], 0), max(theta[3], .Machine$double.xmin))
}

#' .schab_gauss_newton
#'
#' A step of the schab_gn_shared implementation. Called by \code{.schab_fit_semivariogram}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param lags A vector; indexed elementwise.
#' @param ghat A vector; indexed elementwise.
#' @param counts A vector; indexed elementwise.
#' @param start Passed to \code{.schab_gn_project}.
#' @param model Passed to \code{.sp_semivariogram}. Defaults to \code{"exponential"}.
#' @param kind Passed to \code{.schab_gn_weights}. Defaults to \code{"wls"}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-12}.
#' @param max_halvings A count; the body uses it as \code{seq_len(...)}. Defaults to \code{40L}.
#' @return A list with \code{theta}, \code{objective}, \code{converged}, \code{iterations}.
#' @export
.schab_gauss_newton <- function(lags, ghat, counts, start, model = "exponential",
                                kind = "wls", max_iter = 200L, tol = 1e-12,
                                max_halvings = 40L) {
  ok <- is.finite(lags) & is.finite(ghat) & counts > 0
  h <- lags[ok]
  g <- ghat[ok]
  n <- counts[ok]
  if (length(h) < 3L) {
    stop("need at least 3 usable lag classes to fit 3 parameters", call. = FALSE)
  }

  objective <- function(theta) {
    fitted <- .sp_semivariogram(h, theta[1], theta[2], theta[3], model)
    if (any(!is.finite(fitted))) {
      return(list(
        value = Inf, fitted = fitted, w = rep(0, length(h)),
        resid = rep(0, length(h))
      ))
    }
    if (identical(kind, "wls") && any(fitted <= 0)) {
      # gamma(h) = 0 at a positive lag means no nugget and no partial sill:
      # the model has collapsed and (4.34) is undefined there.
      return(list(
        value = Inf, fitted = fitted, w = rep(0, length(h)),
        resid = rep(0, length(h))
      ))
    }
    w <- .schab_gn_weights(kind, fitted, n)
    resid <- g - fitted
    val <- sum(w * resid^2)
    list(
      value = if (is.finite(val)) val else Inf, fitted = fitted,
      w = w, resid = resid
    )
  }

  theta <- .schab_gn_project(start)
  cur <- objective(theta)
  obj <- cur$value
  converged <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    jac <- .schab_semivariogram_jacobian(h, theta[1], theta[2], theta[3], model)
    # Normal equations of the weighted Gauss-Newton step:
    #   (J' W J) delta = J' W r
    jw <- jac * cur$w
    lhs <- crossprod(jac, jw)
    rhs <- crossprod(jw, cur$resid)
    if (any(!is.finite(lhs)) || any(!is.finite(rhs))) break
    delta <- tryCatch(as.numeric(solve(lhs, rhs)), error = function(e) NULL)
    if (is.null(delta) || any(!is.finite(delta))) break
    # Step halving: a plain Gauss-Newton step need not decrease a weighted
    # objective whose weights also move, so shorten it until it does. The
    # sequence is fixed, so both language arms take the same steps.
    stepped <- FALSE
    for (k in seq_len(max_halvings) - 1L) {
      trial <- .schab_gn_project(theta + delta / (2^k))
      tr <- objective(trial)
      if (is.finite(tr$value) && tr$value < obj) {
        rel <- (obj - tr$value) / max(abs(obj), 1e-300)
        theta <- trial
        obj <- tr$value
        cur <- tr
        stepped <- TRUE
        converged <- rel < tol
        break
      }
    }
    if (!stepped || converged) {
      converged <- TRUE
      break
    }
  }
  list(theta = theta, objective = obj, converged = converged, iterations = it)
}
