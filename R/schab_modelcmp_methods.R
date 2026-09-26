# SPDX-License-Identifier: AGPL-3.0-or-later
.sp_neg2loglik <- function(coords, z, model, nugget, psill, rng, X, reml) {
  d <- as.matrix(stats::dist(coords))
  mdl <- if (model == "linear") "tent" else model
  cm <- nugget + psill - .sp_semivariogram(as.vector(d), nugget, psill, max(rng, 1e-12), mdl)
  cm <- matrix(cm, nrow(d))
  diag(cm) <- nugget + psill
  n <- length(z)
  cm <- cm + diag(1e-10 * max(sum(diag(cm)) / n, 1e-12), n)
  ch <- tryCatch(chol(cm), error = function(e) NULL)
  if (is.null(ch)) return(list(value = Inf, beta = NA_real_))
  ci_x <- backsolve(ch, forwardsolve(t(ch), X))
  xcx <- crossprod(X, ci_x)
  beta <- drop(solve(xcx, crossprod(ci_x, z)))
  r <- z - drop(X %*% beta)
  quad <- sum(forwardsolve(t(ch), r)^2)
  logdet <- 2 * sum(log(diag(ch)))
  if (reml) {
    dxcx <- determinant(xcx, logarithm = TRUE)
    if (dxcx$sign <= 0) return(list(value = Inf, beta = NA_real_))
    return(list(value = logdet + as.numeric(dxcx$modulus) + quad + (n - ncol(X)) * log(2 * pi),
                beta = beta))
  }
  list(value = logdet + n * log(2 * pi) + quad, beta = beta)
}

.sp_ml_fit <- function(coords, z, model, X, reml, nugget = TRUE) {
  v0 <- stats::var(z) * (length(z) - 1) / length(z)
  hmax <- max(stats::dist(coords))
  obj <- function(p) {
    t <- exp(p)
    v <- if (nugget) .sp_neg2loglik(coords, z, model, t[1L], t[2L], t[3L], X, reml)$value else
      .sp_neg2loglik(coords, z, model, 0, t[1L], t[2L], X, reml)$value
    if (is.finite(v)) v else 1e12
  }
  best <- NULL
  for (frac in c(0.1, 0.25, 0.5, 1.0)) {
    s <- if (nugget) log(c(0.1 * v0, 0.9 * v0, max(frac * hmax, 1e-6))) else
      log(c(v0, max(frac * hmax, 1e-6)))
    o <- .schab_npvg_nelder_mead(obj, s, max_iter = 2000L, tol = 1e-12)
    if (is.null(best) || o$value < best$value) best <- o
  }
  t <- exp(best$par)
  if (!nugget) t <- c(0, t)
  f <- .sp_neg2loglik(coords, z, model, t[1L], t[2L], t[3L], X, reml)
  list(nugget = t[1L], psill = t[2L], range = t[3L], sill = t[1L] + t[2L], beta = f$beta,
       neg2loglik = f$value, converged = is.finite(f$value))
}

#' Covariance parameters by maximum or restricted maximum likelihood
#'
#' Schabenberger & Gotway (2005) eq (4.35) (ML, mean profiled by GLS) and p.
#' 263 (REML, adding ln|X' Sigma^-1 X| and n - k in the constant), for
#' isotropic models with nugget, partial sill and practical range, by
#' multi-start Nelder-Mead on the log scale. ML covariance estimates are
#' biased downward; REML likelihoods compare only across equal mean models.
#'
#' @param coords Two-column matrix of site coordinates.
#' @param z Numeric responses.
#' @param variogram_model "exponential", "spherical", "gaussian" or "linear".
#' @param method "ml", "reml" or "both".
#' @param X Optional mean design, default an intercept.
#' @return Named list: nugget, psill, range, sill, beta, neg2loglik,
#'   converged, method_used (with ml and reml when both), model, n.
#' @references Schabenberger & Gotway (2005), eqs (4.35)-(4.39), pp. 166-168.
#' @examples
#' xy <- cbind(rep(1:5, 5), rep(1:5, each = 5))
#' spml(xy, sin(xy[, 1]) + cos(xy[, 2]), method = "reml")$range
#' @export
spml <- function(coords, z, variogram_model = "exponential", method = "ml", X = NULL) {
  if (!variogram_model %in% c("exponential", "spherical", "gaussian", "linear")) {
    stop("model must be one of exponential, spherical, gaussian, linear", call. = FALSE)
  }
  if (!method %in% c("ml", "reml", "both")) stop("method must be 'ml', 'reml' or 'both'", call. = FALSE)
  z <- as.numeric(z)
  X <- if (is.null(X)) matrix(1, length(z), 1L) else as.matrix(X)
  if (stats::var(z) <= 0) stop("z has zero variance; no covariance to estimate.", call. = FALSE)
  if (method == "both") {
    ml <- .sp_ml_fit(coords, z, variogram_model, X, FALSE)
    rl <- .sp_ml_fit(coords, z, variogram_model, X, TRUE)
    out <- c(rl, list(ml = ml, reml = rl, method_used = "both"))
  } else {
    out <- c(.sp_ml_fit(coords, z, variogram_model, X, method == "reml"), list(method_used = method))
  }
  c(out, list(model = variogram_model, n = length(z)))
}

#' Likelihood-ratio tests and AIC for nested spatial covariance models
#'
#' Schabenberger & Gotway (2005) eqs (6.57)-(6.60): fit Z = X beta + e under
#' `model` with a nugget, without one, and independence, by ML or REML. The
#' nugget test is on the boundary, so its p-value is half the chi-square(1)
#' one (Self & Liang 1987); the independence test uses chi-square(1) as in the
#' book. AIC is phi + 2(k + q) for ML and phi_R + 2q for REML.
#'
#' @param coords Two-column matrix of site coordinates.
#' @param z Numeric responses.
#' @param X Optional mean design, default an intercept.
#' @param model "exponential", "gaussian", "spherical" or "linear".
#' @param method "ml" or "reml".
#' @return Named list: fits, lrt_nugget, p_nugget, p_nugget_naive,
#'   lrt_spatial, p_spatial, k, method, model.
#' @references Self, S. G. & Liang, K.-Y. (1987). JASA 82, 605-610.
#'   Schabenberger & Gotway (2005), eqs (6.57)-(6.60), pp. 343-345.
#' @examples
#' xy <- cbind(rep(1:5, 5), rep(1:5, each = 5))
#' spcmp(xy, sin(xy[, 1]) + cos(xy[, 2]))$lrt_spatial
#' @export
spcmp <- function(coords, z, X = NULL, model = "exponential", method = "reml") {
  if (!method %in% c("ml", "reml")) stop("`method` must be 'ml' or 'reml'", call. = FALSE)
  z <- as.numeric(z)
  n <- length(z)
  X <- if (is.null(X)) matrix(1, n, 1L) else as.matrix(X)
  k <- ncol(X)
  reml <- method == "reml"
  full <- .sp_ml_fit(coords, z, model, X, reml)
  nonug <- .sp_ml_fit(coords, z, model, X, reml, nugget = FALSE)
  rss <- sum(stats::lm.fit(X, z)$residuals^2)
  s2 <- rss / (if (reml) n - k else n)
  phi_ind <- .sp_neg2loglik(coords, z, model, s2, 0, 1, X, reml)$value
  aic <- function(phi, q) phi + 2 * (if (reml) q else k + q)
  fits <- list(
    nugget = list(neg2loglik = full$neg2loglik, q = 3L, aic = aic(full$neg2loglik, 3),
                  nugget = full$nugget, psill = full$psill, range = full$range),
    no_nugget = list(neg2loglik = nonug$neg2loglik, q = 2L, aic = aic(nonug$neg2loglik, 2),
                     nugget = 0, psill = nonug$psill, range = nonug$range),
    independent = list(neg2loglik = phi_ind, q = 1L, aic = aic(phi_ind, 1), sigma2 = s2))
  lrt_n <- max(nonug$neg2loglik - full$neg2loglik, 0)
  lrt_s <- max(phi_ind - nonug$neg2loglik, 0)
  p_naive <- stats::pchisq(lrt_n, 1, lower.tail = FALSE)
  list(fits = fits, lrt_nugget = lrt_n, p_nugget = 0.5 * p_naive, p_nugget_naive = p_naive,
       lrt_spatial = lrt_s, p_spatial = stats::pchisq(lrt_s, 1, lower.tail = FALSE), k = k,
       method = method, model = model)
}
