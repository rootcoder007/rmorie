# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Restricted maximum likelihood for the covariance parameters.
#
# Schabenberger & Gotway (2005), Sec. 4.5.2 and Sec. 5.5.3. Three things come
# straight from the text rather than from convenience:
#
# 1. The matrix of error contrasts K disappears. Sec. 5.5.3 quotes Searle et
#    al. (1992, pp. 451-452),
#      K'(K Sigma K')^-1 K = Sigma^-1 - Sigma^-1 X Omega X' Sigma^-1,
#      Omega = (X' Sigma^-1 X)^-1,
#    and since Omega X' Sigma^-1 Z = beta_hat, Z'K'(K Sigma K')^-1 KZ reduces
#    to r' Sigma^-1 r. With Harville's (1974, 1977) choice of K,
#      phi_R(theta) = ln|Sigma| + ln|X' Sigma^-1 X| + r' Sigma^-1 r
#                     + (n - k) ln(2 pi),
#    which never forms K at all. Harville notes other admissible K change the
#    objective only by a constant free of theta and beta.
#
# 2. A scale parameter is profiled out, eq (5.49). With
#    Sigma(theta) = sigma^2 Sigma(theta*),
#      sigma^2_reml = r' Sigma(theta*)^-1 r / (n - k)
#      phi_R,sigma(theta*) = ln|Sigma(theta*)| + ln|X' Sigma(theta*)^-1 X|
#                            + (n-k) ln(sigma^2) + (n-k)(ln(2 pi) - 1),
#    leaving TWO free parameters, the nugget ratio and the range.
#
# 3. Sec. 5.5.2 names the optimiser: "Newton-Raphson, Quasi-Newton, or some
#    other suitable algorithm". The quasi-Newton branch is taken, driven by
#    the exact gradient below, so no finite-difference step enters and this
#    arm runs the same arithmetic as the Python one.
#
# Internal; `aaa_` keeps it collated before its callers.

#' Sigma(theta*) = xi I + (1 - xi) R(h; a). Factoring
#'
#' sigma^2 = c0 + sigma0^2 out of Sigma leaves the nugget as a RATIO in
#' [0, 1] -- the reparameterisation Sec. 5.5.2 calls for.
#'
#' @param coords See Usage.
#' @param nugget_ratio See Usage.
#' @param rng See Usage.
#' @param model See Usage.
#' @return A list with \code{sigma}, \code{d}, \code{r}.
#' @export
.schab_correlation_matrix <- function(coords, nugget_ratio, rng, model) {
  # Sigma(theta*) = xi I + (1 - xi) R(h; a). Factoring
  # sigma^2 = c0 + sigma0^2 out of Sigma leaves the nugget as a RATIO in
  # [0, 1] -- the reparameterisation Sec. 5.5.2 calls for.
  coords <- as.matrix(coords)
  d <- as.matrix(stats::dist(coords))
  r <- matrix(.sp_correlogram(as.numeric(d), rng, model), nrow(d), ncol(d))
  sigma <- (1 - nugget_ratio) * r
  diag(sigma) <- 1
  list(sigma = sigma, d = d, r = r)
}

#' DSigma(theta*)/d(xi, a): d/dxi is I - R, d/da is (1 - xi) dR/da.
#' dR/da is
#'
#' the same expression the Gauss-Newton Jacobian uses, so both fitters
#' share one derivation.
#'
#' @param d See Usage.
#' @param r See Usage.
#' @param nugget_ratio See Usage.
#' @param rng See Usage.
#' @param model See Usage.
#' @return The value of \code{list}.
#' @export
.schab_dsigma <- function(d, r, nugget_ratio, rng, model) {
  # dSigma(theta*)/d(xi, a): d/dxi is I - R, d/da is (1 - xi) dR/da. dR/da is
  # the same expression the Gauss-Newton Jacobian uses, so both fitters share
  # one derivation.
  n <- nrow(d)
  d_xi <- -r
  diag(d_xi) <- 0
  flat <- .schab_semivariogram_jacobian(as.numeric(d), 0, 1, rng, model)[, 3]
  dr_da <- matrix(-flat, n, n)
  d_a <- (1 - nugget_ratio) * dr_da
  diag(d_a) <- 0
  list(d_xi, d_a)
}

#' .schab_profiled_reml
#'
#' Part of the schab_reml_shared implementation; see the file header for
#' the source it follows.
#'
#' @param coords See Usage.
#' @param z See Usage.
#' @param X See Usage.
#' @param nugget_ratio See Usage.
#' @param rng See Usage.
#' @param model See Usage.
#' @return A list with \code{value}, \code{gradient}, \code{sigma2}, \code{beta}.
#' @export
.schab_profiled_reml <- function(coords, z, X, nugget_ratio, rng, model) {
  n <- nrow(X)
  k <- ncol(X)
  bad <- list(
    value = Inf, gradient = c(0, 0), sigma2 = NA_real_,
    beta = rep(NA_real_, k)
  )
  if (nugget_ratio < 0 || nugget_ratio > 1 || rng <= 0) {
    return(bad)
  }
  cm <- .schab_correlation_matrix(coords, nugget_ratio, rng, model)
  ch <- tryCatch(chol(cm$sigma), error = function(e) NULL)
  if (is.null(ch)) {
    return(bad)
  }
  logdet <- 2 * sum(log(diag(ch)))
  sinv <- chol2inv(ch)
  sx <- sinv %*% X
  xsx <- crossprod(X, sx)
  omega <- tryCatch(solve(xsx), error = function(e) NULL)
  if (is.null(omega)) {
    return(bad)
  }
  ldx <- determinant(xsx, logarithm = TRUE)
  if (ldx$sign <= 0) {
    return(bad)
  }
  logdet_xsx <- as.numeric(ldx$modulus)
  beta <- as.numeric(omega %*% crossprod(sx, z))
  resid <- as.numeric(z - X %*% beta)
  sr <- as.numeric(sinv %*% resid)
  dof <- n - k
  sigma2 <- sum(resid * sr) / dof
  if (!is.finite(sigma2) || sigma2 <= 0) {
    return(bad)
  }
  value <- logdet + logdet_xsx + dof * log(sigma2) + dof * (log(2 * pi) - 1)

  # Exact gradient. For each derivative D of Sigma(theta*):
  #   d ln|Sigma|         = tr(Sigma^-1 D)
  #   d ln|X'Sigma^-1 X|  = -tr(Omega X'Sigma^-1 D Sigma^-1 X)
  #   d (n-k) ln(sigma^2) = -r'Sigma^-1 D Sigma^-1 r / sigma^2
  # The d beta / d theta* terms cancel through the GLS normal equations.
  ds <- .schab_dsigma(cm$d, cm$r, nugget_ratio, rng, model)
  grad <- numeric(2)
  for (j in 1:2) {
    dmat <- ds[[j]]
    t1 <- sum(sinv * t(dmat))
    t2 <- -sum(diag(omega %*% crossprod(sx, dmat %*% sx)))
    t3 <- -as.numeric(crossprod(sr, dmat %*% sr)) / sigma2
    grad[j] <- t1 + t2 + t3
  }
  list(value = value, gradient = grad, sigma2 = sigma2, beta = beta)
}

#' .schab_logistic
#'
#' Part of the schab_reml_shared implementation; see the file header for
#' the source it follows.
#'
#' @param u See Usage.
#' @return A numeric value.
#' @export
.schab_logistic <- function(u) 1 / (1 + exp(-u))
#' .schab_logit
#'
#' Part of the schab_reml_shared implementation; see the file header for
#' the source it follows.
#'
#' @param p See Usage.
#' @return A numeric value.
#' @export
.schab_logit <- function(p) {
  p <- min(max(p, 1e-12), 1 - 1e-12)
  log(p / (1 - p))
}

#' .schab_fit_reml
#'
#' Part of the schab_reml_shared implementation; see the file header for
#' the source it follows.
#'
#' @param coords See Usage.
#' @param z See Usage.
#' @param X See Usage.
#' @param model Defaults to \code{"exponential"}.
#' @param start_ratio Defaults to \code{0.1}.
#' @param start_range Defaults to \code{NULL}.
#' @param max_iter Defaults to \code{200L}.
#' @param tol Defaults to \code{1e-10}.
#' @return A list with \code{nugget_ratio}, \code{range}, \code{sigma2}, \code{nugget}, \code{partial_sill}, \code{beta}, \code{neg2_restricted_loglik}, \code{converged}.
#' @export
.schab_fit_reml <- function(coords, z, X, model = "exponential",
                            start_ratio = 0.1, start_range = NULL,
                            max_iter = 200L, tol = 1e-10) {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  X <- as.matrix(X)
  n <- length(z)
  if (nrow(coords) != n || nrow(X) != n) {
    stop("`coords`, `z` and `X` must agree on the sample size", call. = FALSE)
  }
  if (ncol(X) >= n) stop("design matrix leaves no error contrasts", call. = FALSE)
  if (is.null(start_range)) {
    start_range <- max(max(as.matrix(stats::dist(coords))) / 4, 1e-6)
  }

  # Unconstrained scale: xi = logistic(u1) keeps the nugget ratio in [0, 1]
  # and a = exp(u2) keeps the range positive, so the constraints of Sec. 4.3
  # hold by construction rather than by clipping.
  wrapped <- function(u) {
    xi <- .schab_logistic(u[1])
    a <- exp(u[2])
    res <- .schab_profiled_reml(coords, z, X, xi, a, model)
    if (!is.finite(res$value)) {
      return(list(
        value = Inf, gradient = c(0, 0), sigma2 = res$sigma2,
        beta = res$beta
      ))
    }
    res$gradient <- res$gradient * c(xi * (1 - xi), a)
    res
  }

  x <- c(.schab_logit(start_ratio), log(start_range))
  cur <- wrapped(x)
  hess_inv <- diag(2)
  for (iter in seq_len(max_iter)) {
    if (!is.finite(cur$value) || max(abs(cur$gradient)) < tol) break
    direction <- as.numeric(-hess_inv %*% cur$gradient)
    slope <- sum(cur$gradient * direction)
    if (slope >= 0) { # not a descent direction; reset
      hess_inv <- diag(2)
      direction <- -cur$gradient
      slope <- sum(cur$gradient * direction)
    }
    step <- 1
    moved <- FALSE
    trial <- NULL
    tr <- NULL
    # Armijo backtracking with the textbook constants c1 = 1e-4, rho = 1/2
    # (Nocedal & Wright), fixed rather than tuned.
    for (b in seq_len(60L)) {
      trial <- x + step * direction
      tr <- wrapped(trial)
      if (is.finite(tr$value) && tr$value <= cur$value + 1e-4 * step * slope) {
        moved <- TRUE
        break
      }
      step <- step / 2
    }
    if (!moved) break
    s <- trial - x
    y <- tr$gradient - cur$gradient
    sy <- sum(s * y)
    if (sy > 1e-300) { # BFGS update, skipped if unstable
      rho <- 1 / sy
      eye <- diag(2)
      hess_inv <- (eye - rho * outer(s, y)) %*% hess_inv %*%
        (eye - rho * outer(y, s)) + rho * outer(s, s)
    }
    x <- trial
    cur <- tr
  }

  xi <- .schab_logistic(x[1])
  a <- exp(x[2])
  list(
    nugget_ratio = xi, range = a, sigma2 = cur$sigma2,
    nugget = xi * cur$sigma2, partial_sill = (1 - xi) * cur$sigma2,
    beta = cur$beta, neg2_restricted_loglik = cur$value,
    converged = max(abs(cur$gradient)) < 1e-6
  )
}
