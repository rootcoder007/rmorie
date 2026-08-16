# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Simulation of Gaussian random fields. Schabenberger & Gotway (2005), Ch. 7.
#
# The chapter opens from the reproductive property of the Gaussian: if
# Sigma = Sigma^(1/2) Sigma^(1/2)' and X ~ G(0, I), then mu + Sigma^(1/2) X
# has a G(mu, Sigma) distribution. Everything in Sec. 7.1 is a choice of
# square root.
#
#   Sec. 7.1.1  Cholesky (LU): Sigma = U'U with U upper triangular, and
#               "Return y = mu + U'x as a realization from a G(mu, Sigma)".
#   Sec. 7.1.2  Spectral: Sigma = P Delta P', so Sigma^(1/2) = P Delta^(1/2) P'
#               -- the SYMMETRIC root, hence a different field from the same X.
#   Sec. 7.2.2  Conditioning by kriging, eq (7.1):
#               Zc(s) = S(s) + c' Sigma^-1 (Z - Sm).
#
# Draws come from morie's own Philox/AS 241 generator, so an R and a Python
# run with the same seed produce the same field, not merely the same
# distribution.
#
# Internal; `aaa_` collates it before its callers.

#' Lower-triangular L with L L\' = Sigma. The book writes the root as an
#'
#' upper triangular U with Sigma = U\'U; L is that U\'.
#'
#' @param cov See Usage.
#' @param jitter Defaults to \code{1e-10}.
#' @return A matrix, from \code{t}.
#' @export
.schab_cholesky_root <- function(cov, jitter = 1e-10) {
  # Lower-triangular L with L L' = Sigma. The book writes the root as an
  # upper triangular U with Sigma = U'U; L is that U'.
  cov <- as.matrix(cov)
  if (nrow(cov) != ncol(cov)) stop("`cov` must be square", call. = FALSE)
  u <- tryCatch(chol(cov),
    error = function(e) chol(cov + jitter * diag(nrow(cov)))
  )
  t(u)
}

#' Symmetric square root P Delta^(1/2) P\'. Negative eigenvalues can
#' only
#'
#' come from rounding on a matrix positive semi-definite in exact
#' arithmetic, so they are clipped rather than allowed to make NaNs.
#'
#' @param cov See Usage.
#' @param tol Defaults to \code{NULL}.
#' @return The value of \code{%*%}.
#' @export
.schab_spectral_root <- function(cov, tol = NULL) {
  # Symmetric square root P Delta^(1/2) P'. Negative eigenvalues can only
  # come from rounding on a matrix positive semi-definite in exact
  # arithmetic, so they are clipped rather than allowed to make NaNs.
  cov <- as.matrix(cov)
  if (nrow(cov) != ncol(cov)) stop("`cov` must be square", call. = FALSE)
  sym <- (cov + t(cov)) / 2
  e <- eigen(sym, symmetric = TRUE)
  if (is.null(tol)) {
    tol <- nrow(sym) * .Machine$double.eps * max(abs(max(e$values)), 1)
  }
  vals <- ifelse(e$values < tol, 0, e$values)
  e$vectors %*% (sqrt(vals) * t(e$vectors))
}

#' .schab_simulate_unconditional
#'
#' Part of the schab_sim_shared implementation; see the file header for
#' the source it follows.
#'
#' @param mean See Usage.
#' @param cov See Usage.
#' @param method Defaults to \code{"cholesky"}.
#' @param seed Defaults to \code{0}.
#' @param stream Defaults to \code{0}.
#' @return A vector, from \code{as.numeric}.
#' @export
.schab_simulate_unconditional <- function(mean, cov, method = "cholesky",
                                          seed = 0, stream = 0) {
  mean <- as.numeric(mean)
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!all(dim(cov) == c(n, n))) {
    stop("`cov` must be square and match `mean`", call. = FALSE)
  }
  root <- switch(method,
    cholesky = .schab_cholesky_root(cov),
    spectral = .schab_spectral_root(cov),
    stop("`method` must be 'cholesky' or 'spectral'", call. = FALSE)
  )
  as.numeric(mean + root %*% .morie_random_normal(n, seed = seed, stream = stream))
}

#' .schab_simulate_conditional
#'
#' Part of the schab_sim_shared implementation; see the file header for
#' the source it follows.
#'
#' @param cov_all See Usage.
#' @param z_obs See Usage.
#' @param n_obs See Usage.
#' @param mean Defaults to \code{0}.
#' @param method Defaults to \code{"cholesky"}.
#' @param seed Defaults to \code{0}.
#' @param stream Defaults to \code{0}.
#' @return A vector, from \code{as.numeric}.
#' @export
.schab_simulate_conditional <- function(cov_all, z_obs, n_obs, mean = 0,
                                        method = "cholesky", seed = 0,
                                        stream = 0) {
  # eq (7.1): Zc(s) = S(s) + c' Sigma^-1 (Z - Sm). The text notes the
  # unconditional draw need not carry the same mean -- "Any mean will do,
  # for example, E[S(s)] = 0" -- so it is centred and the mean rides in
  # through the correction.
  cov_all <- as.matrix(cov_all)
  z_obs <- as.numeric(z_obs)
  n_obs <- as.integer(n_obs)
  n <- nrow(cov_all)
  if (ncol(cov_all) != n) stop("`cov_all` must be square", call. = FALSE)
  if (length(z_obs) != n_obs) {
    stop("`z_obs` must have `n_obs` entries", call. = FALSE)
  }
  if (n_obs <= 0L || n_obs >= n) {
    stop("`n_obs` must leave at least one target location", call. = FALSE)
  }
  mu <- if (length(mean) == 1L) rep(as.numeric(mean), n) else as.numeric(mean)
  sim <- .schab_simulate_unconditional(rep(0, n), cov_all,
    method = method,
    seed = seed, stream = stream
  )
  sigma_obs <- cov_all[seq_len(n_obs), seq_len(n_obs), drop = FALSE]
  cvec <- cov_all[, seq_len(n_obs), drop = FALSE]
  resid <- (z_obs - mu[seq_len(n_obs)]) - sim[seq_len(n_obs)]
  as.numeric(mu + sim + cvec %*% solve(sigma_obs, resid))
}

#' Sigma^2_sk at every location, for the E[(Zc - Z)^2] = 2 sigma^2_sk
#'
#' identity that closes Sec. 7.2.2.
#'
#' @param cov_all See Usage.
#' @param n_obs See Usage.
#' @return A numeric value.
#' @export
.schab_simple_kriging_variance <- function(cov_all, n_obs) {
  # sigma^2_sk at every location, for the E[(Zc - Z)^2] = 2 sigma^2_sk
  # identity that closes Sec. 7.2.2.
  cov_all <- as.matrix(cov_all)
  n_obs <- as.integer(n_obs)
  sigma_obs <- cov_all[seq_len(n_obs), seq_len(n_obs), drop = FALSE]
  cvec <- cov_all[, seq_len(n_obs), drop = FALSE]
  diag(cov_all) - rowSums(cvec * t(solve(sigma_obs, t(cvec))))
}
