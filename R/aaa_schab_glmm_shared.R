# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Spatial GLMs, GLMMs, and the CAR family of disease-mapping priors.
# Twin of the Python arm's src/morie/fn/_schab_glmm.py -- the same equations
# and the same arithmetic, so the two arms agree by construction rather than
# to a tolerance nobody chose.
#
# Two primary sources, because one book does not cover the family.
#
# Schabenberger, O. & Gotway, C. A. (2005), Statistical Methods for Spatial
# Data Analysis, Sec. 6.3-6.4: the conditional specification (6.73)-(6.74),
# the pseudo-likelihood machinery (6.78)-(6.85), prediction (6.87)-(6.91),
# and the disease-mapping hierarchy (6.99)-(6.104).
#
# Besag, J., York, J. & Mollie, A. (1991), Bayesian Image Restoration, with
# Two Applications in Spatial Statistics, Ann. Inst. Statist. Math.
# 43(1):1-59, Sec. 4: the intrinsic autoregression (4.2), its conditional
# moments (4.3), the median alternative (4.4), the joint posterior (4.5) and
# the hyperprior (4.6). Schabenberger cites this paper but never states the
# convolution.
#
# Tonui, B., Mwalili, S. & Wanjoya, A. (2018), Open Journal of Statistics
# 8:811-830: the matrix form of the ICAR structure, the Leroux LCAR prior,
# the random-walk temporal priors, the Kronecker space-time interactions
# with their rank deficiencies, and the null-space constraint (12).
#
# Internal; `aaa_` collates it before its callers.

# --- links and variance functions -----------------------------------------

#' .schab_link
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_conditional_mean}, \code{.schab_fit_pseudo_likelihood}, \code{.schab_naive_marginal_mean} and 3 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; passed to \code{exp}.
#' @param kind Passed to \code{identical}.
#' @param inverse A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return Nothing; this branch always raises.
#' @export
.schab_link <- function(x, kind, inverse = FALSE) {
  x <- as.numeric(x)
  if (identical(kind, "log")) {
    return(if (inverse) exp(x) else log(x))
  }
  if (identical(kind, "logit")) {
    return(if (inverse) 1 / (1 + exp(-x)) else log(x / (1 - x)))
  }
  if (identical(kind, "identity")) {
    return(x)
  }
  stop(sprintf("`link` must be 'log', 'logit' or 'identity', got '%s'", kind),
    call. = FALSE
  )
}

#' .schab_link_derivative
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_mu_eta}, \code{.schab_predict_glm}, \code{.schab_pseudo_data} and 1 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param mu A vector; its length is taken.
#' @param kind Passed to \code{identical}.
#' @return Nothing; this branch always raises.
#' @export
.schab_link_derivative <- function(mu, kind) {
  mu <- as.numeric(mu) # g'(mu) = d eta / d mu
  if (identical(kind, "log")) {
    return(1 / mu)
  }
  if (identical(kind, "logit")) {
    return(1 / (mu * (1 - mu)))
  }
  if (identical(kind, "identity")) {
    return(rep(1, length(mu)))
  }
  stop("unknown link", call. = FALSE)
}

#' D mu / d eta, the diagonal of Psi; the reciprocal of g\'(mu), as the
#' text
#'
#' notes when deriving (6.89).
#'
#' @param mu Passed to \code{.schab_link_derivative}.
#' @param kind Passed to \code{.schab_link_derivative}.
#' @return A numeric value.
#' @export
.schab_mu_eta <- function(mu, kind) {
  # d mu / d eta, the diagonal of Psi; the reciprocal of g'(mu), as the text
  # notes when deriving (6.89).
  1 / .schab_link_derivative(mu, kind)
}

#' .schab_variance_function
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_conditional_variance}, \code{.schab_data_covariance}, \code{.schab_sigma_mu}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param mu A vector; its length is taken.
#' @param family The body requires: `family` must be 'poisson', 'binomial' or 'gaussian'.
#' @return Nothing; this branch always raises.
#' @export
.schab_variance_function <- function(mu, family) {
  mu <- as.numeric(mu) # v(mu) in eq (6.74)
  if (identical(family, "poisson")) {
    return(mu)
  }
  if (identical(family, "binomial")) {
    return(mu * (1 - mu))
  }
  if (identical(family, "gaussian")) {
    return(rep(1, length(mu)))
  }
  stop("`family` must be 'poisson', 'binomial' or 'gaussian'", call. = FALSE)
}

#' .schab_canonical_link
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_fit_pseudo_likelihood}, \code{spglmm}, \code{sppql}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param family The body requires: unknown family.
#' @return The value of \code{switch}.
#' @export
.schab_canonical_link <- function(family) {
  switch(family,
    poisson = "log",
    binomial = "logit",
    gaussian = "identity",
    stop("unknown family", call. = FALSE)
  )
}

# --- Sec. 6.3.4, the conditional specification ----------------------------

#' .schab_conditional_mean
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spglmm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{\%*\%}.
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @param S Coerced to numeric by the body, with \code{as.numeric}.
#' @param link_kind Passed to \code{.schab_link}. Defaults to \code{"log"}.
#' @return The value of \code{.schab_link}.
#' @export
.schab_conditional_mean <- function(X, beta, S, link_kind = "log") {
  X <- as.matrix(X) # eq (6.73)
  .schab_link(as.numeric(X %*% as.numeric(beta)) + as.numeric(S),
    link_kind,
    inverse = TRUE
  )
}

#' .schab_conditional_variance
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spglmm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param mu Passed to \code{.schab_variance_function}.
#' @param sigma2 Coerced to numeric by the body, with \code{as.numeric}.
#' @param family Passed to \code{.schab_variance_function}.
#' @return A numeric value.
#' @export
.schab_conditional_variance <- function(mu, sigma2, family) {
  as.numeric(sigma2) * .schab_variance_function(mu, family) # eq (6.74)
}

#' .schab_marginal_moments_lognormal
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spglmm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{\%*\%}.
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @param sigma2_S Coerced to numeric by the body, with \code{as.numeric}.
#' @param sigma2 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param rho Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return The value of \code{out}, as built in the body.
#' @export
.schab_marginal_moments_lognormal <- function(X, beta, sigma2_S, sigma2 = 1,
                                              rho = NULL) {
  # Example 6.6. NOTE the second variance term carries m(s)^2: the printed
  # text renders it with m(s), but the book's own covariance expression on
  # the next line reduces to the squared form at i = j.
  X <- as.matrix(X)
  m <- as.numeric(exp(X %*% as.numeric(beta)))
  s2 <- as.numeric(sigma2_S)
  out <- list(
    m = m,
    mean = m * exp(s2 / 2),
    variance = m * as.numeric(sigma2) * exp(s2 / 2) +
      m^2 * exp(s2) * (exp(s2) - 1)
  )
  if (!is.null(rho)) {
    r <- as.matrix(rho)
    out$covariance <- outer(m, m) * exp(s2) * (exp(s2 * r) - 1)
  }
  out
}

#' G^-1(x\'beta) -- what the marginal mean is NOT, in a GLMM
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spglmm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{\%*\%}.
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @param link_kind Passed to \code{.schab_link}. Defaults to \code{"log"}.
#' @return The value of \code{.schab_link}.
#' @export
.schab_naive_marginal_mean <- function(X, beta, link_kind = "log") {
  # g^-1(x'beta) -- what the marginal mean is NOT, in a GLMM.
  X <- as.matrix(X)
  .schab_link(as.numeric(X %*% as.numeric(beta)), link_kind, inverse = TRUE)
}

# --- Sec. 6.3.5, pseudo-likelihood ----------------------------------------

#' .schab_pseudo_data
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_fit_pseudo_likelihood}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @param mu Numeric; combined arithmetically in the body.
#' @param link_kind Passed to \code{.schab_link}.
#' @return A numeric value.
#' @export
.schab_pseudo_data <- function(z, mu, link_kind) {
  z <- as.numeric(z)
  mu <- as.numeric(mu) # eq (6.78)
  .schab_link(mu, link_kind) + .schab_link_derivative(mu, link_kind) * (z - mu)
}

#' Eq (6.79): the covariance of the PSEUDO-data, carrying Psi^-1 on both
#'
#' sides. Distinct from .schab_data_covariance -- see .schab_pql_score.
#'
#' @param mu A vector; its length is taken.
#' @param sigma2 Coerced to numeric by the body, with \code{as.numeric}.
#' @param family Passed to \code{.schab_variance_function}.
#' @param link_kind Passed to \code{.schab_link_derivative}.
#' @param R Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A numeric value.
#' @export
.schab_sigma_mu <- function(mu, sigma2, family, link_kind, R = NULL) {
  # eq (6.79): the covariance of the PSEUDO-data, carrying Psi^-1 on both
  # sides. Distinct from .schab_data_covariance -- see .schab_pql_score.
  mu <- as.numeric(mu)
  n <- length(mu)
  psi_inv <- .schab_link_derivative(mu, link_kind)
  v_half <- sqrt(.schab_variance_function(mu, family))
  if (is.null(R)) R <- diag(n)
  d <- psi_inv * v_half
  as.numeric(sigma2) * (d %o% d) * as.matrix(R)
}

#' Sigma^2 V^1/2 R V^1/2, on the DATA scale. Sec. 6.3.5.3 writes its
#' score
#'
#' equations with the symbol Sigma_mu, but the matrix they need is this
#' one; with (6.79) as written the scores are wrong by a factor of
#' Psi^2.
#'
#' @param mu A vector; its length is taken.
#' @param sigma2 Coerced to numeric by the body, with \code{as.numeric}.
#' @param family Passed to \code{.schab_variance_function}.
#' @param R Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A numeric value.
#' @export
.schab_data_covariance <- function(mu, sigma2, family, R = NULL) {
  # sigma^2 V^1/2 R V^1/2, on the DATA scale. Sec. 6.3.5.3 writes its score
  # equations with the symbol Sigma_mu, but the matrix they need is this
  # one; with (6.79) as written the scores are wrong by a factor of Psi^2.
  mu <- as.numeric(mu)
  n <- length(mu)
  v_half <- sqrt(.schab_variance_function(mu, family))
  if (is.null(R)) R <- diag(n)
  as.numeric(sigma2) * (v_half %o% v_half) * as.matrix(R)
}

#' .schab_gls_beta
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_fit_pseudo_likelihood}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{t}.
#' @param Sigma_nu A matrix; passed to \code{as.matrix}.
#' @param nu A matrix; passed to \code{\%*\%}.
#' @return A list with \code{beta}, \code{cov_beta}.
#' @export
.schab_gls_beta <- function(X, Sigma_nu, nu) {
  X <- as.matrix(X)
  nu <- as.numeric(nu) # eq (6.80)
  sinv <- solve(as.matrix(Sigma_nu))
  xsx <- t(X) %*% sinv %*% X
  list(
    beta = as.numeric(solve(xsx, t(X) %*% sinv %*% nu)),
    cov_beta = solve(xsx)
  )
}

#' .schab_predict_random_field
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_fit_pseudo_likelihood}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Sigma_S A matrix; passed to \code{as.matrix}.
#' @param Sigma_nu A matrix; passed to \code{as.matrix}.
#' @param nu Coerced to numeric by the body, with \code{as.numeric}.
#' @param X A matrix; passed to \code{as.matrix}.
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
.schab_predict_random_field <- function(Sigma_S, Sigma_nu, nu, X, beta) {
  resid <- as.numeric(nu) - as.numeric(as.matrix(X) %*% as.numeric(beta))
  as.numeric(as.matrix(Sigma_S) %*% solve(as.matrix(Sigma_nu), resid)) # (6.81)
}

#' .schab_reml_objective
#'
#' A step of the schab_glmm_shared implementation. Called by \code{sppql}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param Sigma_nu A matrix; passed to \code{as.matrix}.
#' @param nu A matrix; passed to \code{\%*\%}.
#' @return A vector, from \code{as.numeric}.
#' @export
.schab_reml_objective <- function(X, Sigma_nu, nu) {
  X <- as.matrix(X)
  nu <- as.numeric(nu)
  S <- as.matrix(Sigma_nu) # (6.84)
  n <- nrow(X)
  k <- ncol(X)
  ds <- determinant(S, logarithm = TRUE)
  if (ds$sign <= 0) {
    return(Inf)
  }
  sinv <- solve(S)
  xsx <- t(X) %*% sinv %*% X
  dx <- determinant(xsx, logarithm = TRUE)
  if (dx$sign <= 0) {
    return(Inf)
  }
  beta <- solve(xsx, t(X) %*% sinv %*% nu)
  r <- nu - as.numeric(X %*% beta)
  as.numeric(ds$modulus + dx$modulus + t(r) %*% sinv %*% r +
    (n - k) * log(2 * pi))
}

#' .schab_initial_mu
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_fit_pseudo_likelihood}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Coerced to numeric by the body, with \code{as.numeric}.
#' @param family Passed to \code{identical}.
#' @return The value of \code{z}, as built in the body.
#' @export
.schab_initial_mu <- function(z, family) {
  z <- as.numeric(z)
  if (identical(family, "poisson")) {
    return(pmax(z, 0.25))
  }
  if (identical(family, "binomial")) {
    return(pmin(pmax(z, 1e-3), 1 - 1e-3))
  }
  z
}

#' .schab_fit_pseudo_likelihood
#'
#' A step of the schab_glmm_shared implementation. Called by \code{sppql}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A vector; its length is taken.
#' @param X A matrix; passed to \code{nrow}.
#' @param Sigma_S A matrix; passed to \code{dim}.
#' @param family Passed to \code{.schab_canonical_link}. Defaults to \code{"poisson"}.
#' @param link_kind Optional; may be \code{NULL}. Passed to \code{.schab_pseudo_data}.
#' @param sigma2 Passed to \code{.schab_sigma_mu}. Defaults to \code{1}.
#' @param R Passed to \code{.schab_sigma_mu}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-08}.
#' @return A list with \code{beta}, \code{S}, \code{mu}, \code{sigma2}, \code{cov_beta}, \code{se_beta}, \code{Sigma_nu}, \code{pseudo_data}, \code{n_iter}, \code{converged}, \code{link}, \code{family}.
#' @export
.schab_fit_pseudo_likelihood <- function(z, X, Sigma_S, family = "poisson",
                                         link_kind = NULL, sigma2 = 1,
                                         R = NULL, max_iter = 100L,
                                         tol = 1e-8) {
  # The six-step algorithm of Sec. 6.3.5.2, verbatim.
  z <- as.numeric(z)
  X <- as.matrix(X)
  Sigma_S <- as.matrix(Sigma_S)
  if (is.null(link_kind)) link_kind <- .schab_canonical_link(family)
  n <- nrow(X)
  k <- ncol(X)
  if (length(z) != n || !all(dim(Sigma_S) == c(n, n))) {
    stop("`z`, `X` and `Sigma_S` must agree on the sample size", call. = FALSE)
  }
  mu <- .schab_initial_mu(z, family) # step 1
  beta <- rep(0, k)
  S_hat <- rep(0, n)
  converged <- FALSE
  sigma2_hat <- as.numeric(sigma2)
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    nu <- .schab_pseudo_data(z, mu, link_kind) # step 2
    Sig_mu <- .schab_sigma_mu(mu, sigma2, family, link_kind, R = R)
    Sigma_nu <- Sigma_S + Sig_mu
    g <- .schab_gls_beta(X, Sigma_nu, nu) # step 4
    S_new <- .schab_predict_random_field(Sigma_S, Sigma_nu, nu, X, g$beta)
    resid <- nu - as.numeric(X %*% g$beta)
    sigma2_hat <- as.numeric(t(resid) %*% solve(Sigma_nu, resid) / n) # (6.82)
    mu_new <- .schab_link(as.numeric(X %*% g$beta) + S_new,
      link_kind,
      inverse = TRUE
    ) # step 5
    delta <- max(max(abs(g$beta - beta)), max(abs(S_new - S_hat)))
    beta <- g$beta
    S_hat <- S_new
    mu <- mu_new
    if (delta < tol) {
      converged <- TRUE
      break
    }
  }
  nu <- .schab_pseudo_data(z, mu, link_kind)
  Sigma_nu <- Sigma_S + .schab_sigma_mu(mu, sigma2, family, link_kind, R = R)
  g <- .schab_gls_beta(X, Sigma_nu, nu)
  list(
    beta = beta, S = S_hat, mu = mu, sigma2 = sigma2_hat,
    cov_beta = g$cov_beta, se_beta = sqrt(diag(g$cov_beta)),
    Sigma_nu = Sigma_nu, pseudo_data = nu, n_iter = it,
    converged = converged, link = link_kind, family = family
  )
}

#' .schab_pql_score
#'
#' A step of the schab_glmm_shared implementation. Called by \code{sppql}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @param X A matrix; passed to \code{t}.
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @param S A matrix; passed to \code{solve}.
#' @param Sigma_S A matrix; passed to \code{as.matrix}.
#' @param family Passed to \code{.schab_data_covariance}.
#' @param link_kind Passed to \code{.schab_link}.
#' @param sigma2 Passed to \code{.schab_data_covariance}. Defaults to \code{1}.
#' @param R Passed to \code{.schab_data_covariance}.
#' @return A list with \code{score_beta}, \code{score_S}.
#' @export
.schab_pql_score <- function(z, X, beta, S, Sigma_S, family, link_kind,
                             sigma2 = 1, R = NULL) {
  # Sec. 6.3.5.3 first-order conditions, with the DATA-scale covariance.
  z <- as.numeric(z)
  X <- as.matrix(X)
  S <- as.numeric(S)
  mu <- .schab_link(as.numeric(X %*% as.numeric(beta)) + S,
    link_kind,
    inverse = TRUE
  )
  psi <- .schab_mu_eta(mu, link_kind)
  sig_inv <- solve(.schab_data_covariance(mu, sigma2, family, R = R))
  common <- as.numeric(psi * (sig_inv %*% (z - mu)))
  list(
    score_beta = as.numeric(t(X) %*% common),
    score_S = common - as.numeric(solve(as.matrix(Sigma_S), S))
  )
}

# --- Sec. 6.3.6, prediction -----------------------------------------------

#' Eq (6.90) with its own MSPE (6.91), kept apart from the inverse-link
#'
#' predictor (6.87), whose delta-method variance (6.88) the text says
#' belongs to a different predictor.
#'
#' @param nu0_hat Coerced to numeric by the body, with \code{as.numeric}.
#' @param sigma2_nu0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param mu0_hat Coerced to numeric by the body, with \code{as.numeric}.
#' @param link_kind Passed to \code{.schab_link_derivative}.
#' @return A list with \code{prediction}, \code{mspe}, \code{prediction_error}, \code{inverse_link_prediction}, \code{pseudo_scale_prediction}, \code{pseudo_scale_mspe}, \code{mspe_is_for}.
#' @export
.schab_predict_glm <- function(nu0_hat, sigma2_nu0, mu0_hat, link_kind) {
  # eq (6.90) with its own MSPE (6.91), kept apart from the inverse-link
  # predictor (6.87), whose delta-method variance (6.88) the text says
  # belongs to a different predictor.
  nu0 <- as.numeric(nu0_hat)
  s2 <- as.numeric(sigma2_nu0)
  mu0 <- as.numeric(mu0_hat)
  gprime <- .schab_link_derivative(mu0, link_kind)
  dmu_deta <- 1 / gprime
  list(
    prediction = mu0 + (nu0 - .schab_link(mu0, link_kind)) / gprime,
    mspe = dmu_deta^2 * s2,
    prediction_error = sqrt(dmu_deta^2 * s2),
    inverse_link_prediction = .schab_link(nu0, link_kind, inverse = TRUE),
    pseudo_scale_prediction = nu0, pseudo_scale_mspe = s2,
    mspe_is_for = paste(
      "eq (6.90), the linearised predictor -- NOT the",
      "inverse-link predictor of eq (6.87)"
    )
  )
}

# --- CAR family -----------------------------------------------------------

#' .schab_neighbour_structure
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_bym_icar_log_prior}, \code{.schab_bym_map}, \code{spbayr}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param adjacency A matrix; passed to \code{as.matrix}.
#' @return A numeric value.
#' @export
.schab_neighbour_structure <- function(adjacency) {
  A <- as.matrix(adjacency) # R_ii = n_i, R_ij = -1
  if (nrow(A) != ncol(A)) stop("`adjacency` must be square", call. = FALSE)
  if (!isTRUE(all.equal(A, t(A)))) {
    stop("`adjacency` must be symmetric", call. = FALSE)
  }
  if (any(diag(A) != 0)) {
    stop("`adjacency` must have a zero diagonal (no self-neighbours)",
      call. = FALSE
    )
  }
  if (!all(A %in% c(0, 1))) {
    stop("`adjacency` must be a 0/1 matrix", call. = FALSE)
  }
  diag(rowSums(A)) - A
}

#' Sigma^2 R^-, the Moore-Penrose inverse: R is singular by construction
#'
#' A step of the schab_glmm_shared implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param R A matrix; passed to \code{as.matrix}.
#' @param sigma2 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.schab_icar_covariance <- function(R, sigma2 = 1) {
  # sigma^2 R^-, the Moore-Penrose inverse: R is singular by construction.
  R <- as.matrix(R)
  e <- eigen(R, symmetric = TRUE)
  tol <- max(abs(e$values)) * length(e$values) * .Machine$double.eps
  inv <- ifelse(abs(e$values) > tol, 1 / e$values, 0)
  as.numeric(sigma2) * (e$vectors %*% diag(inv) %*% t(e$vectors))
}

#' .schab_icar_full_conditional
#'
#' A step of the schab_glmm_shared implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param u A matrix; passed to \code{\%*\%}.
#' @param adjacency A matrix; passed to \code{as.matrix}.
#' @param sigma2 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{mean}, \code{variance}, \code{n_neighbours}.
#' @export
.schab_icar_full_conditional <- function(u, adjacency, sigma2 = 1) {
  u <- as.numeric(u)
  A <- as.matrix(adjacency) # eq (5)/(4.3)
  n_i <- rowSums(A)
  if (any(n_i == 0)) {
    stop("every area must have at least one neighbour", call. = FALSE)
  }
  list(
    mean = as.numeric(A %*% u) / n_i, variance = as.numeric(sigma2) / n_i,
    n_neighbours = n_i
  )
}

#' .schab_lcar_precision
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spbayr}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param R A matrix; passed to \code{nrow}.
#' @param rho Numeric; combined arithmetically in the body.
#' @param sigma2 The body requires: `sigma2` must be positive. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.schab_lcar_precision <- function(R, rho, sigma2 = 1) {
  R <- as.matrix(R)
  rho <- as.numeric(rho) # eq (6)
  if (rho < 0 || rho > 1) stop("`rho` must lie in [0, 1]", call. = FALSE)
  if (sigma2 <= 0) stop("`sigma2` must be positive", call. = FALSE)
  rho * R + (1 - rho) * diag(nrow(R))
}

#' .schab_lcar_full_conditional
#'
#' A step of the schab_glmm_shared implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param u A matrix; passed to \code{\%*\%}.
#' @param adjacency A matrix; passed to \code{as.matrix}.
#' @param rho Numeric; combined arithmetically in the body.
#' @param sigma2 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{mean}, \code{variance}, \code{n_neighbours}.
#' @export
.schab_lcar_full_conditional <- function(u, adjacency, rho, sigma2 = 1) {
  u <- as.numeric(u)
  A <- as.matrix(adjacency)
  rho <- as.numeric(rho)
  if (rho < 0 || rho > 1) stop("`rho` must lie in [0, 1]", call. = FALSE)
  n_i <- rowSums(A) # eq (7)
  denom <- (1 - rho) + n_i * rho
  list(
    mean = rho * as.numeric(A %*% u) / denom,
    variance = as.numeric(sigma2) / denom, n_neighbours = n_i
  )
}

#' .schab_bym_convolution
#'
#' A step of the schab_glmm_shared implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param u A vector; its length is taken.
#' @param v A vector; its length is taken.
#' @return A numeric value.
#' @export
.schab_bym_convolution <- function(u, v) {
  u <- as.numeric(u)
  v <- as.numeric(v)
  if (length(u) != length(v)) {
    stop("`u` and `v` must have the same length", call. = FALSE)
  }
  u + v
}

#' .schab_bym_identifiability_note
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spbym}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
.schab_bym_identifiability_note <- function() {
  paste(
    "only u + v enters the likelihood, so sigma_u^2 and sigma_v^2 are not",
    "separately identifiable from the data; informative hyperpriors are",
    "required, or use the Leroux LCAR prior, which nests the exchangeable",
    "(rho=0) and ICAR (rho=1) cases in one identifiable parameter"
  )
}

#' .schab_smr
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spbayr}, \code{spbym}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param counts Coerced to numeric by the body, with \code{as.numeric}.
#' @param expected Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.schab_smr <- function(counts, expected) {
  z <- as.numeric(counts)
  e <- as.numeric(expected)
  if (length(z) != length(e)) {
    stop("`counts` and `expected` must have the same length", call. = FALSE)
  }
  if (any(e <= 0)) stop("`expected` counts must be positive", call. = FALSE)
  z / e
}

#' .schab_poisson_disease_mean
#'
#' A step of the schab_glmm_shared implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param expected Coerced to numeric by the body, with \code{as.numeric}.
#' @param X A matrix; passed to \code{as.matrix}.
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @param psi Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.schab_poisson_disease_mean <- function(expected, X, beta, psi) {
  as.numeric(expected) *
    exp(as.numeric(as.matrix(X) %*% as.numeric(beta)) + as.numeric(psi))
}

# --- Besag, York & Mollie (1991) Sec. 4 -----------------------------------

#' .schab_bym_icar_log_prior
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_bym_log_posterior}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param u A matrix; passed to \code{t}.
#' @param adjacency Passed to \code{.schab_neighbour_structure}.
#' @param kappa Numeric; passed to \code{log}.
#' @return A numeric value.
#' @export
.schab_bym_icar_log_prior <- function(u, adjacency, kappa) {
  u <- as.numeric(u)
  R <- .schab_neighbour_structure(adjacency) # eq (4.2)
  kappa <- as.numeric(kappa)
  if (kappa <= 0) stop("`kappa` must be positive", call. = FALSE)
  -0.5 * length(u) * log(kappa) - as.numeric(t(u) %*% R %*% u) / (2 * kappa)
}

#' .schab_bym_median_log_prior
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spbym}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param u A vector; its length is taken and its elements indexed.
#' @param adjacency A matrix; passed to \code{as.matrix}.
#' @param kappa Numeric; passed to \code{log}.
#' @return A numeric value.
#' @export
.schab_bym_median_log_prior <- function(u, adjacency, kappa) {
  u <- as.numeric(u)
  A <- as.matrix(adjacency) # eq (4.4)
  kappa <- as.numeric(kappa)
  if (kappa <= 0) stop("`kappa` must be positive", call. = FALSE)
  idx <- which(upper.tri(A) & A > 0, arr.ind = TRUE)
  total <- if (nrow(idx)) sum(abs(u[idx[, 1]] - u[idx[, 2]])) else 0
  -length(u) * log(kappa) - total / kappa
}

#' .schab_bym_log_posterior
#'
#' A step of the schab_glmm_shared implementation. Called by \code{.schab_bym_map}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y A vector; its length is taken.
#' @param c_exp Coerced to numeric by the body, with \code{as.numeric}.
#' @param u A vector; its length is taken.
#' @param v A vector; its length is taken.
#' @param kappa Numeric; combined arithmetically in the body.
#' @param lam Numeric; passed to \code{log}.
#' @param adjacency Passed to \code{.schab_bym_icar_log_prior}.
#' @param epsilon Numeric; combined arithmetically in the body. Defaults to \code{0.01}.
#' @return A numeric value.
#' @export
.schab_bym_log_posterior <- function(y, c_exp, u, v, kappa, lam, adjacency,
                                     epsilon = 0.01) {
  y <- as.numeric(y)
  cc <- as.numeric(c_exp) # eq (4.5)
  u <- as.numeric(u)
  v <- as.numeric(v)
  if (length(unique(c(length(y), length(cc), length(u), length(v)))) != 1L) {
    stop("`y`, `c`, `u` and `v` must have the same length", call. = FALSE)
  }
  if (any(cc <= 0)) stop("`c` (expected counts) must be positive", call. = FALSE)
  kappa <- as.numeric(kappa)
  lam <- as.numeric(lam)
  if (kappa <= 0 || lam <= 0) {
    stop("`kappa` and `lam` must be positive", call. = FALSE)
  }
  n <- length(y)
  x <- u + v
  loglik <- sum(-cc * exp(x) + y * (log(cc) + x))
  loglik + .schab_bym_icar_log_prior(u, adjacency, kappa) -
    0.5 * n * log(lam) - sum(v^2) / (2 * lam) -
    epsilon / (2 * kappa) - epsilon / (2 * lam) # (4.6)
}

#' .schab_bym_map
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spbym}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y A vector; its length is taken.
#' @param c_exp Coerced to numeric by the body, with \code{as.numeric}.
#' @param adjacency Passed to \code{.schab_neighbour_structure}.
#' @param kappa Numeric; combined arithmetically in the body.
#' @param lam Numeric; combined arithmetically in the body.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-11}.
#' @return A list with \code{u}, \code{v}, \code{x}, \code{relative_risk}, \code{fitted}, \code{n_iter}, \code{converged}, \code{sum_v}, \code{fitted_total}, \code{observed_total}, \code{log_posterior}.
#' @export
.schab_bym_map <- function(y, c_exp, adjacency, kappa, lam, max_iter = 200L,
                           tol = 1e-11) {
  # Conditional MAP of u and v by Newton. The paper states the log posterior
  # is "a strictly concave differentiable function of u and v and therefore
  # possesses a single maximum", so there is one optimum.
  #
  # Two identities come free and the paper states both: sum v* = 0 and
  # sum c_i exp(u*_i+v*_i) = sum y_i. Summing the u-gradient annihilates the
  # ICAR term because the structure matrix has zero row sums.
  y <- as.numeric(y)
  cc <- as.numeric(c_exp)
  R <- .schab_neighbour_structure(adjacency)
  n <- length(y)
  if (length(cc) != n || nrow(R) != n) {
    stop("`y`, `c` and `adjacency` must agree on the number of areas",
      call. = FALSE
    )
  }
  if (any(cc <= 0)) stop("`c` (expected counts) must be positive", call. = FALSE)
  kappa <- as.numeric(kappa)
  lam <- as.numeric(lam)
  if (kappa <= 0 || lam <= 0) {
    stop("`kappa` and `lam` must be positive", call. = FALSE)
  }
  u <- rep(0, n)
  v <- rep(0, n)
  I <- diag(n)
  converged <- FALSE
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    w <- cc * exp(u + v)
    g_u <- y - w - as.numeric(R %*% u) / kappa
    g_v <- y - w - v / lam
    H <- rbind(
      cbind(-diag(w) - R / kappa, -diag(w)),
      cbind(-diag(w), -diag(w) - I / lam)
    )
    step <- solve(H, c(g_u, g_v))
    u_new <- u - step[seq_len(n)]
    v_new <- v - step[n + seq_len(n)]
    delta <- max(abs(c(u_new - u, v_new - v)))
    u <- u_new
    v <- v_new
    if (delta < tol) {
      converged <- TRUE
      break
    }
  }
  x <- u + v
  fitted <- cc * exp(x)
  list(
    u = u, v = v, x = x, relative_risk = exp(x), fitted = fitted,
    n_iter = it, converged = converged, sum_v = sum(v),
    fitted_total = sum(fitted), observed_total = sum(y),
    log_posterior = .schab_bym_log_posterior(
      y, cc, u, v, kappa, lam,
      adjacency
    )
  )
}

# --- temporal and space-time structures -----------------------------------

#' .schab_random_walk_structure
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spbayr}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param n_time Coerced to integer by the body, with \code{as.integer}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1L}.
#' @return The value of \code{%*%}.
#' @export
.schab_random_walk_structure <- function(n_time, order = 1L) {
  T_ <- as.integer(n_time)
  k <- as.integer(order)
  if (!k %in% c(1L, 2L)) stop("`order` must be 1 or 2", call. = FALSE)
  if (T_ <= k) {
    stop(sprintf("need more than %d time points for an RW%d", k, k),
      call. = FALSE
    )
  }
  D <- matrix(0, T_ - k, T_)
  row <- if (k == 1L) c(-1, 1) else c(1, -2, 1)
  for (i in seq_len(T_ - k)) D[i, i:(i + k)] <- row
  t(D) %*% D
}

#' .schab_interaction_structure
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spbayr}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param R_space A matrix; passed to \code{as.matrix}.
#' @param R_time A matrix; passed to \code{as.matrix}.
#' @param kind The body requires: `kind` must be one of I, II, III, IV.
#' @return A list with \code{structure}, \code{kind}, \code{rank}, \code{rank_deficiency}, \code{n_constraints_required}.
#' @export
.schab_interaction_structure <- function(R_space, R_time, kind) {
  kinds <- c("I", "II", "III", "IV")
  if (!kind %in% kinds) {
    stop("`kind` must be one of I, II, III, IV", call. = FALSE)
  }
  Rs <- as.matrix(R_space)
  Rt <- as.matrix(R_time)
  Is <- diag(nrow(Rs))
  It <- diag(nrow(Rt))
  M <- switch(kind,
    I = kronecker(Is, It),
    II = kronecker(Is, Rt),
    III = kronecker(Rs, It),
    IV = kronecker(Rs, Rt)
  )
  rk <- qr(M)$rank
  list(
    structure = M, kind = kind, rank = rk,
    rank_deficiency = nrow(M) - rk, n_constraints_required = nrow(M) - rk
  )
}

#' .schab_null_space_constraints
#'
#' A step of the schab_glmm_shared implementation. Called by \code{spbayr}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param R_delta A matrix; passed to \code{as.matrix}.
#' @param tol Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{A}, \code{e}, \code{n_constraints}, \code{rank_deficiency}.
#' @export
.schab_null_space_constraints <- function(R_delta, tol = NULL) {
  M <- as.matrix(R_delta) # eq (12)
  e <- eigen(M, symmetric = TRUE)
  scale <- max(max(abs(e$values)), 1)
  if (is.null(tol)) tol <- 1e-10 * scale * nrow(M)
  null <- abs(e$values) <= tol
  A <- t(e$vectors[, null, drop = FALSE])
  list(
    A = A, e = rep(0, nrow(A)), n_constraints = nrow(A),
    rank_deficiency = sum(null)
  )
}

#' .schab_apply_sum_to_zero
#'
#' A step of the schab_glmm_shared implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param delta Coerced to numeric by the body, with \code{as.numeric}.
#' @param A A matrix; passed to \code{nrow}.
#' @return A numeric value.
#' @export
.schab_apply_sum_to_zero <- function(delta, A) {
  d <- as.numeric(delta)
  A <- as.matrix(A)
  if (nrow(A) == 0L) {
    return(d)
  }
  d - as.numeric(t(A) %*% solve(A %*% t(A), A %*% d))
}

#' .schab_linear_trend_log_risk
#'
#' A step of the schab_glmm_shared implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param u A vector; its length is taken.
#' @param beta_t Coerced to numeric by the body, with \code{as.numeric}.
#' @param delta_i Coerced to numeric by the body, with \code{as.numeric}.
#' @param times Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.schab_linear_trend_log_risk <- function(alpha, u, beta_t, delta_i, times) {
  u <- as.numeric(u)
  d <- as.numeric(delta_i)
  t_ <- as.numeric(times)
  if (length(u) != length(d)) {
    stop("`u` and `delta_i` must have one entry per area", call. = FALSE)
  }
  outer(as.numeric(alpha) + u, rep(1, length(t_))) +
    outer(as.numeric(beta_t) + d, t_) # eq (9)
}

#' .schab_nonparametric_log_risk
#'
#' A step of the schab_glmm_shared implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param u Numeric; combined arithmetically in the body.
#' @param phi A vector; its length is taken.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}.
#' @param delta Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return The value of \code{out}, as built in the body.
#' @export
.schab_nonparametric_log_risk <- function(alpha, u, phi, gamma, delta = NULL) {
  u <- as.numeric(u)
  phi <- as.numeric(phi)
  gam <- as.numeric(gamma)
  if (length(phi) != length(gam)) {
    stop("`phi` and `gamma` must have one entry per time point", call. = FALSE)
  }
  out <- outer(as.numeric(alpha) + u, phi + gam, "+") # eq (10)
  if (!is.null(delta)) {
    D <- as.matrix(delta)
    if (!all(dim(D) == dim(out))) {
      stop(sprintf("`delta` must be %d x %d", nrow(out), ncol(out)),
        call. = FALSE
      )
    }
    out <- out + D
  }
  out
}
