# MVSML chapters 13-15 (CNN, functional regression, ZAP random forest)
# and the marginal structural models of Robins, Hernan & Brumback (2000).
# Mirrors morie.fn._gp_core (Python) function for function.

#' Convolutional feature map (MVSML eq. 13.1-13.2)
#'
#' Each hidden unit computes w'x + b over its receptive field only, and
#' every unit in the map shares the same weights, which is what gives a
#' convolutional layer its parameter reduction and translational
#' invariance.
#' @param image numeric array, height x width or height x width x channels
#' @param kernel numeric array of the same channel depth as `image`
#' @param bias scalar bias added to every receptive field
#' @param stride step between receptive fields
#' @param activation optional "relu" or "logistic"
#' @return list with `feature_map`, `output_shape` and `n_parameters`
#' @noRd
morie_conv2d <- function(image, kernel, bias = 0, stride = 1,
                         activation = NULL) {
  # eq. (13.1): each unit sees w'x + b over its receptive field only,
  # and every unit shares the same w (weight sharing), which is what
  # buys translational invariance and the parameter reduction on p.551.
  img <- as.array(image)
  if (length(dim(img)) == 2L) dim(img) <- c(dim(img), 1L)
  ker <- as.array(kernel)
  if (length(dim(ker)) == 2L) dim(ker) <- c(dim(ker), 1L)
  kh <- dim(ker)[1]
  kw <- dim(ker)[2]
  kc <- dim(ker)[3]
  oh <- (dim(img)[1] - kh) %/% stride + 1L
  ow <- (dim(img)[2] - kw) %/% stride + 1L
  out <- matrix(0, oh, ow)
  for (i in seq_len(oh)) {
    for (j in seq_len(ow)) {
      r0 <- (i - 1L) * stride
      c0 <- (j - 1L) * stride
      s <- bias
      for (a in seq_len(kh)) {
        for (b in seq_len(kw)) {
          for (d in seq_len(kc)) {
            s <- s + img[r0 + a, c0 + b, d] * ker[a, b, d]
          }
        }
      }
      out[i, j] <- s
    }
  }
  if (identical(activation, "relu")) {
    out[out < 0] <- 0
  } else if (identical(activation, "logistic")) out <- 1 / (1 + exp(-out))
  list(
    feature_map = out, output_shape = c(oh, ow),
    n_parameters = kh * kw * kc + 1L
  )
}

#' Functional basis matrix (MVSML eq. 14.5)
#'
#' Evaluates the first `n_basis` Fourier (or monomial) basis functions
#' on the observation grid, the Psi of the basis expansion
#' x(t) = sum_l c_l phi_l(t).
#' @param t numeric grid of observation points
#' @param n_basis number of basis functions
#' @param kind "fourier" (default) or "poly"
#' @param period Fourier period; defaults to the range of `t`
#' @return numeric matrix with one row per grid point
#' @noRd
morie_fda_basis <- function(t, n_basis, kind = "fourier",
                            period = NULL) {
  # the basis expansion of eq. (14.5): x(t) = sum_l c_l phi_l(t)
  tt <- as.numeric(t)
  if (is.null(period)) period <- max(tt) - min(tt)
  if (period <= 0) period <- 1
  Psi <- matrix(0, length(tt), n_basis)
  if (identical(kind, "fourier")) {
    Psi[, 1] <- 1
    k <- 1L
    for (l in seq_len(n_basis - 1L)) {
      if (l %% 2L == 1L) {
        Psi[, l + 1L] <- sin(2 * pi * k * tt / period)
      } else {
        Psi[, l + 1L] <- cos(2 * pi * k * tt / period)
        k <- k + 1L
      }
    }
  } else {
    for (l in seq_len(n_basis)) Psi[, l] <- tt^(l - 1L)
  }
  Psi
}

#' Basis coefficients of an observed curve (MVSML eq. 14.6)
#'
#' Least-squares projection c-hat = (Psi'Psi)^-1 Psi'x of a discretely
#' observed curve onto the basis.
#' @param Psi basis matrix from [morie_fda_basis()]
#' @param x_t observed curve values on the same grid
#' @return numeric vector of basis coefficients
#' @noRd
morie_fda_coefficients <- function(Psi, x_t) {
  # eq. (14.6): c-hat = (Psi'Psi)^-1 Psi'x, least squares onto the basis
  P <- as.matrix(Psi)
  as.numeric(morie_solve(crossprod(P), crossprod(P, as.numeric(x_t))))
}

#' Basis inner-product matrix (MVSML eq. 14.7)
#'
#' Q_{jl} = integral phi_j(t) psi_l(t) dt evaluated by the trapezoid
#' rule over the observation grid.
#' @param t numeric grid of observation points
#' @param L1 number of coefficient-function basis terms
#' @param L2 number of curve basis terms
#' @param kind basis kind, "fourier" or "poly"
#' @return an L1 x L2 numeric matrix
#' @noRd
morie_fda_inner_product <- function(t, L1, L2, kind = "fourier") {
  # eq. (14.7): Q_{jl} = integral phi_j(t) psi_l(t) dt, by the
  # trapezoid rule over the observed grid
  tt <- as.numeric(t)
  Phi <- morie_fda_basis(tt, L1, kind)
  Psi <- morie_fda_basis(tt, L2, kind)
  Q <- matrix(0, L1, L2)
  for (j in seq_len(L1)) {
    for (l in seq_len(L2)) {
      f <- Phi[, j] * Psi[, l]
      Q[j, l] <- sum(diff(tt) * (utils::head(f, -1) + utils::tail(f, -1)) / 2)
    }
  }
  Q
}

#' Functional regression design matrix (MVSML eq. 14.3, 14.9)
#'
#' X* = C Q' turns the functional predictor
#' integral beta(t) x(t) dt into an ordinary linear model in beta.
#' @inheritParams morie_fda_inner_product
#' @param X_curves matrix or list of observed curves, one per subject
#' @return list with `X_star`, `C`, `Q` and `Psi`
#' @noRd
morie_fda_design <- function(t, X_curves, L1 = 3, L2 = 5,
                             kind = "fourier") {
  # eq. (14.3)/(14.9): X* = C Q', so the functional regression
  # integral beta(t) x(t) dt becomes an ordinary linear model in beta
  tt <- as.numeric(t)
  Psi <- morie_fda_basis(tt, L2, kind)
  Q <- morie_fda_inner_product(tt, L1, L2, kind)
  Xc <- if (is.matrix(X_curves)) X_curves else do.call(rbind, X_curves)
  C <- t(apply(Xc, 1, function(r) morie_fda_coefficients(Psi, r)))
  if (!is.matrix(C)) C <- matrix(C, nrow = nrow(Xc))
  # eq. (14.9) p.581: X* = [1_n X], so the intercept column is part
  # of the design, not something the caller adds
  list(
    X_star = cbind(1, C %*% t(Q)), X = C %*% t(Q),
    C = C, Q = Q, Psi = Psi
  )
}

#' Fit a functional linear model (MVSML eq. 14.4)
#'
#' beta-hat = (X*'X*)^-1 X*'y with
#' sigma2-hat = (1/n)(y - X*beta-hat)'(y - X*beta-hat).
#' @inheritParams morie_fda_design
#' @param y response vector, one entry per curve
#' @return list with `beta`, `fitted`, `residuals`, `sigma2`, `X_star`
#' @noRd
morie_fda_fit <- function(t, X_curves, y, L1 = 3, L2 = 5,
                          kind = "fourier") {
  # eq. (14.4): beta-hat = (X*'X*)^-1 X*'y and
  # sigma2-hat = (1/n)(y - X*beta-hat)'(y - X*beta-hat)
  d <- morie_fda_design(t, X_curves, L1, L2, kind)
  Xs <- d$X_star
  yy <- as.numeric(y)
  beta <- as.numeric(morie_solve(crossprod(Xs), crossprod(Xs, yy)))
  fitted <- as.numeric(Xs %*% beta)
  resid <- yy - fitted
  list(
    beta = beta, fitted = fitted, residuals = resid,
    sigma2 = sum(resid^2) / length(yy), X_star = Xs, Q = d$Q, C = d$C
  )
}

#' Evaluate a fitted coefficient function (MVSML ch. 14)
#'
#' Reconstructs beta(t) = sum_j b_j phi_j(t) from its basis
#' coefficients.
#' @param t numeric grid of evaluation points
#' @param beta_coefs basis coefficients from [morie_fda_fit()]
#' @param L1 number of basis terms used
#' @param kind basis kind, "fourier" or "poly"
#' @return numeric vector of beta(t) values
#' @noRd
morie_fda_beta_function <- function(t, beta_coefs, L1,
                                    kind = "fourier") {
  as.numeric(morie_fda_basis(t, L1, kind) %*% as.numeric(beta_coefs))
}

#' Bayesian information criterion for a basis dimension
#'
#' BIC = -2 loglik + p log(n), used in MVSML ch. 14 to choose the
#' number of basis functions.
#' @param loglik maximised log-likelihood
#' @param n_params number of estimated parameters
#' @param n_obs number of observations
#' @return the BIC value
#' @noRd
morie_fda_bic <- function(loglik, n_params, n_obs) {
  # p.582: BIC = -2 loglik + (L + 1) log(n); the +1 is the intercept
  -2 * loglik + (n_params + 1) * log(n_obs)
}

#' Leave-one-out cross-validation for a basis dimension (eq. 14.8)
#'
#' Drops each grid point in turn, refits the basis coefficients on the
#' rest and predicts the held-out value.
#' @param t numeric grid of observation points
#' @param x_t observed curve values
#' @param L2 number of basis functions to score
#' @param kind basis kind, "fourier" or "poly"
#' @return mean squared leave-one-out prediction error
#' @noRd
morie_fda_loocv <- function(t, x_t, L2, kind = "fourier") {
  # eq. (14.8): leave one grid point out, refit the basis, predict it
  tt <- as.numeric(t)
  xx <- as.numeric(x_t)
  err <- 0
  for (i in seq_along(tt)) {
    Psi <- morie_fda_basis(tt[-i], L2, kind, period = max(tt) - min(tt))
    cf <- morie_fda_coefficients(Psi, xx[-i])
    pi_ <- morie_fda_basis(tt, L2, kind, period = max(tt) - min(tt))[i, ]
    err <- err + (xx[i] - sum(pi_ * cf))^2
  }
  err / length(tt)
}

#' Zero-altered Poisson links (MVSML eq. 15.1)
#'
#' The two nonparametric links of a ZAP model: log for the count mean
#' and logit for the probability of the zero state.
#' @param mu_pred linear predictor for the count mean
#' @param theta_pred linear predictor for the zero probability
#' @return list with `mu` and `theta`
#' @noRd
morie_zap_link <- function(mu_pred, theta_pred) {
  # eq. (15.1): the two nonparametric links of the zero-altered
  # Poisson, log for the count mean and logit for the zero part
  mu <- exp(as.numeric(mu_pred))
  th <- 1 / (1 + exp(-as.numeric(theta_pred)))
  list(mu = mu, theta = th)
}

#' Zero-truncated Poisson log-likelihood (MVSML eq. 15.2)
#'
#' The splitting criterion of a ZAP random forest; the
#' log(1 - exp(-mu)) term is the zero-truncation correction.
#' @param y_positive strictly positive counts
#' @param mu count mean; the sample mean if omitted
#' @param x unused, kept for signature parity with the Python API
#' @return the log-likelihood value
#' @noRd
morie_zap_loglik <- function(y_positive, mu = NULL, x = NULL) {
  # eq. (15.2): the zero-truncated Poisson log-likelihood used as the
  # splitting criterion; the truncation is the log(1 - exp(-mu)) term
  yy <- as.numeric(y_positive)
  if (is.null(mu)) mu <- morie_zap_mle(yy)
  mu <- max(mu, 1e-9)
  sum(yy * log(mu) - mu - lgamma(yy + 1) - log1p(-exp(-mu)))
}

#' Zero-truncated Poisson maximum likelihood estimate (MVSML p. 652)
#'
#' Solves the estimating equation sum_i Y_i+ / N+ = mu / (1 - exp(-mu))
#' by bisection.  This, not the sample mean, is the mu that the ZAP
#' random forest plugs into the splitting criterion (15.2).
#' @param y_positive strictly positive counts
#' @param tol convergence tolerance on the estimating equation
#' @param max_iter maximum number of bisection steps
#' @return the estimate of mu, or 0 when the positive mean is at most 1
#' @noRd
morie_zap_mle <- function(y_positive, tol = 1e-12,
                          max_iter = 200) {
  yy <- as.numeric(y_positive)
  if (!length(yy)) stop("need at least one positive observation")
  target <- mean(yy)
  if (target <= 1) {
    return(0)
  }
  lo <- 1e-9
  hi <- 1
  while (hi / (1 - exp(-hi)) < target && hi <= 1e6) hi <- hi * 2
  for (it in seq_len(max_iter)) {
    mid <- (lo + hi) / 2
    v <- mid / (1 - exp(-mid))
    if (abs(v - target) < tol) {
      return(mid)
    }
    if (v < target) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}

#' ZAP and ZAPC predictions (MVSML eq. 15.3-15.4)
#'
#' E[Y] = (1 - theta) mu / (1 - exp(-mu)); supplying `threshold` adds
#' the ZAPC hard classification, which predicts 0 when theta exceeds
#' the threshold and mu otherwise.  See the erratum note in the
#' function body on the mu factor the book drops.
#' @param theta_hat estimated zero probabilities
#' @param mu_hat estimated count means
#' @param threshold optional classification cut-off for ZAPC
#' @return list with `prediction`, `zero_probability` and, when
#'   `threshold` is given, `is_zero` and `prediction_classified`
#' @noRd
morie_zap_predict <- function(theta_hat, mu_hat, threshold = NULL) {
  # eq. (15.3): E[Y] = (1 - theta) mu / (1 - exp(-mu)).
  #
  # Book erratum: (15.3) as printed, and the E(Y) line on p.651, give
  # the numerator as (1 - theta) exp(-mu), dropping the mu.  The
  # book's own probability mass function on p.651, its own Var(Y) on
  # the next line (which subtracts the square of the mean *with* the
  # mu), and the estimating equation Ybar+ = mu / (1 - exp(-mu)) on
  # p.652 all show the mu belongs there; the printed form is not even
  # a count, since it decreases as mu grows.
  #
  # eq. (15.4): ZAPC_RF predicts 0 when theta-hat > threshold and the
  # estimated count mu-hat otherwise -- mu-hat, not the ZAP mean.
  th <- as.numeric(theta_hat)
  mu <- pmax(as.numeric(mu_hat), 1e-9)
  pred <- (1 - th) * mu / (1 - exp(-mu))
  out <- list(prediction = pred, zero_probability = th)
  if (!is.null(threshold)) {
    out$is_zero <- as.numeric(th > threshold)
    out$prediction_classified <- ifelse(th > threshold, 0, mu)
  }
  out
}

#' Mean and variance of the zero-altered Poisson (MVSML p. 651)
#'
#' The variance is transcribed as printed; the mean uses the corrected
#' numerator documented in [morie_zap_predict()].
#' @param theta probability of the zero state
#' @param mu Poisson mean of the count part
#' @return list with `mean` and `variance`
#' @noRd
morie_zap_mean_variance <- function(theta, mu) {
  th <- as.numeric(theta)
  m <- pmax(as.numeric(mu), 1e-9)
  k <- (1 - th) / (1 - exp(-m))
  mean_ <- k * m
  list(mean = mean_, variance = k * (m + m^2) - mean_^2)
}

# ---- marginal structural models (Robins, Hernan & Brumback 2000) ----

#' @noRd
morie_msm_design <- function(treatment_history, extra = NULL) {
  A <- if (is.matrix(treatment_history)) {
    treatment_history
  } else {
    do.call(rbind, treatment_history)
  }
  abar <- rowSums(A)
  X <- cbind(1, abar)
  if (!is.null(extra)) X <- cbind(X, as.matrix(extra))
  list(X = X, a_bar = abar)
}

#' @noRd
morie_msm_weighted_glm <- function(y, X, weights = NULL,
                                   family = "gaussian", offset = NULL,
                                   n_iter = 60, tol = 1e-10) {
  # weighted IRLS: fitting the outcome model in the pseudo-population
  # created by the IPT weights estimates the causal MSM parameter
  Xm <- as.matrix(X)
  yy <- as.numeric(y)
  n <- length(yy)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  off <- if (is.null(offset)) rep(0, n) else as.numeric(offset)
  if (identical(family, "gaussian")) {
    beta <- as.numeric(morie_solve(
      crossprod(Xm, Xm * w),
      crossprod(Xm, w * (yy - off))
    ))
    eta <- off + as.numeric(Xm %*% beta)
    mu <- eta
  } else {
    beta <- rep(0, ncol(Xm))
    if (identical(family, "poisson")) {
      beta[1] <- log(max(sum(w * yy) / max(sum(w), 1e-9), 1e-6))
    }
    for (it in seq_len(n_iter)) {
      eta <- off + as.numeric(Xm %*% beta)
      if (identical(family, "binomial")) {
        mu <- 1 / (1 + exp(-pmax(pmin(eta, 700), -700)))
        Wd <- pmax(mu * (1 - mu), 1e-9)
      } else if (identical(family, "poisson")) {
        mu <- exp(pmin(eta, 700))
        Wd <- pmax(mu, 1e-9)
      } else {
        stop("unknown family: ", family)
      }
      z <- eta - off + (yy - mu) / Wd
      ww <- w * Wd
      new <- as.numeric(morie_solve(
        crossprod(Xm, Xm * ww),
        crossprod(Xm, ww * z)
      ))
      gap <- max(abs(new - beta))
      beta <- new
      if (gap < tol) break
    }
    eta <- off + as.numeric(Xm %*% beta)
    mu <- if (identical(family, "binomial")) {
      1 / (1 + exp(-pmax(pmin(eta, 700), -700)))
    } else {
      exp(pmin(eta, 700))
    }
  }
  list(beta = beta, fitted = mu, eta = eta, weights = w, family = family)
}

#' @noRd
morie_msm_cox_weighted <- function(time, event, treatment_history,
                                   weights = NULL, n_iter = 60,
                                   tol = 1e-10) {
  # the partial likelihood weighted by the stabilized IPT weights, so
  # exp(beta) is a marginal rather than conditional hazard ratio
  ts <- as.numeric(time)
  ev <- as.numeric(event)
  d <- morie_msm_design(treatment_history)
  a <- d$a_bar
  n <- length(ts)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  beta <- 0
  for (it in seq_len(n_iter)) {
    g <- 0
    h <- 0
    for (i in order(ts)) {
      if (ev[i] <= 0) next
      risk <- which(ts >= ts[i])
      e <- w[risk] * exp(beta * a[risk])
      den <- sum(e)
      if (den <= 0) next
      m1 <- sum(a[risk] * e) / den
      m2 <- sum(a[risk]^2 * e) / den
      g <- g + w[i] * (a[i] - m1)
      h <- h + w[i] * (m2 - m1^2)
    }
    if (h <= 1e-12) break
    step <- g / h
    beta <- beta + step
    if (abs(step) < tol) break
  }
  list(beta = beta, hazard_ratio = exp(beta))
}

#' @noRd
morie_msm_gmm <- function(y, X, Z, weights = NULL) {
  # E[Z (Y - g(a-bar; beta))] = 0 with the IPT weights inside the
  # moment condition (Hansen 1982; Robins 1999)
  Xm <- as.matrix(X)
  Zm <- as.matrix(Z)
  yy <- as.numeric(y)
  w <- if (is.null(weights)) rep(1, length(yy)) else as.numeric(weights)
  ZtWX <- crossprod(Zm, Xm * w)
  ZtWy <- crossprod(Zm, w * yy)
  beta <- if (ncol(Zm) == ncol(Xm)) {
    as.numeric(morie_solve(ZtWX, ZtWy))
  } else {
    as.numeric(morie_solve(crossprod(ZtWX), crossprod(ZtWX, ZtWy)))
  }
  resid <- yy - as.numeric(Xm %*% beta)
  list(
    beta = beta, residuals = resid,
    moments = as.numeric(crossprod(Zm, w * resid)) / length(yy)
  )
}

#' Linear marginal structural model
#'
#' E[Y(a-bar)] = beta_0 + beta_a a-bar fitted by weighted least
#' squares with stabilized inverse-probability-of-treatment weights
#' (Robins, Hernan & Brumback 2000).
#' @param y outcome vector
#' @param treatment_history matrix of treatments, one row per subject
#' @param weights stabilized IPT weights (unit weights if omitted)
#' @return list with `estimate`, `beta`, `a_bar` and `fitted`
#' @noRd
morie_msm_linear <- function(y, treatment_history, weights = NULL) {
  d <- morie_msm_design(treatment_history)
  f <- morie_msm_weighted_glm(y, d$X, weights, "gaussian")
  list(
    estimate = f$beta[2], beta = f$beta, beta_a = f$beta[2],
    a_bar = d$a_bar, fitted = f$fitted
  )
}

#' Logistic marginal structural model
#'
#' logit P(Y(a-bar) = 1) = beta_0 + beta_a a-bar; exp(beta_a) is a
#' causal odds ratio per unit of cumulative treatment.
#' @inheritParams morie_msm_linear
#' @return list with `estimate`, `beta` and `odds_ratio`
#' @noRd
morie_msm_logistic <- function(y, treatment_history, weights = NULL) {
  d <- morie_msm_design(treatment_history)
  f <- morie_msm_weighted_glm(y, d$X, weights, "binomial")
  list(
    estimate = f$beta[2], beta = f$beta,
    odds_ratio = exp(f$beta[2]), fitted = f$fitted
  )
}

#' Poisson marginal structural model
#'
#' log E[Y(a-bar)] = beta_0 + beta_a a-bar; exp(beta_a) is a causal
#' rate ratio.
#' @inheritParams morie_msm_linear
#' @param offset optional log-exposure offset
#' @return list with `estimate`, `beta` and `rate_ratio`
#' @noRd
morie_msm_poisson <- function(y, treatment_history, offset = NULL,
                              weights = NULL) {
  d <- morie_msm_design(treatment_history)
  f <- morie_msm_weighted_glm(y, d$X, weights, "poisson", offset)
  list(
    estimate = f$beta[2], beta = f$beta,
    rate_ratio = exp(f$beta[2]), fitted = f$fitted
  )
}

#' Negative-binomial marginal structural model
#'
#' The Poisson MSM mean model with the overdispersed variance
#' V(mu) = mu + alpha mu^2 (Hilbe 2011).
#' @inheritParams morie_msm_poisson
#' @param alpha overdispersion parameter
#' @return list with `estimate`, `beta`, `rate_ratio` and `variance`
#' @noRd
morie_msm_negative_binomial <- function(y, treatment_history, alpha = 1,
                                        offset = NULL, weights = NULL) {
  f <- morie_msm_poisson(y, treatment_history, offset, weights)
  f$variance <- f$fitted + alpha * f$fitted^2
  f$alpha <- alpha
  f
}

#' Marginal structural Cox model
#'
#' lambda(t | a-bar) = lambda_0(t) exp(beta_a a-bar), fitted by a
#' weighted partial likelihood (Hernan, Brumback & Robins 2000).
#' @param time observed follow-up times
#' @param event event indicator (1 = event, 0 = censored)
#' @inheritParams morie_msm_linear
#' @return list with `estimate`, `beta` and `hazard_ratio`
#' @noRd
morie_msm_cox_marginal <- function(time, event, treatment_history,
                                   weights = NULL) {
  f <- morie_msm_cox_weighted(time, event, treatment_history, weights)
  list(estimate = f$beta, beta = f$beta, hazard_ratio = f$hazard_ratio)
}

#' Structural accelerated failure time model
#'
#' log T(a-bar) = beta_0 + beta_a a-bar + eps (Robins & Tsiatis 1991),
#' fitted on the log scale over the uncensored observations.
#' @inheritParams morie_msm_cox_marginal
#' @return list with `estimate`, `beta`, `time_ratio` and `n_uncensored`
#' @noRd
morie_msm_accelerated_failure <- function(time, event, treatment_history,
                                          weights = NULL) {
  ts <- as.numeric(time)
  ev <- as.numeric(event)
  d <- morie_msm_design(treatment_history)
  keep <- which(ev > 0 & ts > 0)
  if (!length(keep)) stop("need at least one uncensored positive time")
  w <- if (is.null(weights)) NULL else as.numeric(weights)[keep]
  f <- morie_msm_weighted_glm(
    log(ts[keep]), d$X[keep, , drop = FALSE],
    w, "gaussian"
  )
  list(
    estimate = f$beta[2], beta = f$beta,
    time_ratio = exp(f$beta[2]), n_uncensored = length(keep)
  )
}

#' GMM estimator for a marginal structural model
#'
#' Solves E[Z (Y - g(a-bar; beta))] = 0 with IPT-weighted moments
#' (Hansen 1982; Robins 1999).
#' @inheritParams morie_msm_linear
#' @param instruments optional instrument matrix; defaults to the design
#' @return list with `estimate`, `beta` and `moments`
#' @noRd
morie_msm_gmm_estimator <- function(y, treatment_history,
                                    instruments = NULL, weights = NULL) {
  d <- morie_msm_design(treatment_history)
  Z <- if (is.null(instruments)) d$X else cbind(1, as.matrix(instruments))
  f <- morie_msm_gmm(y, d$X, Z, weights)
  list(estimate = f$beta[2], beta = f$beta, moments = f$moments)
}

#' Time-varying exposure marginal structural model
#'
#' The stabilized weights reweight the sample into a pseudo-population
#' in which exposure is independent of the measured time-varying
#' confounders; the MSM is then fitted by weighted least squares.
#' @param y outcome vector
#' @param exposure_history matrix of exposures, one row per subject
#' @param weights stabilized IPT weights
#' @return list with `estimate`, `beta`, `weight_mean` and `weight_max`
#' @noRd
morie_msm_time_varying_exposure <- function(y, exposure_history,
                                            weights = NULL) {
  d <- morie_msm_design(exposure_history)
  f <- morie_msm_weighted_glm(y, d$X, weights, "gaussian")
  list(
    estimate = f$beta[2], beta = f$beta, a_bar = d$a_bar,
    weight_mean = mean(f$weights), weight_max = max(f$weights)
  )
}
