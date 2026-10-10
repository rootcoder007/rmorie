# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.

# Native Bayesian structural time series behind morie_causal_impact()
# (R/causal.R): local level + static regression fitted by Gibbs sampling,
# following the defaults CausalImpact (Brodersen et al. 2015) passes to
# bsts. See the morie_causal_impact() documentation for the model, the
# priors and the one simplification (normal slab instead of spike-and-slab).

# Draw sigma^2 from its inverse-gamma full conditional with an upper limit
# on sigma (bsts' SdPrior upper.limit), by rejection with a capped fallback.
#' @noRd
.ci_draw_sigma2 <- function(shape, rate, upper) {
  for (i in seq_len(100L)) {
    s2 <- 1 / stats::rgamma(1L, shape = shape, rate = rate)
    if (sqrt(s2) <= upper) {
      return(s2)
    }
  }
  upper^2
}

# Forward-filter backward-sample the local level mu_1..T given
# z_t = y_t - x_t'b (NA = missing), observation variance s2, level
# variance q and the initial-state prior N(m0, C0).
#' @noRd
.ci_ffbs <- function(z, s2, q, m0, C0) {
  n <- length(z)
  m <- numeric(n)
  C <- numeric(n)
  a <- m0
  P <- C0
  for (t in seq_len(n)) {
    if (!is.na(z[t])) {
      K <- P / (P + s2)
      a <- a + K * (z[t] - a)
      P <- P * (1 - K)
    }
    m[t] <- a
    C[t] <- P
    P <- P + q
  }
  mu <- numeric(n)
  mu[n] <- stats::rnorm(1L, m[n], sqrt(C[n]))
  if (n > 1L) {
    for (t in (n - 1L):1L) {
      g <- C[t] / (C[t] + q)
      mu[t] <- stats::rnorm(1L, m[t] + g * (mu[t + 1L] - m[t]), sqrt(C[t] * (1 - g)))
    }
  }
  mu
}

#' @noRd
.ci_bsts_gibbs <- function(y, X, niter, prior_level_sd) {
  n <- length(y)
  obs <- !is.na(y)
  sdy <- stats::sd(y[obs])
  p <- if (is.null(X)) 0L else ncol(X)
  # Priors (CausalImpact -> bsts defaults):
  #   level sd     ~ SdPrior(prior.level.sd * sdy, sample.size = 32,
  #                          upper.limit = sdy)
  #   initial mu_1 ~ N(y_1, sdy^2)
  #   obs sd       ~ SdPrior(sqrt(1 - 0.8) * sdy, sample.size = 50,
  #                          upper.limit = 1.2 sdy)  with regressors
  #                  SdPrior(sdy, sample.size = 0.01, upper 1.2 sdy) without
  #   beta | s2    ~ N(0, s2 * Omega^-1),  Omega = 0.01 * (0.5 X'X / n +
  #                  0.5 diag(X'X / n))  (bsts' slab; spike dropped)
  lev_guess <- prior_level_sd * sdy
  lev_ss <- 32
  if (p > 0L) {
    obs_guess <- sqrt(1 - 0.8) * sdy
    obs_ss <- 50
  } else {
    obs_guess <- sdy
    obs_ss <- 0.01
  }
  m0 <- y[which(obs)[1L]]
  C0 <- sdy^2
  if (p > 0L) {
    Xo <- X[obs, , drop = FALSE]
    XtX <- crossprod(Xo)
    no <- sum(obs)
    Omega <- 0.01 * (0.5 * XtX / no + 0.5 * diag(diag(XtX) / no, p))
    Prec <- XtX + Omega
    R <- chol(Prec)
  }
  beta <- numeric(p)
  s2 <- (0.5 * sdy)^2
  q <- lev_guess^2
  draws_mu <- matrix(NA_real_, niter, n)
  draws_beta <- matrix(NA_real_, niter, p)
  draws_s2 <- numeric(niter)
  draws_q <- numeric(niter)
  reg <- function(b) if (p > 0L) as.numeric(X %*% b) else numeric(n)
  for (it in seq_len(niter)) {
    xb <- reg(beta)
    mu <- .ci_ffbs(y - xb, s2, q, m0, C0)
    if (p > 0L) {
      r <- (y - mu)[obs]
      bhat <- backsolve(R, forwardsolve(t(R), crossprod(Xo, r)))
      beta <- as.numeric(bhat + backsolve(R, stats::rnorm(p)) * sqrt(s2))
      xb <- reg(beta)
    }
    res <- (y - mu - xb)[obs]
    pen <- if (p > 0L) sum(beta * (Omega %*% beta)) else 0
    s2 <- .ci_draw_sigma2(
      (obs_ss + length(res) + p) / 2,
      (obs_ss * obs_guess^2 + sum(res^2) + pen) / 2,
      1.2 * sdy
    )
    q <- .ci_draw_sigma2(
      (lev_ss + n - 1) / 2,
      (lev_ss * lev_guess^2 + sum(diff(mu)^2)) / 2,
      sdy
    )
    draws_mu[it, ] <- mu
    if (p > 0L) draws_beta[it, ] <- beta
    draws_s2[it] <- s2
    draws_q[it] <- q
  }
  list(mu = draws_mu, beta = draws_beta, sigma2_obs = draws_s2, sigma2_level = draws_q)
}

#' @noRd
.ci_native <- function(data, pre_period, post_period, model_args, alpha) {
  fn <- "morie_causal_impact"
  ma <- list(
    niter = 1000L, prior.level.sd = 0.01, standardize.data = TRUE,
    nseasons = 1L, season.duration = 1L, dynamic.regression = FALSE,
    seed = NULL
  )
  if (!is.null(model_args)) {
    bad <- setdiff(names(model_args), names(ma))
    if (length(bad)) {
      stop(sprintf(
        "%s(): model_args %s not supported by the native sampler (supported: %s).",
        fn, paste0("`", bad, "`", collapse = ", "),
        paste0("`", names(ma), "`", collapse = ", ")
      ), call. = FALSE)
    }
    ma[names(model_args)] <- model_args
  }
  if (!identical(as.integer(ma$nseasons), 1L)) {
    stop(fn, "(): seasonal components (nseasons > 1) are not supported by the native sampler.",
      call. = FALSE
    )
  }
  if (!isFALSE(ma$dynamic.regression)) {
    stop(fn, "(): dynamic.regression = TRUE is not supported by the native sampler.",
      call. = FALSE
    )
  }
  if (!(is.numeric(alpha) && length(alpha) == 1L && alpha > 0 && alpha < 1)) {
    stop(fn, "(): `alpha` must be a single number in (0, 1).", call. = FALSE)
  }
  idx <- attr(data, "index")
  M <- if (is.data.frame(data)) data.matrix(data) else as.matrix(unclass(data))
  if (is.null(dim(M))) M <- matrix(M, ncol = 1L)
  storage.mode(M) <- "double"
  n_all <- nrow(M)
  to_row <- function(v) {
    if (!is.null(idx) && !is.numeric(v)) {
      r <- match(v, idx)
    } else {
      r <- as.integer(v)
    }
    if (length(r) != 2L || anyNA(r) || r[1L] < 1L || r[2L] > n_all || r[1L] > r[2L]) {
      stop(fn, "(): pre_period / post_period must be two increasing positions within the data.",
        call. = FALSE
      )
    }
    r
  }
  pre <- to_row(pre_period)
  post <- to_row(post_period)
  if (post[1L] <= pre[2L]) {
    stop(fn, "(): post_period must start after pre_period ends.", call. = FALSE)
  }
  rows <- pre[1L]:post[2L]
  M <- M[rows, , drop = FALSE]
  n <- nrow(M)
  pre_i <- seq_len(pre[2L] - pre[1L] + 1L)
  post_i <- (post[1L] - pre[1L] + 1L):n
  y_obs <- M[, 1L]
  X <- if (ncol(M) > 1L) M[, -1L, drop = FALSE] else NULL
  if (!is.null(X) && anyNA(X)) {
    stop(fn, "(): covariates must not contain missing values.", call. = FALSE)
  }
  if (sum(!is.na(y_obs[pre_i])) < 3L) {
    stop(fn, "(): need at least 3 observed pre-period responses.", call. = FALSE)
  }
  # Standardise on the pre-period (CausalImpact's standardize.data = TRUE).
  if (isTRUE(ma$standardize.data)) {
    my <- mean(y_obs[pre_i], na.rm = TRUE)
    sy <- stats::sd(y_obs[pre_i], na.rm = TRUE)
    if (!is.finite(sy) || sy == 0) sy <- 1
  } else {
    my <- 0
    sy <- 1
  }
  ys <- (y_obs - my) / sy
  if (!is.null(X)) {
    mx <- colMeans(X[pre_i, , drop = FALSE])
    sx <- apply(X[pre_i, , drop = FALSE], 2L, stats::sd)
    keep <- is.finite(sx) & sx > 0
    X <- X[, keep, drop = FALSE]
    if (!ncol(X)) {
      X <- NULL
    } else if (isTRUE(ma$standardize.data)) {
      X <- sweep(sweep(X, 2L, mx[keep]), 2L, sx[keep], "/")
    }
  }
  y_fit <- ys
  y_fit[-pre_i] <- NA_real_
  niter <- as.integer(ma$niter)
  if (niter < 10L) stop(fn, "(): niter must be at least 10.", call. = FALSE)
  if (!is.null(ma$seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
    }
    set.seed(ma$seed)
  }
  g <- .ci_bsts_gibbs(y_fit, X, niter, ma$prior.level.sd)
  burn <- max(1L, floor(0.1 * niter))
  keep_it <- (burn + 1L):niter
  nd <- length(keep_it)
  xb <- if (is.null(X)) {
    matrix(0, nd, n)
  } else {
    g$beta[keep_it, , drop = FALSE] %*% t(X)
  }
  eps <- matrix(stats::rnorm(nd * n), nd, n) * sqrt(g$sigma2_obs[keep_it])
  ysamp <- (g$mu[keep_it, , drop = FALSE] + xb + eps) * sy + my
  # -- summary table (CausalImpact's layout) --------------------------------
  yp <- y_obs[post_i]
  if (anyNA(yp)) {
    stop(fn, "(): the post-period response must not contain missing values.",
      call. = FALSE
    )
  }
  sp <- ysamp[, post_i, drop = FALSE]
  lo_q <- alpha / 2
  hi_q <- 1 - alpha / 2
  qq <- function(v, pr) unname(stats::quantile(v, pr))
  mk <- function(actual, pred_draws) {
    pred <- mean(pred_draws)
    eff <- actual - pred_draws
    rel <- eff / pred_draws
    c(
      Actual = actual, Pred = pred,
      Pred.lower = qq(pred_draws, lo_q), Pred.upper = qq(pred_draws, hi_q),
      Pred.sd = stats::sd(pred_draws),
      AbsEffect = actual - pred,
      AbsEffect.lower = qq(eff, lo_q), AbsEffect.upper = qq(eff, hi_q),
      AbsEffect.sd = stats::sd(eff),
      RelEffect = (actual - pred) / pred,
      RelEffect.lower = qq(rel, lo_q), RelEffect.upper = qq(rel, hi_q),
      RelEffect.sd = stats::sd(rel)
    )
  }
  sums <- rowSums(sp)
  smry <- as.data.frame(rbind(
    Average = mk(mean(yp), rowMeans(sp)),
    Cumulative = mk(sum(yp), sums)
  ))
  smry$alpha <- alpha
  # Posterior tail-area probability of the observed post-period sum
  # (CausalImpact's p): one-sided, the smaller tail, (+1) corrected.
  ysum <- sum(yp)
  tail_n <- min(sum(c(sums, ysum) >= ysum), sum(c(sums, ysum) <= ysum))
  smry$p <- tail_n / (length(sums) + 1)
  # -- pointwise series over the analysed window ------------------------------
  pt <- colMeans(ysamp)
  in_post <- seq_len(n) %in% post_i
  eff_draws <- matrix(y_obs, nd, n, byrow = TRUE) - ysamp
  eff_draws[, !in_post] <- 0
  cum_draws <- t(apply(eff_draws, 1L, cumsum))
  if (n == 1L) cum_draws <- t(cum_draws)
  colq <- function(m, pr) apply(m, 2L, stats::quantile, probs = pr, names = FALSE)
  series <- data.frame(
    time = rows,
    response = y_obs,
    point.pred = pt,
    point.pred.lower = colq(ysamp, lo_q),
    point.pred.upper = colq(ysamp, hi_q),
    point.effect = y_obs - pt,
    point.effect.lower = colq(matrix(y_obs, nd, n, byrow = TRUE) - ysamp, lo_q),
    point.effect.upper = colq(matrix(y_obs, nd, n, byrow = TRUE) - ysamp, hi_q),
    cum.effect = cumsum(ifelse(in_post, y_obs - pt, 0)),
    cum.effect.lower = colq(cum_draws, lo_q),
    cum.effect.upper = colq(cum_draws, hi_q),
    post_period = in_post
  )
  beta_draws <- g$beta[keep_it, , drop = FALSE]
  if (!is.null(X)) colnames(beta_draws) <- colnames(X)
  structure(list(
    summary = smry,
    series = series,
    y_samples = sp,
    model = list(
      coefficients = beta_draws,
      sigma_obs = sqrt(g$sigma2_obs[keep_it]) * sy,
      sigma_level = sqrt(g$sigma2_level[keep_it]) * sy,
      niter = niter, burn = burn,
      standardization = list(y_mean = my, y_sd = sy),
      model_args = ma
    ),
    pre_period = pre, post_period = post, alpha = alpha
  ), class = "morie_causal_impact")
}
