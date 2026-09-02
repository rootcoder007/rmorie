# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native counterparts for morie.fn.kmperm, morie.fn.gestee and
# morie.fn.bnsadm: the XLNet permutation language-model loss, the
# differentially private clipped mean, and the two bounds that
# constrain every admissible estimator and treatment rule.
#
# .morie_bounds_logit mirrors morie.fn._did.logit_fit exactly --
# iteratively reweighted least squares on the working response, the
# same 1e-8 ridge, the same +/- 30 clip on the linear predictor -- so
# the cross-language parity anchors hold to ten significant digits.
# The Newton-Raphson fitter in R/causal_shared_native.R converges to
# the same MLE but not to the same last digits, which is why it is not
# reused here.

#' .morie_bounds_logit
#'
#' A step of the bounds_native implementation. Called by \code{morie_efficiency_bound_ate}, \code{morie_text_ate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{ncol}.
#' @param y Numeric; combined arithmetically in the body.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-10}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.
#' @return A list with \code{beta}, \code{fitted}, \code{separated}.
#' @export
.morie_bounds_logit <- function(X, y, max_iter = 100L, tol = 1e-10,
                                ridge = 1e-8) {
  beta <- rep(0, ncol(X))
  for (i in seq_len(max_iter)) {
    eta <- pmin(pmax(as.vector(X %*% beta), -30), 30)
    p <- 1 / (1 + exp(-eta))
    w <- pmax(p * (1 - p), 1e-10)
    z <- eta + (y - p) / w
    XtW <- t(X) * rep(w, each = ncol(X))
    step <- solve(XtW %*% X + ridge * diag(ncol(X)), XtW %*% z)
    step <- as.vector(step)
    done <- max(abs(step - beta)) < tol
    beta <- step
    if (done) break
  }
  eta <- pmin(pmax(as.vector(X %*% beta), -30), 30)
  p <- 1 / (1 + exp(-eta))
  list(
    beta = beta, fitted = p,
    separated = min(p) < 1e-6 || max(p) > 1 - 1e-6
  )
}

#' Two-stream attention masks induced by a factorization order
#'
#' Returns the pair of logical matrices that make XLNet's permutation
#' objective computable in a single pass over an unpermuted sequence.
#' `content\[i, j\]` is TRUE when position `i` may attend to position `j`.
#'
#' The content stream sees \eqn{z_{\le t}} -- itself included -- and the
#' query stream sees only \eqn{z_{<t}}. The single difference is the
#' diagonal, and that diagonal is the whole reason two streams exist: a
#' query able to see its own token would make predicting that token
#' trivial, while a content stream unable to see it could not encode it
#' for later positions.
#'
#' @param permutation Integer permutation of `0:(T-1)`, where
#'   `permutation\[t\]` is the sequence position generated `t`-th.
#' @return A list with `content`, `query` and `rank`.
#' @references Yang Z, Dai Z, Yang Y, Carbonell J, Salakhutdinov R,
#'   Le QV (2019) XLNet. arXiv:1906.08237, eq (3).
#' @export
#' @examples
#' morie_permutation_attention_masks(c(2L, 0L, 3L, 1L))
morie_permutation_attention_masks <- function(permutation) {
  z <- as.integer(permutation)
  T_ <- length(z)
  if (T_ < 1L) stop("permutation must not be empty.", call. = FALSE)
  if (!identical(sort(z), 0:(T_ - 1L))) {
    stop("permutation must be a permutation of 0 .. T-1.", call. = FALSE)
  }
  rank <- integer(T_)
  rank[z + 1L] <- seq_len(T_) - 1L
  content <- outer(rank, rank, ">=")
  query <- outer(rank, rank, ">")
  list(content = content, query = query, rank = rank)
}

#' Permutation language-model loss under one factorization order
#'
#' Equation (3) of the XLNet paper, negated:
#' \deqn{-\sum_{t} \log p_\theta(x_{z_t} \mid x_{z_{<t}}).}
#'
#' With the logits held fixed the full-sequence loss does NOT depend on
#' the permutation, because reordering the terms of a sum leaves the sum
#' alone. The permutation acts only on what the network was allowed to
#' condition on when producing those logits -- that is, on the masks --
#' never on this arithmetic. `order_invariant` records that, and is TRUE
#' by construction whenever the whole sequence is scored.
#'
#' Equation (5) is partial prediction: score only the trailing
#' `num_predict` positions of the factorization order, whose contexts
#' are the longest available. The scored subset then depends on the
#' permutation, so the loss does too, and `order_invariant` goes FALSE.
#'
#' @param logits Numeric matrix, `T` rows by `V` vocabulary columns.
#' @param targets Integer vector of true token ids in `0:(V-1)`.
#' @param permutation Factorization order, a permutation of `0:(T-1)`.
#' @param num_predict Optional count of trailing positions to score.
#' @param reduction One of "mean", "sum" or "none".
#' @return A list with `loss`, `token_nll`, `scored_positions`,
#'   `perplexity`, `order_invariant`, `mean_context_length`.
#' @references Yang et al (2019) arXiv:1906.08237, eq (3) and (5).
#' @export
morie_permutation_lm_loss <- function(logits, targets, permutation,
                                      num_predict = NULL,
                                      reduction = c("mean", "sum", "none")) {
  reduction <- match.arg(reduction)
  L <- as.matrix(logits)
  T_ <- nrow(L)
  V <- ncol(L)
  y <- as.integer(targets)
  if (length(y) != T_) {
    stop(sprintf(
      "targets has length %d but logits has %d positions.",
      length(y), T_
    ), call. = FALSE)
  }
  if (any(y < 0L) || any(y >= V)) {
    stop(sprintf("targets must lie in 0 .. %d.", V - 1L), call. = FALSE)
  }
  masks <- morie_permutation_attention_masks(permutation)
  z <- as.integer(permutation)
  if (length(z) != T_) {
    stop(sprintf(
      "permutation has length %d but logits has %d positions.",
      length(z), T_
    ), call. = FALSE)
  }
  m <- apply(L, 1L, max)
  d <- L - m
  logp <- d - log(rowSums(exp(d)))
  nll <- -logp[cbind(seq_len(T_), y + 1L)]

  if (is.null(num_predict)) {
    scored <- z
    partial <- FALSE
  } else {
    k <- as.integer(num_predict)
    if (k < 1L || k > T_) {
      stop(sprintf("num_predict must lie in 1 .. %d; got %d.", T_, k),
        call. = FALSE
      )
    }
    scored <- z[(T_ - k + 1L):T_]
    partial <- k < T_
  }
  sel <- nll[scored + 1L]
  loss <- switch(reduction,
    mean = mean(sel),
    sum = sum(sel),
    none = sel
  )
  list(
    loss = loss, estimate = loss, token_nll = nll,
    scored_positions = scored,
    mean_context_length = mean(masks$rank[scored + 1L]),
    perplexity = exp(mean(sel)),
    order_invariant = !partial, partial_prediction = partial,
    content_mask = masks$content, query_mask = masks$query,
    rank = masks$rank, n = T_, vocab_size = V,
    method = "Permutation language-model loss (XLNet, eq 3 and 5)"
  )
}

#' Differentially private mean of a bounded sample
#'
#' Clip to \eqn{\[lower, lower + C\]}, average, then perturb. Under
#' replace-one adjacency the sensitivity of the mean of \eqn{n} clipped
#' values is \eqn{\Delta = C/n}; Laplace noise \eqn{Lap(\Delta/\epsilon)}
#' gives \eqn{\epsilon}-differential privacy and Gaussian noise with
#' \eqn{\sigma = \Delta\sqrt{2\ln(1.25/\delta)}/\epsilon} gives
#' \eqn{(\epsilon, \delta)}-differential privacy.
#'
#' Clipping is the only stage that introduces bias, and nothing
#' downstream removes it, so the bias is returned rather than folded
#' into the estimate. Supplying `noise` directly makes every returned
#' quantity deterministic, which is what the cross-language parity test
#' anchors against -- R and Python cannot be made to draw the same
#' Laplace variate, but everything else agrees exactly.
#'
#' The width `C` must come from prior knowledge of the domain. Taking it
#' from the range of `y` makes it a function of the private data and
#' voids the guarantee; that case is flagged in `warnings`.
#'
#' @param y Numeric sample.
#' @param C Width of the clipping interval.
#' @param epsilon Privacy parameter, positive.
#' @param n Denominator for the sensitivity; defaults to `length(y)`.
#' @param mechanism "laplace" or "gaussian".
#' @param delta Used by the Gaussian mechanism only.
#' @param alpha Two-sided level.
#' @param lower Lower clipping bound.
#' @param noise Optional fixed perturbation, bypassing the draw.
#' @return A list with `estimate`, `clipped_mean`, `clipping_bias`,
#'   `sensitivity`, `noise_scale`, `noise_sd`, `ci_lower`, `ci_upper`,
#'   `ci_naive_lower`, `ci_naive_upper`, `n_clipped`, `warnings`.
#' @references Dwork C, Roth A (2014) \emph{Found Trends Theor Comput
#'   Sci} 9(3-4):211-487, Sec 3.3 and Thm A.1. Karwa V, Vadhan S (2017)
#'   arXiv:1711.03908.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_dp_mean(V)
morie_dp_mean <- function(y, C = NULL, epsilon = 1, n = NULL,
                          mechanism = c("laplace", "gaussian"),
                          delta = 1e-6, alpha = 0.05, lower = NULL,
                          noise = NULL) {
  mechanism <- match.arg(mechanism)
  x <- as.numeric(y)
  x <- x[is.finite(x)]
  m <- length(x)
  if (m < 1L) stop("y must contain at least one finite value.", call. = FALSE)
  if (!is.finite(epsilon) || epsilon <= 0) {
    stop(sprintf("epsilon must be positive and finite; got %s.", epsilon),
      call. = FALSE
    )
  }
  if (alpha <= 0 || alpha >= 1) {
    stop(sprintf("alpha must lie in (0, 1); got %s.", alpha), call. = FALSE)
  }
  nn <- if (is.null(n)) m else as.integer(n)
  if (nn < 1L) stop("n must be at least 1.", call. = FALSE)

  warns <- character(0)
  c_from_data <- is.null(C)
  if (c_from_data) {
    width <- max(x) - min(x)
    C_ <- if (width > 0) width else 1
    lo <- min(x)
    warns <- c(warns, paste(
      "C was taken from the range of y, which is itself a function of the",
      "private data. The stated guarantee does not hold for that choice."
    ))
  } else {
    C_ <- as.numeric(C)
    if (!is.finite(C_) || C_ <= 0) {
      stop(sprintf("C must be positive and finite; got %s.", C), call. = FALSE)
    }
    lo <- mean(x) - C_ / 2
  }
  if (!is.null(lower)) lo <- as.numeric(lower)
  hi <- lo + C_

  clipped <- pmin(pmax(x, lo), hi)
  n_clipped <- sum(x < lo | x > hi)
  mu_clip <- mean(clipped)
  mu_raw <- mean(x)
  sens <- C_ / nn

  if (mechanism == "laplace") {
    scale <- sens / epsilon
    noise_sd <- sqrt(2) * scale
    eps_draw <- if (is.null(noise)) {
      u <- stats::runif(1L) - 0.5
      -scale * sign(u) * log(1 - 2 * abs(u))
    } else {
      as.numeric(noise)
    }
  } else {
    if (delta <= 0 || delta >= 1) {
      stop(sprintf("delta must lie in (0, 1); got %s.", delta), call. = FALSE)
    }
    scale <- sens * sqrt(2 * log(1.25 / delta)) / epsilon
    noise_sd <- scale
    eps_draw <- if (is.null(noise)) {
      stats::rnorm(1L, 0, scale)
    } else {
      as.numeric(noise)
    }
    if (epsilon >= 1) {
      warns <- c(warns, paste(
        "The sigma used is the Dwork-Roth Thm A.1 bound, proved only for",
        "epsilon < 1. Above that use the analytic Gaussian mechanism of",
        "Balle and Wang (2018)."
      ))
    }
  }
  priv <- mu_clip + eps_draw

  s <- if (m > 1L) stats::sd(clipped) else 0
  samp_se <- s / sqrt(m)
  zc <- stats::qnorm(1 - alpha / 2)
  z_half <- stats::qnorm(1 - alpha / 4)
  noise_half <- if (mechanism == "laplace") {
    scale * log(2 / alpha)
  } else {
    z_half * scale
  }
  half <- z_half * samp_se + noise_half

  if (n_clipped > 0L) {
    warns <- c(warns, sprintf(
      "%d of %d observations were clipped, biasing the estimate by %+.4g
       before any noise was added.", n_clipped, m, mu_clip - mu_raw
    ))
  }
  list(
    estimate = priv, non_private_mean = mu_raw, clipped_mean = mu_clip,
    clipping_bias = mu_clip - mu_raw, sensitivity = sens,
    noise_scale = scale, noise_sd = noise_sd, noise_drawn = eps_draw,
    mechanism = mechanism, epsilon = epsilon,
    clip_lower = lo, clip_upper = hi, clip_width = C_,
    n_clipped = n_clipped, sampling_se = samp_se,
    total_se = sqrt(samp_se^2 + noise_sd^2),
    ci_lower = priv - half, ci_upper = priv + half,
    ci_naive_lower = priv - zc * samp_se,
    ci_naive_upper = priv + zc * samp_se,
    n = m, n_denominator = nn, warnings = warns,
    method = paste(
      "Differentially private clipped mean",
      "(Laplace / Gaussian mechanism)"
    )
  )
}

#' The constant in the Hirano-Porter treatment-choice bound
#'
#' Under local asymptotics the welfare regret of the rule that treats
#' when \eqn{\hat\tau > 0}, against a truth \eqn{\tau = h/\sqrt n} with
#' \eqn{\hat\tau \sim N(h/\sqrt n, V/n)}, is
#' \eqn{|h| \Phi(-|h|/\sqrt V)/\sqrt n}. The worst case over the local
#' parameter therefore scales as \eqn{c\sqrt{V/n}} with
#' \eqn{c = \max_{t \ge 0} t\,\Phi(-t)}, whose stationary condition is
#' \eqn{\Phi(-t) = t\,\phi(t)}.
#'
#' The constant is solved rather than quoted. The two-figure 0.17 that
#' appears in the literature is not precise enough to check an
#' attainment claim against simulation.
#'
#' @param tol Bisection tolerance.
#' @return A list with `t_star`, `constant`, `stationarity_residual`.
#' @references Hirano K, Porter JR (2009) \emph{Econometrica}
#'   77(5):1683-1701, doi:10.3982/ECTA6630.
#' @export
#' @examples
#' morie_minimax_regret_constant()
morie_minimax_regret_constant <- function(tol = 1e-14) {
  lo <- 0
  hi <- 5
  for (i in seq_len(400L)) {
    mid <- (lo + hi) / 2
    if (stats::pnorm(-mid) - mid * stats::dnorm(mid) > 0) {
      lo <- mid
    } else {
      hi <- mid
    }
    if (hi - lo < tol) break
  }
  t <- (lo + hi) / 2
  list(
    t_star = t, constant = t * stats::pnorm(-t),
    stationarity_residual = abs(stats::pnorm(-t) - t * stats::dnorm(t))
  )
}

#' Semiparametric efficiency and minimax bounds for the ATE
#'
#' Hahn's bound under unconfoundedness is
#' \deqn{V_{eff} = E\left\[\frac{\sigma_1^2(X)}{e(X)} +
#'   \frac{\sigma_0^2(X)}{1 - e(X)} + (\tau(X) - \tau)^2\right\],}
#' a floor on the asymptotic variance of every regular asymptotically
#' linear estimator, and \eqn{c\sqrt{V_{eff}/n}} is a floor on the
#' worst-case welfare regret of every treatment rule.
#'
#' Three consequences follow from the shape of it, and the return value
#' separates them so each can be checked rather than assumed.
#'
#' The first term blows up as the propensity approaches zero or one.
#' That is not a numerical artefact to be trimmed away: it is the bound
#' stating that regions of covariate space with no comparison units
#' carry no information about the contrast there.
#'
#' The third term is the variance of the CONDITIONAL effect. It does not
#' shrink with better nuisance estimation and does not vanish when the
#' propensity is known. Effect heterogeneity has an irreducible price
#' for anyone reporting an average.
#'
#' Knowing the true propensity score does not lower the bound -- Hahn's
#' result is that it is the same either way, and that using an estimated
#' score can give a smaller variance than plugging in the true one.
#'
#' @param y Outcome vector.
#' @param D Binary treatment indicator.
#' @param X Covariate matrix; an intercept is added.
#' @param family "gaussian" or "binomial" for the arm regressions.
#' @param trim Propensities are confined to `\[trim, 1 - trim\]`.
#' @param alpha Level for the interval on the ATE.
#' @return A list with `efficiency_bound`, `se_bound`, `estimate` (the
#'   AIPW ATE), `var_aipw`, `var_ipw`, `aipw_efficiency_ratio`,
#'   `ipw_efficiency_ratio`, `minimax_regret_bound`, `overlap_term`,
#'   `heterogeneity_term`, `tau_x`, `warnings`.
#' @references Hahn J (1998) \emph{Econometrica} 66(2):315-331,
#'   doi:10.2307/2998560. Hirano K, Porter JR (2009)
#'   \emph{Econometrica} 77(5):1683-1701, doi:10.3982/ECTA6630.
#' @export
#' @examples
#' set.seed(1)
#' r <- morie_efficiency_bound_ate(y = rnorm(10), D = rbinom(10, 1, 0.5), X = rnorm(10)); TRUE
morie_efficiency_bound_ate <- function(y, D, X, family = c(
                                         "gaussian",
                                         "binomial"
                                       ),
                                       trim = 0.01, alpha = 0.05) {
  family <- match.arg(family)
  yv <- as.numeric(y)
  d <- as.numeric(D)
  Xa <- as.matrix(X)
  n <- length(yv)
  if (length(d) != n || nrow(Xa) != n) {
    stop(sprintf(
      "y, D and X must agree in length; got %d, %d and %d.",
      n, length(d), nrow(Xa)
    ), call. = FALSE)
  }
  if (n < 10L) {
    stop(sprintf("need at least 10 observations; got %d.", n), call. = FALSE)
  }
  if (!all(d %in% c(0, 1))) stop("D must be binary 0/1.", call. = FALSE)
  if (trim < 0 || trim >= 0.5) {
    stop(sprintf("trim must lie in [0, 0.5); got %s.", trim), call. = FALSE)
  }
  if (family == "binomial" && !all(yv %in% c(0, 1))) {
    stop('family = "binomial" requires a binary outcome.', call. = FALSE)
  }

  Xd <- cbind(1, Xa)
  gfit <- .morie_bounds_logit(Xd, d)
  e_raw <- gfit$fitted
  e <- pmin(pmax(e_raw, trim), 1 - trim)
  trim_binding <- sum(e_raw < trim | e_raw > 1 - trim)

  arm <- function(mask) {
    if (sum(mask) < ncol(Xd) + 1L) {
      stop(sprintf(paste(
        "only %d observations in one treatment arm for %d",
        "design columns; the outcome model is not",
        "identified."
      ), sum(mask), ncol(Xd)), call. = FALSE)
    }
    if (family == "binomial") {
      b <- .morie_bounds_logit(Xd[mask, , drop = FALSE], yv[mask])$beta
      eta <- pmin(pmax(as.vector(Xd %*% b), -30), 30)
      mu <- 1 / (1 + exp(-eta))
      # the Bernoulli variance is a function of the mean, not a free
      # parameter, so nothing further is estimated
      return(list(mu = mu, s2 = mu * (1 - mu)))
    }
    b <- qr.coef(qr(Xd[mask, , drop = FALSE]), yv[mask])
    b[is.na(b)] <- 0
    mu <- as.vector(Xd %*% b)
    r <- yv[mask] - as.vector(Xd[mask, , drop = FALSE] %*% b)
    dof <- max(sum(mask) - ncol(Xd), 1L)
    list(mu = mu, s2 = rep(sum(r^2) / dof, n))
  }
  a1 <- arm(d == 1)
  a0 <- arm(d == 0)
  tau_x <- a1$mu - a0$mu

  psi <- tau_x + d * (yv - a1$mu) / e - (1 - d) * (yv - a0$mu) / (1 - e)
  tau <- mean(psi)
  var_aipw <- mean((psi - tau)^2)

  w1 <- d / e
  w0 <- (1 - d) / (1 - e)
  mu1_h <- sum(w1 * yv) / sum(w1)
  mu0_h <- sum(w0 * yv) / sum(w0)
  tau_ipw <- mu1_h - mu0_h
  psi_ipw <- w1 * (yv - mu1_h) - w0 * (yv - mu0_h)
  var_ipw <- mean((psi_ipw - mean(psi_ipw))^2)

  overlap <- mean(a1$s2 / e + a0$s2 / (1 - e))
  heterogeneity <- mean((tau_x - mean(tau_x))^2)
  v_eff <- overlap + heterogeneity
  se_bound <- sqrt(v_eff / n)
  mc <- morie_minimax_regret_constant()

  warns <- character(0)
  if (isTRUE(gfit$separated)) {
    warns <- c(warns, paste(
      "The propensity model separated the data perfectly. Neither the",
      "fitted scores nor any bound computed from them is trustworthy."
    ))
  }
  if (trim_binding > 0L) {
    warns <- c(warns, sprintf(paste(
      "%d of %d propensity scores lay outside [%g, %g] and were trimmed.",
      "The bound is for the trimmed subpopulation, a different estimand",
      "from the ATE."
    ), trim_binding, n, trim, 1 - trim))
  }
  if (var_aipw < v_eff * (1 - 1e-8)) {
    warns <- c(warns, sprintf(
      paste(
        "The AIPW influence-function variance (%.6g) came out below the",
        "efficiency bound (%.6g), which cannot happen asymptotically."
      ),
      var_aipw, v_eff
    ))
  }

  zc <- stats::qnorm(1 - alpha / 2)
  se <- sqrt(var_aipw / n)
  list(
    estimate = tau, ate_aipw = tau, ate_ipw = tau_ipw, se = se,
    ci_lower = tau - zc * se, ci_upper = tau + zc * se,
    efficiency_bound = v_eff, se_bound = se_bound,
    var_aipw = var_aipw, var_ipw = var_ipw,
    aipw_efficiency_ratio = var_aipw / v_eff,
    ipw_efficiency_ratio = var_ipw / v_eff,
    overlap_term = overlap, heterogeneity_term = heterogeneity,
    minimax_regret_bound = mc$constant * se_bound,
    minimax_constant = mc$constant, minimax_t_star = mc$t_star,
    propensity = e, propensity_untrimmed = e_raw, tau_x = tau_x,
    mu1 = a1$mu, mu0 = a0$mu, trim = trim, trim_binding = trim_binding,
    min_propensity = min(e_raw), max_propensity = max(e_raw),
    family = family, n = n, n_treated = sum(d == 1), warnings = warns,
    method = paste(
      "Semiparametric efficiency and minimax regret",
      "bounds for the ATE"
    )
  )
}
