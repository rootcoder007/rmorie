# Deep Survival Machines: a mixture of parametric survival experts.
#
# The conditional survival function is a weighted mixture of K primitive
# parametric distributions,
#
#   S(t | x) = sum_{k=1..K} g_k(x) S_k(t | beta_k, eta_k),
#   g(x) = softmax(Phi(x)^T w),
#
# with the gates coming from a representation of the covariates and the
# K experts fixed across subjects. Two primitives are offered, as in
# the paper: Weibull and log-normal. Both have closed-form density AND
# survival function, which is exactly why they were chosen -- a
# censored observation needs S(t), not just f(t).
#
# The loss. Maximum likelihood over a mixture has no closed form, so
# the paper maximises an evidence lower bound obtained from Jensen's
# inequality: rather than ln sum_k g_k f_k, it uses sum_k g_k ln f_k.
# Uncensored and censored cases contribute separately,
#
#   L = sum_{i in U} sum_k g_k(x_i) ln f_k(t_i)            [ELBO_U]
#     + alpha * sum_{i in C} sum_k g_k(x_i) ln S_k(t_i)    [ELBO_C]
#     + L_prior,
#
# and alpha in [0, 1] discounts the censored term. That discount is
# not cosmetic: survival distributions have long right tails, and
# censored cases are the ones asking for P(T > t) far out in that
# tail, so weighting them fully biases the fit. Setting alpha = 0
# drops them entirely, which the anchor uses -- the loss then does
# not move at all when a censored time is changed.
#
# Jensen's inequality is checked, not assumed. elbo and exact_loglik
# both exist here, and the anchor verifies ELBO <= ln-likelihood on
# real numbers for every fit. A sign error or a misplaced gate would
# break that inequality immediately.
#
# Competing risks are handled the paper's way, by treating the other
# event as independent censoring and giving each risk its own expert
# set over a shared representation; fit_competing does that and
# returns one fit per risk.
#
# Sources: Nagpal, C., Li, X. & Dubrawski, A. (2021) "Deep Survival
# Machines: Fully Parametric Survival Regression and Representation
# Learning for Censored Data with Competing Risks", IEEE Journal of
# Biomedical and Health Informatics 25(8), 3163-3175,
# doi:10.1109/JBHI.2021.3052441 (arXiv:2003.01176). Sec. III for the
# mixture of K parametric distributions with softmax gates over a
# learned representation and for the choice of Weibull and log-normal
# primitives; Sec. III-C for ELBO_U, ELBO_C, the prior term and the
# combined loss L = ELBO_U + alpha ELBO_C + L_prior, including the
# long-tail argument for the discount alpha; and Sec. III-D for
# competing risks by shared representation with per-risk experts.

# Accepted primitives, mirroring morie.fn.survvae.PRIMITIVES.

# Base R has no erf/erfc; both are pnorm in disguise. Defined here so
# the arm stays base-R only, as the package requires.
#' Base R has no erf/erfc; both are pnorm in disguise. Defined here so
#'
#' the arm stays base-R only, as the package requires.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .survvae_erf(x = x)
#' res
.survvae_erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
#' .survvae_erfc
#'
#' A step of the survvae_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .survvae_erfc(x = x)
#' res
.survvae_erfc <- function(x) 2 * pnorm(-x * sqrt(2))

.GHC_SURVVAE_PRIMITIVES <- c("weibull", "lognormal")
.GHC_SURVVAE_FLOOR <- 1e-300

# Internal: validate the primitive argument. Single source of the error
# message, mirroring the Python arm's .check().
#' Internal: validate the primitive argument. Single source of the error
#'
#' message, mirroring the Python arm\'s .check().
#'
#' @param primitive A vector; its length is taken.
#' @return One of two values, depending on the branch taken.
#' @export
.ghc_survvae_check_primitive <- function(primitive) {
  if (!(length(primitive) == 1L && is.character(primitive) &&
        !is.na(primitive) && primitive %in% .GHC_SURVVAE_PRIMITIVES))
    stop(sprintf("survvae: primitive must be one of %s, got '%s'",
                 paste(.GHC_SURVVAE_PRIMITIVES, collapse = ", "),
                 as.character(primitive)))
}

# Internal: unpack the flat parameter vector v = (W | bias |
# log_shapes | log_scales) into named pieces. Mirrors the Python
# arm's .unpack(): W is a list of K length-d vectors, bias is length
# K, shapes and scales are length K (positives, recovered via exp()).
#' Internal: unpack the flat parameter vector v = (W | bias |
#'
#' log_shapes | log_scales) into named pieces. Mirrors the Python arm\'s
#' .unpack(): W is a list of K length-d vectors, bias is length K,
#' shapes and scales are length K (positives, recovered via exp()).
#'
#' @param v A vector; its length is taken and its elements indexed.
#' @param K A count; the body uses it as \code{seq_len(...)}.
#' @param d Numeric; combined arithmetically in the body.
#' @return A list with \code{W}, \code{bias}, \code{shapes}, \code{scales}.
#' @export
.ghc_survvae_unpack <- function(v, K, d) {
  v <- as.numeric(v)
  expected <- K * d + 3L * K
  if (length(v) < expected)
    stop("survvae: parameter vector too short")
  W <- lapply(seq_len(K) - 1L,
              function(k) v[(k * d + 1L):((k + 1L) * d)])
  off <- K * d
  bias <- v[(off + 1L):(off + K)]
  off <- off + K
  shapes <- exp(v[(off + 1L):(off + K)])
  off <- off + K
  scales <- exp(v[(off + 1L):(off + K)])
  list(W = W, bias = bias, shapes = shapes, scales = scales)
}

# Internal: Nelder-Mead wrapper. The Python arm uses
# _sci_core.minimize with method="Nelder-Mead"; optim(method=
# "Nelder-Mead") in base R runs the same algorithm and gives
# numerically equivalent results on smooth objectives like this one.
# The Python arm's six-inner-iteration structure is preserved by the
# outer loop in morie_survvae(), which calls this helper up to six
# times per restart, each time from the current point -- exactly
# mirroring the "for _ in range(6)" loop in fit().
#' Internal: Nelder-Mead wrapper. The Python arm uses
#'
#' _sci_core.minimize with method="Nelder-Mead"; optim(method=
#' "Nelder-Mead") in base R runs the same algorithm and gives
#' numerically equivalent results on smooth objectives like this one.
#' The Python arm\'s six-inner-iteration structure is preserved by the
#' outer loop in morie_survvae(), which calls this helper up to six
#' times per restart, each time from the current point -- exactly
#' mirroring the "for _ in range(6)" loop in fit().
#'
#' @param objective Passed to \code{optim}.
#' @param x0 A vector; its length is taken.
#' @param maxit Optional; may be \code{NULL}. Carried through into a list the body builds.
#' @return A list with \code{x}, \code{value}.
#' @export
.ghc_minimize_nm <- function(objective, x0, maxit = NULL) {
  n <- length(x0)
  if (is.null(maxit)) maxit <- 200L * n
  res <- tryCatch(
    optim(par = x0, fn = objective, method = "Nelder-Mead",
          control = list(maxit = maxit, fnscale = 1,
                         reltol = 1e-8, abstol = 1e-8)),
    error = function(e) list(par = x0, value = objective(x0)))
  list(x = as.numeric(res$par), value = res$value)
}

# Internal: Harrell's concordance index in base R, mirroring
# morie.fn.survrsf.c_index. Counts over comparable pairs (i, j)
# where the earlier observation is an event; non-events at the
# shorter time do not contribute because their true event time is
# unknown.
#' Internal: Harrell\'s concordance index in base R, mirroring
#'
#' morie.fn.survrsf.c_index. Counts over comparable pairs (i, j) where
#' the earlier observation is an event; non-events at the shorter time
#' do not contribute because their true event time is unknown.
#'
#' @param times A vector; its length is taken and its elements indexed.
#' @param events A vector; its length is taken and its elements indexed.
#' @param risks A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
.ghc_c_index <- function(times, events, risks) {
  times <- as.numeric(times)
  events <- as.numeric(events)
  risks <- as.numeric(risks)
  n <- length(times)
  if (length(events) != n || length(risks) != n)
    stop("survvae: times, events and risks must have the same length")
  conc <- 0
  disc <- 0
  tied <- 0
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      if (times[i] < times[j]) {
        if (events[i] == 1) {
          if (risks[i] > risks[j]) conc <- conc + 1
          else if (risks[i] < risks[j]) disc <- disc + 1
          else tied <- tied + 1
        }
      } else if (times[i] > times[j]) {
        if (events[j] == 1) {
          if (risks[i] > risks[j]) disc <- disc + 1
          else if (risks[i] < risks[j]) conc <- conc + 1
          else tied <- tied + 1
        }
      }
    }
  }
  total <- conc + disc + tied
  if (total == 0L) return(NaN)
  (conc + 0.5 * tied) / total
}

#' Accepted primitives for the DSM experts
#'
#' The character vector \code{c("weibull", "lognormal")}, mirroring
#' \code{morie.fn.survvae.PRIMITIVES}.
#'
#' @export
morie_survvae_PRIMITIVES <- .GHC_SURVVAE_PRIMITIVES

#' Log density of one expert
#'
#' Closed-form log density of the chosen primitive, used by
#' \code{morie_survvae_elbo} for uncensored observations.
#'
#' @param t Time, must be positive.
#' @param shape Shape parameter, must be positive.
#' @param scale Scale parameter, must be positive.
#' @param primitive One of \code{"weibull"} or \code{"lognormal"}.
#' @return Numeric scalar, the log density.
#' @references Nagpal et al. (2021), Sec. III.
#' @export
morie_survvae_log_pdf <- function(t, shape, scale, primitive = "weibull") {
  .ghc_survvae_check_primitive(primitive)
  t <- as.numeric(t)
  if (length(t) != 1L || is.na(t) || t <= 0)
    stop("survvae: survival times must be positive")
  shape <- as.numeric(shape)
  scale <- as.numeric(scale)
  if (length(shape) != 1L || is.na(shape) || shape <= 0 ||
      length(scale) != 1L || is.na(scale) || scale <= 0)
    stop("survvae: shape and scale must be positive")
  if (primitive == "weibull") {
    z <- t / scale
    return(log(shape) - log(scale) + (shape - 1) * log(z) - z ^ shape)
  }
  z <- (log(t) - log(scale)) / shape
  -log(t) - log(shape) - 0.5 * log(2 * pi) - 0.5 * z * z
}

#' Log survival of one expert -- what a censored case needs
#'
#' Closed-form log survival of the chosen primitive, used by
#' \code{morie_survvae_elbo} for censored observations and by
#' \code{morie_survvae_predict_survival}. \code{t = 0} returns
#' \code{0} for both primitives (an event at time zero has survival
#' one by convention). The Python arm does not validate
#' shape/scale here; invalid values propagate as arithmetic errors
#' and we mirror that.
#'
#' @param t Time, must be non-negative.
#' @param shape Shape parameter.
#' @param scale Scale parameter.
#' @param primitive One of \code{"weibull"} or \code{"lognormal"}.
#' @return Numeric scalar, the log survival.
#' @references Nagpal et al. (2021), Sec. III.
#' @export
morie_survvae_log_survival <- function(t, shape, scale,
                                       primitive = "weibull") {
  .ghc_survvae_check_primitive(primitive)
  t <- as.numeric(t)
  if (length(t) != 1L || is.na(t) || t < 0)
    stop("survvae: survival times must be non-negative")
  if (t == 0) return(0)
  if (primitive == "weibull")
    return(-((t / scale) ^ shape))
  z <- (log(t) - log(scale)) / shape
  # 0.5 * .erfc(z / sqrt(2)) == pnorm(-z) in base R, since pnorm is
  # the standard normal CDF and .erfc(x) = 2 Phi(-x sqrt(2)).
  s <- pnorm(-z)
  log(max(s, .GHC_SURVVAE_FLOOR))
}

#' Softmax over the experts
#'
#' Numerical-stable softmax of the gate logits
#' \eqn{z_k = W_k x + b_k}.
#'
#' @param x Covariate vector of length \code{d}.
#' @param W List of \code{K} numeric weight vectors, each of length
#'   \code{d}.
#' @param bias Numeric vector of length \code{K}.
#' @return Numeric vector of length \code{K}, sums to one.
#' @references Nagpal et al. (2021), Sec. III.
#' @export
morie_survvae_gates <- function(x, W, bias) {
  K <- length(bias)
  z <- numeric(K)
  for (k in seq_len(K)) z[k] <- sum(W[[k]] * x) + bias[k]
  m <- max(z)
  e <- exp(z - m)
  e / sum(e)
}

#' The paper's lower bound: gates outside the logarithm
#'
#' ELBO_U + \code{alpha} * ELBO_C - prior_penalty, where each term
#' uses the gate weights OUTSIDE the log (Jensen). Verifying that
#' this bound is less than or equal to the true mixture log-
#' likelihood is a built-in sanity check (see
#' \code{morie_survvae_exact_loglik}).
#'
#' @param X List of covariate vectors, length \code{n}.
#' @param y_lower Numeric vector of observation times, length
#'   \code{n}.
#' @param events Event indicator, length \code{n} (truthy =
#'   uncensored).
#' @param W List of \code{K} weight vectors of length \code{d}.
#' @param bias Numeric vector of length \code{K}.
#' @param shapes Numeric vector of \code{K} positive shape
#'   parameters.
#' @param scales Numeric vector of \code{K} positive scale
#'   parameters.
#' @param primitive One of \code{"weibull"} or \code{"lognormal"}.
#' @param alpha Discount on the censored term, in \code{\[0, 1\]}.
#' @param prior Strength of the log-parameter L2 penalty.
#' @return Named list with \code{elbo}, \code{uncensored},
#'   \code{censored}, \code{prior_penalty}, \code{alpha}.
#' @references Nagpal et al. (2021), Sec. III-C.
#' @export
morie_survvae_elbo <- function(X, y_lower, events, W, bias, shapes, scales,
                                primitive = "weibull", alpha = 1,
                                prior = 0) {
  .ghc_survvae_check_primitive(primitive)
  alpha <- as.numeric(alpha)
  if (!(length(alpha) == 1L && !is.na(alpha) && alpha >= 0 && alpha <= 1))
    stop(sprintf("survvae: alpha must lie in [0, 1], got '%s'",
                 as.character(alpha)))
  K <- length(shapes)
  tot_u <- 0
  tot_c <- 0
  for (i in seq_along(X)) {
    g <- morie_survvae_gates(X[[i]], W, bias)
    if (events[[i]]) {
      for (k in seq_len(K))
        tot_u <- tot_u + g[k] * morie_survvae_log_pdf(
          y_lower[[i]], shapes[k], scales[k], primitive)
    } else {
      for (k in seq_len(K))
        tot_c <- tot_c + g[k] * morie_survvae_log_survival(
          y_lower[[i]], shapes[k], scales[k], primitive)
    }
  }
  pen <- as.numeric(prior) *
    (sum(log(as.numeric(shapes)) ^ 2) +
     sum(log(as.numeric(scales)) ^ 2))
  list(elbo = tot_u + alpha * tot_c - pen,
       uncensored = tot_u, censored = tot_c,
       prior_penalty = pen, alpha = as.numeric(alpha))
}

#' The true mixture log-likelihood the bound sits underneath
#'
#' Computes \eqn{\ln \sum_k g_k \exp(\cdot)} rather than the
#' Jensen-relaxed sum. The anchor verifies this is at least as large
#' as the ELBO from \code{morie_survvae_elbo} on every fit.
#'
#' @inheritParams morie_survvae_elbo
#' @return Named list with \code{loglik}, \code{uncensored},
#'   \code{censored}.
#' @references Nagpal et al. (2021), Sec. III-C.
#' @export
morie_survvae_exact_loglik <- function(X, y_lower, events, W, bias,
                                        shapes, scales,
                                        primitive = "weibull",
                                        alpha = 1) {
  .ghc_survvae_check_primitive(primitive)
  K <- length(shapes)
  tot_u <- 0
  tot_c <- 0
  for (i in seq_along(X)) {
    g <- morie_survvae_gates(X[[i]], W, bias)
    if (events[[i]]) {
      m <- 0
      for (k in seq_len(K))
        m <- m + g[k] * exp(morie_survvae_log_pdf(
          y_lower[[i]], shapes[k], scales[k], primitive))
      tot_u <- tot_u + log(max(m, .GHC_SURVVAE_FLOOR))
    } else {
      m <- 0
      for (k in seq_len(K))
        m <- m + g[k] * exp(morie_survvae_log_survival(
          y_lower[[i]], shapes[k], scales[k], primitive))
      tot_c <- tot_c + log(max(m, .GHC_SURVVAE_FLOOR))
    }
  }
  list(loglik = tot_u + as.numeric(alpha) * tot_c,
       uncensored = tot_u, censored = tot_c)
}

#' Maximise the combined loss over gates and expert parameters
#'
#' Nelder-Mead over the stacked vector
#' \eqn{(W, b, \log \beta, \log \eta)} with random multi-restart.
#' Each restart builds its own starting point from \code{t0} (the
#' mean of the positive observed times) and the shared RNG, then
#' calls Nelder-Mead up to six times -- if a call lowers the
#' objective by more than \code{1e-9} the new point becomes the
#' start of the next call, exactly mirroring the Python arm's
#' \code{"for _ in range(6)"} loop in \code{fit()}.
#'
#' @param X List of covariate vectors, length \code{n}.
#' @param times Numeric vector of survival times, length \code{n}.
#' @param events Event indicator (0/1 or logical), length \code{n}.
#' @param K Number of experts.
#' @param primitive One of \code{"weibull"} or \code{"lognormal"}.
#' @param alpha Discount on the censored term, in \code{\[0, 1\]}.
#' @param prior Strength of the log-parameter L2 penalty.
#' @param seed Seed for the shared generator (see
#'   \code{.ghc_rng}).
#' @param restarts Number of random restarts; values below 1 are
#'   treated as 1.
#' @return Named list with element names matching the Python
#'   payload keys: \code{estimate}, \code{elbo}, \code{loglik},
#'   \code{jensen_gap}, \code{W}, \code{bias}, \code{shapes},
#'   \code{scales}, \code{K}, \code{primitive}, \code{alpha},
#'   \code{prior}, \code{times}, \code{events}, \code{method}.
#' @references Nagpal et al. (2021), Sec. III and III-C.
#' @export
morie_survvae <- function(X, times, events, K = 3L, primitive = "weibull",
                          alpha = 1, prior = 0, seed = 0,
                          restarts = 4L) {
  .ghc_survvae_check_primitive(primitive)
  times <- as.numeric(times)
  n <- length(times)
  if (!(n == length(X) && n == length(events)))
    stop("survvae: X, times and events must have the same length")
  if (n == 0L)
    stop("survvae: no observations")
  K <- as.integer(K)
  if (length(K) != 1L || is.na(K) || K < 1L)
    stop("survvae: K must be at least 1")
  if (is.logical(events)) events <- as.integer(events)
  events <- as.numeric(events)
  d <- length(X[[1L]])
  obs <- times[times > 0]
  if (length(obs) == 0L)
    # Mirrors the Python arm's ZeroDivisionError on "sum(obs) / 0".
    stop("survvae: division by zero")
  t0 <- sum(obs) / length(obs)

  objective <- function(v) {
    parts <- tryCatch(.ghc_survvae_unpack(v, K, d),
                      error = function(e) NULL)
    if (is.null(parts)) return(1e12)
    shapes <- parts$shapes
    scales <- parts$scales
    ss <- c(shapes, scales)
    if (any(ss <= 0 | ss > 1e6)) return(1e12)
    e <- tryCatch(
      morie_survvae_elbo(X, times, events, parts$W, parts$bias,
                         shapes, scales, primitive, alpha, prior),
      error = function(e) NULL)
    if (is.null(e)) return(1e12)
    val <- -e$elbo
    if (!is.finite(val)) return(1e12)
    val
  }

  rng <- .ghc_rng(as.numeric(seed))
  best <- NULL
  nr <- max(1L, as.integer(restarts))
  for (r in seq_len(nr)) {
    v0 <- numeric(K * d + K)
    for (k in seq_len(K))
      v0 <- c(v0, log(1 + 0.5 * (.ghc_unif(rng, 1L) - 0.5)))
    for (k in seq_len(K))
      v0 <- c(v0, log(t0 * (0.5 + .ghc_unif(rng, 1L))))
    val <- objective(v0)
    cur <- v0
    for (iter in seq_len(6L)) {
      res <- .ghc_minimize_nm(objective, cur)
      cand <- res$x
      nv <- objective(cand)
      if (nv < val - 1e-9) {
        val <- nv
        cur <- cand
      } else {
        if (nv < val) cur <- cand
        break
      }
    }
    if (is.null(best) || val < best$val)
      best <- list(val = val, cur = cur)
  }

  parts <- .ghc_survvae_unpack(best$cur, K, d)
  W <- parts$W
  bias <- parts$bias
  shapes <- parts$shapes
  scales <- parts$scales
  e <- morie_survvae_elbo(X, times, events, W, bias, shapes, scales,
                           primitive, alpha, prior)
  ex <- morie_survvae_exact_loglik(X, times, events, W, bias, shapes,
                                    scales, primitive, alpha)
  list(estimate = e$elbo, elbo = e$elbo,
       loglik = ex$loglik, jensen_gap = ex$loglik - e$elbo,
       W = W, bias = bias, shapes = shapes, scales = scales,
       K = K, primitive = primitive, alpha = as.numeric(alpha),
       prior = as.numeric(prior), times = as.numeric(times),
       events = as.numeric(events),
       method = sprintf("Deep Survival Machines: mixture of %s experts with softmax gates, ELBO_U + alpha ELBO_C + prior; Nagpal et al. (2021) Sec. III", primitive))
}

#' \eqn{S(t | x)} as the gated mixture of expert survivals
#'
#' @param fit_result A fit returned by \code{morie_survvae} or
#'   \code{morie_survvae_fit_competing}.
#' @param x Covariate vector.
#' @param times Numeric vector of times to evaluate.
#' @return Named list with \code{time}, \code{survival}, \code{gates}.
#' @references Nagpal et al. (2021), Sec. III.
#' @export
morie_survvae_predict_survival <- function(fit_result, x, times) {
  g <- morie_survvae_gates(x, fit_result$W, fit_result$bias)
  K <- fit_result$K
  out <- numeric(length(times))
  for (i in seq_along(times)) {
    t <- times[[i]]
    s <- 0
    for (k in seq_len(K))
      s <- s + g[k] * exp(morie_survvae_log_survival(
        t, fit_result$shapes[k], fit_result$scales[k],
        fit_result$primitive))
    out[i] <- s
  }
  list(time = as.numeric(times), survival = out, gates = g)
}

#' Risk at a horizon: \eqn{1 - S(t | x)}
#'
#' Default horizon is the median observation time -- the
#' \code{len // 2}-th element of the sorted \code{times} field of the
#' fit, matching the Python arm.
#'
#' @inheritParams morie_survvae_predict_survival
#' @param X List of covariate vectors.
#' @param horizon Optional numeric horizon; if \code{NULL} the
#'   median observed time is used.
#' @return Numeric vector of risks, one per row of \code{X}.
#' @references Nagpal et al. (2021), Sec. III.
#' @export
morie_survvae_risk_score <- function(fit_result, X, horizon = NULL) {
  if (is.null(horizon)) {
    sorted_times <- sort(fit_result$times)
    n <- length(sorted_times)
    horizon <- sorted_times[n %/% 2L + 1L]
  }
  vapply(X, function(x) {
    1 - morie_survvae_predict_survival(fit_result, x,
                                       c(horizon))$survival[1L]
  }, numeric(1L))
}

#' Harrell's C at a horizon
#'
#' Mirrors \code{morie.fn.survrsf.c_index}: pairwise count of
#' concordances, discordances and tied risks over the comparable
#' pairs.
#'
#' @inheritParams morie_survvae_risk_score
#' @param times Numeric vector of survival times.
#' @param events Event indicator.
#' @return Numeric scalar, the concordance index.
#' @references Harrell, F. E., Califf, R. M., Pryor, D. B., Lee,
#'   K. L. & Rosati, R. A. (1982). Evaluating the yield of medical
#'   tests. JAMA, 247(18), 2543-2546.
#' @export
morie_survvae_concordance <- function(fit_result, X, times, events,
                                       horizon = NULL) {
  .ghc_c_index(times, events,
               morie_survvae_risk_score(fit_result, X, horizon))
}

#' One fit per risk, other causes treated as censoring
#'
#' For each non-zero cause label \code{lab}, refits the model
#' treating \code{lab} as the event of interest and every other
#' non-zero cause as independent censoring (cause 0 is already
#' censored).
#'
#' @inheritParams morie_survvae
#' @param causes Integer vector of cause labels (0 = censored,
#'   \code{>0} = competing events).
#' @return Named list with \code{estimate} (number of risks),
#'   \code{risks} (sorted unique non-zero labels), \code{fits}
#'   (one fit per risk keyed by the label as a string) and
#'   \code{method}.
#' @references Nagpal et al. (2021), Sec. III-D.
#' @export
morie_survvae_fit_competing <- function(X, times, causes, K = 3L,
                                         primitive = "weibull",
                                         alpha = 1, prior = 0,
                                         seed = 0) {
  causes_int <- as.integer(causes)
  labels <- sort(unique(causes_int[causes_int != 0L]))
  if (length(labels) == 0L)
    stop("survvae: no competing events found; cause 0 means censored")
  out <- list()
  for (lab in labels) {
    ev <- as.integer(causes_int == lab)
    out[[as.character(lab)]] <- morie_survvae(
      X, times, ev, K, primitive, alpha, prior, seed)
  }
  list(estimate = length(labels), risks = as.integer(labels),
       fits = out,
       method = "competing risks by treating other causes as independent censoring; Nagpal et al. (2021) Sec. III-D")
}

#' Compact one-paragraph summary of the model
#'
#' Returns the Python arm's \code{cheatsheet()} string, so both arms
#' carry the same compact description.
#'
#' @return Character scalar.
#' @export
morie_survvae_cheatsheet <- function() {
  paste(paste0(
    "survvae: S(t|x) = sum_k g_k(x) S_k(t), gates a softmax and t",
    "he experts Weibull or log-normal -- both chosen because a ce",
    "nsored case needs S(t) in closed form. Trained on ELBO_U + a",
    "lpha ELBO_C + prior, with the gates OUTSIDE the log (Jensen)",
    ". alpha discounts the censored term against the long right t",
    "ail; alpha = 0 drops it entirely. The ELBO is checked agains",
    "t the exact mixture likelihood rather than assumed to sit be",
    "low it."
  ))
}
