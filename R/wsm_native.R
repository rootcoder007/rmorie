# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Plug-in, resampling and Monte Carlo estimators.
#
# R mirror of morie.fn.{wsmkdn,wsmiis,wsmpst,wsmboo,wsmmle,wsmbgn,
# wsmadm}.
#
# These modules originally cited a textbook that is not in this
# repository's reference library, by chapter, with no way to check
# the chapters against anything. Each has been re-grounded on a
# primary source that IS in the library and was read from its PDF:
#
#   Silverman, B. W. (1986), Density Estimation for Statistics and
#     Data Analysis, Chapman and Hall -- the kernel estimator (2.2a)
#     and the window-width rules (3.28)-(3.31).
#   MacKay, D. J. C. (2003), Information Theory, Inference, and
#     Learning Algorithms, CUP -- importance sampling, Sec. 29.2,
#     Eqs. (29.21)-(29.22).
#   Hastie, Tibshirani and Friedman (2009), The Elements of
#     Statistical Learning, 2nd ed. -- the bootstrap variance (7.53)
#     and bagging (Sec. 8.7).
#   Kosorok, M. R. (2008), Introduction to Empirical Processes and
#     Semiparametric Inference, Springer -- the functional delta
#     method (Ch. 12) and M-estimation (Ch. 14).
#
# Admissibility is the exception: no text in the library covers
# statistical decision theory, so that module states its definition
# in full and says it carries no page-level citation, rather than
# inventing one.

# Silverman (3.30): A = min(standard deviation, interquartile
# range / 1.34). The divisor is 1.34 as the book prints it; 1.349 is
# the normal-theory value it rounds. Using the plain standard
# deviation is what makes the rule oversmooth skewed and long-tailed
# data -- one outlier moves the standard deviation a long way and the
# interquartile range hardly at all.
#' Silverman (3.30): A = min(standard deviation, interquartile
#'
#' range / 1.34). The divisor is 1.34 as the book prints it; 1.349 is
#' the normal-theory value it rounds. Using the plain standard deviation
#' is what makes the rule oversmooth skewed and long-tailed data -- one
#' outlier moves the standard deviation a long way and the interquartile
#' range hardly at all.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .wsm_spread(x = x)
#' res
.wsm_spread <- function(x) {
  xv <- as.numeric(x)
  if (length(xv) < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", length(xv)),
         call. = FALSE)
  }
  s <- stats::sd(xv)
  iqr <- unname(diff(stats::quantile(xv, c(0.25, 0.75), type = 7L)))
  if (iqr > 0) min(s, iqr / 1.34) else s
}

# Silverman Sec. 3.4.2. "3.28" is 1.06 sigma n^(-1/5), the pure
# normal reference; "3.29" is 0.79 R n^(-1/5) with R the
# interquartile range; "3.31" is 0.9 A n^(-1/5) and is the book's
# actual recommendation, reported to land within 10% of the optimal
# mean integrated square error across every t-distribution
# considered, log-normals with skewness up to about 1.8, and normal
# mixtures separated by up to 3 standard deviations.
#' Silverman Sec. 3.4.2. "3.28" is 1.06 sigma n^(-1/5), the pure
#'
#' normal reference; "3.29" is 0.79 R n^(-1/5) with R the interquartile
#' range; "3.31" is 0.9 A n^(-1/5) and is the book\'s actual
#' recommendation, reported to land within 10% of the optimal mean
#' integrated square error across every t-distribution considered,
#' log-normals with skewness up to about 1.8, and normal mixtures
#' separated by up to 3 standard deviations.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param rule The body requires: rule must be '3.28', '3.29' or '3.31'. Defaults to \code{"3.31"}.
#' @return Nothing; this branch always raises.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .wsm_bandwidth(x = x)
#' res
.wsm_bandwidth <- function(x, rule = "3.31") {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n), call. = FALSE)
  }
  if (identical(rule, "3.28")) {
    s <- stats::sd(xv)
    return(1.06 * (if (s > 0) s else 1) * n^(-0.2))
  }
  if (identical(rule, "3.29")) {
    r <- unname(diff(stats::quantile(xv, c(0.25, 0.75), type = 7L)))
    return(0.79 * (if (r > 0) r else 1) * n^(-0.2))
  }
  if (identical(rule, "3.31")) {
    a <- .wsm_spread(xv)
    return(0.9 * (if (a > 0) a else 1) * n^(-0.2))
  }
  stop("rule must be '3.28', '3.29' or '3.31'.", call. = FALSE)
}

#' .wsm_boot_reps
#'
#' A step of the wsm_native implementation. Called by \code{morie_wsm_bootstrap},
#' \code{morie_wsm_plug_in}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param data A matrix; the body checks with \code{is.matrix}.
#' @param statistic Accepted by the signature and not used anywhere in the body.
#' @param B A count; the body uses it as \code{seq_len(...)}.
#' @param seed Passed to \code{set.seed}.
#' @return A vector, from \code{vapply}.
#' @export
.wsm_boot_reps <- function(data, statistic, B, seed) {
  d <- if (is.matrix(data)) data else matrix(as.numeric(data), ncol = 1L)
  n <- nrow(d)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n), call. = FALSE)
  }
  B <- as.integer(B)
  if (is.na(B) || B < 2L) {
    stop("need at least 2 replicates.", call. = FALSE)
  }
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  vapply(seq_len(B), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    as.numeric(statistic(if (ncol(d) == 1L) d[idx, 1L] else d[idx, ,
                                                              drop = FALSE]))
  }, numeric(1))
}


#' Kernel density estimate with Silverman's window-width rules
#'
#' Silverman Eq. (2.2a), `fhat(x) = (1/nh) sum K((x - X_i)/h)`. Since
#' `K` is itself a probability density, the estimate is one too, and
#' inherits every continuity property of `K`.
#'
#' The default window width is the book's own recommendation,
#' Eq. (3.31) `h = 0.9 A n^(-1/5)` with `A = min(sd, IQR/1.34)` from
#' (3.30). It is NOT `1.06 sigma n^(-1/5)`: that is Eq. (3.28), the
#' pure normal reference, which the book presents as a starting point
#' and then improves on twice because it oversmooths skewed,
#' long-tailed and bimodal data.
#'
#' @param x evaluation points.
#' @param data numeric sample.
#' @param h window width; `rule` decides it when `NULL`.
#' @param rule one of `"3.31"` (default), `"3.28"`, `"3.29"`.
#' @return list: x, density, h, rule, adaptive_spread,
#'   h_normal_reference, h_iqr, mass, is_density, n, method.
#' @references Silverman, B. W. (1986), *Density Estimation for
#'   Statistics and Data Analysis*, Chapman and Hall, Eq. (2.2a) and
#'   Sec. 3.4.2 Eqs. (3.28)-(3.31); Rosenblatt (1956); Parzen (1962).
#' @examples
#' morie_wsm_kde(c(-1, 0, 1), stats::rnorm(200))$h
#' @export
morie_wsm_kde <- function(x, data, h = NULL, rule = "3.31") {
  d <- as.numeric(data)
  n <- length(d)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n), call. = FALSE)
  }
  hh <- if (is.null(h)) .wsm_bandwidth(d, rule) else as.numeric(h)
  if (hh <= 0) {
    stop(sprintf("the window width must be positive, got %g.", hh),
         call. = FALSE)
  }
  g <- as.numeric(x)
  dens <- rowSums(stats::dnorm(outer(g, d, "-") / hh)) / (n * hh)
  mass <- if (length(g) > 2L && all(diff(g) > 0)) {
    sum(diff(g) * (dens[-1L] + dens[-length(dens)])) / 2
  } else NULL
  list(x = g, density = dens, h = hh, rule = rule,
       adaptive_spread = .wsm_spread(d),
       h_normal_reference = .wsm_bandwidth(d, "3.28"),
       h_iqr = .wsm_bandwidth(d, "3.29"),
       mass = mass, is_density = TRUE,
       why_not_1_06 = paste("1.06 sigma n^(-1/5) is (3.28), the pure normal",
                            "reference; (3.31) replaces sigma with the",
                            "adaptive spread A and the constant with 0.9,",
                            "and that is what the book recommends"),
       n = n,
       method = "Silverman (2.2a) kernel density, window width by (3.31)")
}


#' Importance sampling
#'
#' MacKay Eqs. (29.21)-(29.22): weight draws from a sampler density
#' `Q` by `w_r = P*(x_r)/Q*(x_r)` and estimate `E_P\[phi\]` by
#' `sum(w phi)/sum(w)`.
#'
#' (29.22) is SELF-NORMALISED, and that is the point: dividing by
#' `sum(w)` means `P*` and `Q*` need only be known up to
#' multiplicative constants, which is the usual situation and the
#' reason the method is worth using. The unnormalised alternative
#' `mean(w phi)` requires both densities to be exactly normalised; it
#' is available through `normalised = TRUE`, and is unbiased where
#' (29.22) is biased for small `R` and consistent as `R` grows.
#'
#' The book is blunt about the failure mode. The variance of the
#' estimate is hard to gauge because the empirical variances of `w`
#' and `w phi` "are not necessarily a good guide to the true
#' variances", and if `Q` is small where `|phi P*|` is large the
#' estimate can be drastically wrong with no empirical sign of it.
#' The reported effective sample size is a diagnostic, not a
#' guarantee: it can only see regions that were actually sampled.
#' MacKay's conclusion is that an importance sampler should have
#' HEAVY TAILS.
#'
#' @param f the function whose mean under `P` is wanted.
#' @param p `P*`, the target up to a constant.
#' @param q `Q*`, the sampler density up to a constant.
#' @param samples draws from `Q`.
#' @param normalised use the unnormalised estimator instead.
#' @return list: estimate, weights, self_normalised,
#'   effective_sample_size, ess_fraction, max_weight_share, n, method.
#' @references MacKay, D. J. C. (2003), *Information Theory,
#'   Inference, and Learning Algorithms*, CUP, Sec. 29.2,
#'   Eqs. (29.20)-(29.22) and Fig. 29.6; Kahn and Marshall (1953).
#' @examples
#' xs <- stats::rcauchy(1000)
#' morie_wsm_importance_sampling(function(x) x^2, stats::dnorm,
#'                               stats::dcauchy, xs)$estimate
#' @export
morie_wsm_importance_sampling <- function(f, p, q, samples,
                                          normalised = FALSE) {
  xs <- as.numeric(samples)
  R <- length(xs)
  if (R < 2L) {
    stop(sprintf("need at least 2 draws, got %d.", R), call. = FALSE)
  }
  pv <- as.numeric(p(xs))
  qv <- as.numeric(q(xs))
  if (length(pv) != R || length(qv) != R) {
    stop("p and q must return one value per draw.", call. = FALSE)
  }
  if (any(qv <= 0)) {
    stop(paste("the sampler density is zero or negative at a drawn point;",
               "Q must be positive wherever P is."), call. = FALSE)
  }
  w <- pv / qv
  phi <- as.numeric(f(xs))
  tot <- sum(w)
  if (tot <= 0) {
    stop(paste("every importance weight is zero; the sampler and the target",
               "have no overlap in the drawn region."), call. = FALSE)
  }
  ess <- tot^2 / sum(w^2)
  list(estimate = if (normalised) mean(w * phi) else sum(w * phi) / tot,
       weights = w, self_normalised = !normalised,
       effective_sample_size = ess, ess_fraction = ess / R,
       max_weight_share = max(w) / tot, n = R,
       diagnostics_are_not_guarantees = paste(
         "the effective sample size can only see regions that were actually",
         "sampled; if Q is small where |phi P*| is large the estimate is",
         "wrong with no empirical sign of it"),
       heavy_tail_advice = paste(
         "an importance sampler should have HEAVY TAILS: MacKay's Fig. 29.6",
         "has a Gaussian sampler still wrong after 10^6 draws where a Cauchy",
         "sampler converges after about 5000"),
       method = paste("Importance sampling, MacKay (29.21) weights and",
                      "(29.22) self-normalised estimator"))
}


#' Plug-in estimator of a statistical functional
#'
#' `theta_hat = T(F_n)`: evaluate the functional at the empirical
#' distribution instead of the unknown `F`. Nothing is assumed about
#' `T` beyond being callable on a sample.
#'
#' The standard error is where the content is. Asymptotic normality
#' of a plug-in estimator needs `T` to be Hadamard-differentiable at
#' `F` tangentially to the relevant subspace -- the functional delta
#' method -- and that is a property of `T`, which this function
#' cannot inspect. It reports a nonparametric bootstrap standard
#' error, consistent for the same class of functionals, and says
#' plainly what the validity rests on. A non-differentiable `T` still
#' returns a number here; it is simply the wrong one, and nothing in
#' the output will say so.
#'
#' @param data numeric sample.
#' @param T the functional, applied to a sample.
#' @param B bootstrap replicates.
#' @param seed resampling seed.
#' @param se compute the bootstrap standard error.
#' @return list: estimate, se, bootstrap_bias, replicates,
#'   ci_percentile, n, B, validity_condition, method.
#' @references Kosorok, M. R. (2008), *Introduction to Empirical
#'   Processes and Semiparametric Inference*, Springer, Ch. 12 and
#'   Sec. 2.2.4; Ch. 10 for the bootstrap; von Mises (1947).
#' @examples
#' morie_wsm_plug_in(stats::rnorm(100), mean, B = 50)$se
#' @export
morie_wsm_plug_in <- function(data, T, B = 1000, seed = 0, se = TRUE) {
  if (!is.function(T)) stop("T must be a function.", call. = FALSE)
  d <- as.numeric(data)
  est <- as.numeric(T(d))
  if (!se) {
    return(list(estimate = est, se = NULL, n = length(d), B = 0L,
                method = "Plug-in estimator T(F_n), no standard error"))
  }
  reps <- .wsm_boot_reps(d, T, B, seed)
  ci <- unname(stats::quantile(reps, c(0.025, 0.975), type = 7L))
  list(estimate = est, se = stats::sd(reps),
       bootstrap_bias = mean(reps) - est, replicates = reps,
       ci_percentile = ci, n = length(d), B = length(reps),
       validity_condition = paste(
         "asymptotic normality needs T to be Hadamard-differentiable at F",
         "tangentially to the relevant subspace (functional delta method);",
         "this function cannot verify that, and a non-differentiable T",
         "returns a number that is simply wrong"),
       method = "Plug-in estimator T(F_n) with a nonparametric bootstrap SE")
}


#' Bootstrap variance estimator
#'
#' ESL Eq. (7.53), `Var\[S\] = (1/(B-1)) sum_b (S(Z*b) - Sbar*)^2`.
#'
#' The denominator is `B - 1`, not `B`, for the same reason a sample
#' variance carries `n - 1`: the replicates are centred at their own
#' mean. At the `B = 100` the book suggests the two differ by 1%; at
#' the `B = 20` someone in a hurry will use, by 5%. Both are
#' available and both are always reported.
#'
#' What this estimates is the variance of `S` under sampling from the
#' EMPIRICAL distribution. Its bearing on the real one rests on the
#' bootstrap being consistent for the statistic at hand.
#'
#' @param data numeric sample.
#' @param T the statistic, applied to a sample.
#' @param B replicates.
#' @param seed resampling seed.
#' @param ddof divisor is `B - ddof`; the book's (7.53) is 1.
#' @return list: value, se, variance_ddof1, variance_ddof0,
#'   mean_replicate, bias, replicates, B, n, ddof, method.
#' @references Hastie, Tibshirani and Friedman (2009), Eq. (7.53) and
#'   Fig. 7.12; Efron, B. (1979), *Annals of Statistics* 7:1-26;
#'   Kosorok (2008), Ch. 10.
#' @examples
#' morie_wsm_bootstrap(stats::rnorm(100), mean, B = 100)$se
#' @export
morie_wsm_bootstrap <- function(data, T, B = 1000, seed = 0, ddof = 1L) {
  if (!is.function(T)) stop("T must be a function.", call. = FALSE)
  dd <- as.integer(ddof)
  if (is.na(dd) || !dd %in% c(0L, 1L)) {
    stop(sprintf("ddof must be 0 or 1, got %s.", format(ddof)), call. = FALSE)
  }
  d <- as.numeric(data)
  reps <- .wsm_boot_reps(d, T, B, seed)
  Bn <- length(reps)
  v1 <- stats::var(reps)
  v0 <- v1 * (Bn - 1) / Bn
  v <- if (dd == 1L) v1 else v0
  list(value = v, se = sqrt(v), variance_ddof1 = v1, variance_ddof0 = v0,
       mean_replicate = mean(reps), bias = mean(reps) - as.numeric(T(d)),
       replicates = reps, B = Bn, n = length(d), ddof = dd,
       denominator_note = paste("ESL (7.53) divides by B - 1, not B; the",
                                "replicates are centred at their own mean"),
       what_it_estimates = paste(
         "the variance of S under sampling from the EMPIRICAL distribution;",
         "its bearing on the real one rests on the bootstrap being",
         "consistent for this statistic"),
       method = "Bootstrap variance estimator, ESL (7.53)")
}


#' Maximum likelihood as an M-estimator
#'
#' Maximises `loglik(theta) = sum_i log f(X_i; theta)`. Maximum
#' likelihood is the M-estimator with criterion `log f(x; theta)`, so
#' the general theory applies directly -- and that framing supplies
#' the conditions that matter: consistency needs a WELL-SEPARATED
#' maximum, not merely a stationary point.
#'
#' The standard error uses the OBSERVED information, the numerical
#' second derivative of the negative log-likelihood at the estimate,
#' rather than the expected (Fisher) information, which would need an
#' expectation this function cannot take.
#'
#' If the observed information is not positive definite the point
#' found is not a maximum and the standard error would be the square
#' root of a negative number, so `se` is `NULL` and `is_maximum` is
#' `FALSE` rather than the result being returned as though it were
#' fine.
#'
#' @param data numeric sample.
#' @param f `f(x, theta)` returning the DENSITY at each observation.
#' @param theta0 starting value; its length sets the dimension.
#' @param se compute the observed-information standard error.
#' @return list: estimate, se, loglik, observed_information,
#'   is_maximum, converged, n_params, n, method.
#' @references Kosorok, M. R. (2008), Ch. 14 (M-estimators) and
#'   Sec. 2.2.6; Fisher (1922).
#' @examples
#' x <- stats::rnorm(200, 2, 1.5)
#' morie_wsm_mle(x, function(d, t) stats::dnorm(d, t[1], abs(t[2])),
#'               c(0, 1))$estimate
#' @export
morie_wsm_mle <- function(data, f, theta0, se = TRUE) {
  d <- as.numeric(data)
  n <- length(d)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n), call. = FALSE)
  }
  t0 <- as.numeric(theta0)
  negll <- function(th) {
    v <- suppressWarnings(as.numeric(f(d, th)))
    if (any(!is.finite(v)) || any(v <= 0)) return(Inf)
    -sum(log(v))
  }
  if (!is.finite(negll(t0))) {
    stop(paste("the log-likelihood is not finite at theta0; the density is",
               "zero or negative at some observation there."), call. = FALSE)
  }
  # R warns that one-dimensional Nelder-Mead is unreliable, and it is
  # right to; BFGS is the appropriate simplex-free choice there. The
  # multi-parameter case keeps Nelder-Mead, which matches the Python
  # module and needs no derivatives.
  r <- if (length(t0) == 1L) {
    stats::optim(t0, negll, method = "BFGS",
                 control = list(maxit = 20000, reltol = 1e-12))
  } else {
    stats::optim(t0, negll, method = "Nelder-Mead",
                 control = list(maxit = 20000, reltol = 1e-12))
  }
  th <- as.numeric(r$par)
  out <- list(estimate = if (length(th) > 1L) th else th[1L],
              loglik = -r$value, converged = identical(r$convergence, 0L),
              n_params = length(th), n = n,
              information_used = paste(
                "observed, -d2 loglik/dtheta2 at theta_hat, by central",
                "differences; the expected (Fisher) information would need",
                "an expectation this function cannot take"),
              m_estimator_note = paste(
                "MLE is the M-estimator with criterion log f(x; theta);",
                "consistency needs a WELL-SEPARATED maximum, not just a",
                "stationary point"),
              method = "Maximum likelihood as an M-estimator (Kosorok Ch. 14)")
  if (!se) {
    out$se <- NULL
    out$is_maximum <- NULL
    return(out)
  }
  k <- length(th)
  step <- pmax(1e-5, 1e-4 * abs(th))
  H <- matrix(0, k, k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      ei <- numeric(k)
      ei[i] <- step[i]
      ej <- numeric(k)
      ej[j] <- step[j]
      H[i, j] <- (negll(th + ei + ej) - negll(th + ei - ej) -
                    negll(th - ei + ej) + negll(th - ei - ej)) /
        (4 * step[i] * step[j])
    }
  }
  H <- (H + t(H)) / 2
  eig <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
  is_max <- all(is.finite(eig)) && all(eig > 0)
  out$observed_information <- H
  out$is_maximum <- is_max
  if (is_max) {
    cov <- solve(H)
    s <- sqrt(diag(cov))
    out$se <- if (k > 1L) s else s[1L]
    out$covariance <- cov
  } else {
    out$se <- NULL
    out$not_a_maximum_note <- paste(
      "the observed information is not positive definite, so the point found",
      "is not a maximum and no standard error is reported")
  }
  out
}


#' Bagging
#'
#' ESL Sec. 8.7, `f_bag(x) = (1/B) sum_b f*b(x)`: the average of the
#' fits over `B` bootstrap samples.
#'
#' Bagging moves VARIANCE and leaves bias alone -- the replicates are
#' identically distributed, so the average has the same expectation
#' as any one of them. A sharp consequence follows: for a procedure
#' that is LINEAR in `y`, least squares above all, the bootstrap
#' average converges back to the fit on the original data, so bagging
#' does essentially nothing. It pays off where that argument fails,
#' on high-variance low-bias procedures that are wildly nonlinear in
#' the data -- a deep regression tree being the standard case.
#'
#' @param X predictors.
#' @param y response.
#' @param model optional `function(Xtr, ytr)` returning a predict
#'   function; least squares when `NULL`, which is precisely the case
#'   bagging cannot help and is the default only to make that visible.
#' @param B replicates.
#' @param newdata optional points to predict.
#' @param seed resampling seed.
#' @return list: prediction, single_fit, oob_prediction, oob_mse,
#'   replicate_spread, bagged_spread, max_shift_from_single_fit,
#'   n_oob_missing, B, n, method.
#' @references Hastie, Tibshirani and Friedman (2009), Sec. 8.7;
#'   Breiman, L. (1996), *Machine Learning* 24:123-140.
#' @examples
#' X <- matrix(stats::rnorm(150), 50)
#' morie_wsm_bagging(X, stats::rnorm(50), B = 20)$max_shift_from_single_fit
#' @export
morie_wsm_bagging <- function(X, y, model = NULL, B = 100, newdata = NULL,
                              seed = 0) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  yv <- as.numeric(y)
  if (nrow(A) != length(yv)) A <- t(A)
  if (nrow(A) != length(yv)) {
    stop(sprintf("X has %d rows for %d responses.", nrow(A), length(yv)),
         call. = FALSE)
  }
  n <- length(yv)
  if (n < 4L) {
    stop(sprintf("need at least 4 observations, got %d.", n), call. = FALSE)
  }
  Bn <- as.integer(B)
  if (is.na(Bn) || Bn < 1L) {
    stop("need at least one replicate.", call. = FALSE)
  }
  fit <- if (is.null(model)) function(Xtr, ytr) {
    b <- qr.coef(qr(cbind(1, Xtr)), ytr)
    function(Xn) as.numeric(cbind(1, Xn) %*% b)
  } else model
  Q <- if (is.null(newdata)) A else {
    QQ <- as.matrix(newdata)
    storage.mode(QQ) <- "double"
    QQ
  }
  if (ncol(Q) != ncol(A)) {
    stop(sprintf("newdata has %d columns, expected %d.", ncol(Q), ncol(A)),
         call. = FALSE)
  }
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))

  reps <- matrix(0, Bn, nrow(Q))
  oob_sum <- numeric(n)
  oob_cnt <- numeric(n)
  for (b in seq_len(Bn)) {
    idx <- sample.int(n, n, replace = TRUE)
    pred <- fit(A[idx, , drop = FALSE], yv[idx])
    reps[b, ] <- pred(Q)
    out <- setdiff(seq_len(n), idx)
    if (length(out)) {
      oob_sum[out] <- oob_sum[out] + pred(A[out, , drop = FALSE])
      oob_cnt[out] <- oob_cnt[out] + 1
    }
  }
  bagged <- colMeans(reps)
  single <- fit(A, yv)(Q)
  has_oob <- oob_cnt > 0
  oob_pred <- rep(NA_real_, n)
  oob_pred[has_oob] <- oob_sum[has_oob] / oob_cnt[has_oob]
  spread <- mean(apply(reps, 2L, function(z) mean((z - mean(z))^2)))
  list(prediction = bagged, single_fit = single, oob_prediction = oob_pred,
       oob_mse = if (any(has_oob) && is.null(newdata)) {
         mean((yv[has_oob] - oob_pred[has_oob])^2)
       } else NULL,
       replicate_spread = spread, bagged_spread = spread / Bn,
       max_shift_from_single_fit = max(abs(bagged - single)),
       n_oob_missing = sum(!has_oob), B = Bn, n = n,
       helps_when = paste(
         "the base procedure is high-variance, low-bias and NONLINEAR in y;",
         "for a linear procedure such as least squares the bootstrap average",
         "converges back to the original fit and bagging does nothing"),
       leaves_bias_alone = paste(
         "the replicates are identically distributed, so the average has the",
         "same expectation as any one of them; only the variance moves"),
       method = "Bagging, ESL Sec. 8.7: f_bag(x) = (1/B) sum_b f*b(x)")
}


#' Admissibility of decision rules by risk dominance
#'
#' A rule `T` is INADMISSIBLE when some other rule `T'` satisfies
#' `R(T', F) <= R(T, F)` for all `F` and `R(T', F) < R(T, F)` for
#' some `F`, and admissible when no such `T'` exists. That is a
#' statement about a COLLECTION of rules and a set of states, so it
#' cannot be decided for one rule alone: the argument is the whole
#' risk table, rules by states.
#'
#' Two consequences of the definition are easy to get wrong.
#' Admissibility is not optimality -- a rule can be admissible purely
#' because nothing beats it at one absurd state. And the strictness
#' matters: two rules with identical risk everywhere do not dominate
#' each other, so ties leave both admissible.
#'
#' Whether the answer means anything depends on the supplied table
#' being the real risk over the real state space. A rule admissible
#' against three sampled states may be inadmissible against the full
#' family.
#'
#' Source note: no text in this repository's reference library covers
#' statistical decision theory, so unlike its neighbours this
#' function carries no page-level citation. The definition above is
#' the standard one and is stated in full precisely so it can be
#' checked against any decision-theory reference to hand; the concept
#' is due to Wald's decision-theoretic programme.
#'
#' @param risk numeric matrix, rules by states; lower is better.
#' @param names optional labels for the rules.
#' @param tol comparisons are made up to this tolerance, so rules
#'   agreeing to floating-point noise count as tied.
#' @return list: admissible, bool, dominated_by, admissible_names,
#'   n_rules, n_states, minimax_rule, minimax_risk,
#'   is_complete_class, method.
#' @examples
#' morie_wsm_admissible(rbind(c(1, 5), c(2, 2), c(3, 6)))$admissible
#' @export
morie_wsm_admissible <- function(risk, names = NULL, tol = 1e-12) {
  R <- as.matrix(risk)
  storage.mode(R) <- "double"
  m <- nrow(R)
  s <- ncol(R)
  if (m < 1L || s < 1L) {
    stop("risk must be non-empty.", call. = FALSE)
  }
  if (any(!is.finite(R))) {
    stop("every risk must be finite to compare rules.", call. = FALSE)
  }
  if (!is.null(names) && length(names) != m) {
    stop(sprintf("names has %d entries for %d rules.", length(names), m),
         call. = FALSE)
  }
  adm <- rep(TRUE, m)
  dominated <- list()
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      if (i == j) next
      if (all(R[j, ] <= R[i, ] + tol) && any(R[j, ] < R[i, ] - tol)) {
        adm[i] <- FALSE
        key <- if (is.null(names)) as.character(i) else names[i]
        dominated[[key]] <- c(dominated[[key]],
                              if (is.null(names)) j else names[j])
      }
    }
  }
  worst <- apply(R, 1L, max)
  list(admissible = adm, bool = all(adm), dominated_by = dominated,
       admissible_names = if (is.null(names)) which(adm) else names[adm],
       n_rules = m, n_states = s,
       minimax_rule = if (is.null(names)) which.min(worst) else
         names[which.min(worst)],
       minimax_risk = min(worst),
       is_complete_class = all(adm),
       definition = paste("T is inadmissible when some T' has",
                          "R(T',F) <= R(T,F) for all F and R(T',F) < R(T,F)",
                          "for some F"),
       ties_note = paste("two rules with identical risk everywhere do NOT",
                         "dominate each other; both stay admissible"),
       scope_note = paste("admissibility is relative to the supplied rules",
                          "and states; a rule admissible against three",
                          "sampled states may be inadmissible against the",
                          "full family"),
       method = "Admissibility by pairwise risk dominance")
}
