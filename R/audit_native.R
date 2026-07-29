# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native counterparts for the re-implementation audit. Mirrors
# morie.fn.kpmnsv, morie.fn.diffmed, morie.fn.prdmed and
# morie.fn.entest.
#
# morie_survival_km already exists but calls survival::survfit, so it is
# a wrapper rather than a native specialization. morie_km_native computes
# the product-limit estimator and Greenwood variance directly, with no
# package dependency, and is what the cross-language parity test anchors
# against.

#' Kaplan-Meier product-limit estimator, computed natively
#'
#' \deqn{\hat S(t) = \prod_{t_i \le t}(1 - d_i / n_i)}
#' with Greenwood's variance
#' \deqn{\widehat{Var}\[\hat S(t)\] = \hat S(t)^2 \sum d_i / (n_i (n_i - d_i)).}
#'
#' Censored observations leave the risk set without producing a drop,
#' which is how partial information is used rather than discarded. The
#' assumption that buys it is INDEPENDENT censoring: those censored at
#' \eqn{t} must carry the same future risk as those still observed.
#' Informative censoring biases the curve and leaves no trace in the data.
#'
#' Intervals are on the log-log scale by default. A linear interval can
#' leave \\[0, 1\\], and does so exactly in the tails, where the estimate is
#' least precise.
#'
#' @param time Observed follow-up times.
#' @param event 1 if the event was observed, 0 if right-censored.
#' @param alpha Two-sided level.
#' @param conf_type Either "log-log" (default) or "plain".
#' @return A list with `times`, `survival`, `se`, `ci_lower`, `ci_upper`,
#'   `at_risk`, `events`, `median`, `rmst`, `tail_reliable`.
#' @references Kaplan EL, Meier P (1958) \emph{JASA} 53:457-481.
#'   Greenwood M (1926). Kalbfleisch & Prentice (2002), Sec 1.4.
#' @export
morie_km_native <- function(time, event, alpha = 0.05,
                            conf_type = c("log-log", "plain")) {
  conf_type <- match.arg(conf_type)
  t <- as.numeric(time)
  e <- as.numeric(event)
  n <- length(t)
  if (length(e) != n) stop("time and event must agree in length.", call. = FALSE)
  if (n < 1L) stop("need at least one observation.", call. = FALSE)
  if (!all(e %in% c(0, 1))) stop("event must be binary 0/1.", call. = FALSE)
  if (any(t < 0)) stop("time must be non-negative.", call. = FALSE)

  ord <- order(t, method = "radix")
  t <- t[ord]; e <- e[ord]
  uniq <- sort(unique(t[e == 1]))
  S <- 1; vs <- 0
  times <- surv <- ses <- numeric(0)
  risk <- evs <- integer(0)
  for (u in uniq) {
    nr <- sum(t >= u)
    di <- sum(t == u & e == 1)
    if (nr <= 0L) next
    S <- S * (1 - di / nr)
    vs <- if (nr > di) vs + di / (nr * (nr - di)) else Inf
    times <- c(times, u); surv <- c(surv, S)
    ses <- c(ses, if (is.finite(vs)) S * sqrt(vs) else NA_real_)
    risk <- c(risk, nr); evs <- c(evs, di)
  }
  z <- stats::qnorm(1 - alpha / 2)
  if (conf_type == "plain") {
    lo <- pmin(pmax(surv - z * ses, 0), 1)
    hi <- pmin(pmax(surv + z * ses, 0), 1)
  } else {
    ls <- log(pmax(surv, 1e-300))
    se_ll <- ifelse(surv > 0, ses / (surv * abs(ls)), NA_real_)
    lo <- pmin(pmax(surv^exp(z * se_ll), 0), 1)
    hi <- pmin(pmax(surv^exp(-z * se_ll), 0), 1)
    lo[is.na(lo)] <- 0; hi[is.na(hi)] <- 1
  }
  med <- if (any(surv <= 0.5)) times[which(surv <= 0.5)[1]] else NA_real_
  rmst <- if (length(times)) {
    edges <- c(0, times); heights <- c(1, surv)[-(length(surv) + 1L)]
    sum(diff(edges) * heights)
  } else 0
  list(times = times, survival = surv, se = ses,
       ci_lower = lo, ci_upper = hi, at_risk = risk, events = evs,
       median = med, rmst = rmst, tail_reliable = risk >= 10L,
       conf_type = conf_type,
       n_events = sum(e), n_censored = sum(1 - e), n = n,
       method = "Kaplan-Meier product-limit estimator (native)")
}

#' Difference-in-coefficients mediation estimator
#'
#' \eqn{c - c'}. For a continuous OLS outcome this equals \eqn{ab}
#' EXACTLY in every sample, not merely in expectation, so a non-zero
#' `identity_residual` means the two models were not fitted on the same
#' rows -- usually listwise deletion dropping different cases.
#'
#' The identity fails for logistic or probit outcomes for a structural
#' reason: those coefficients are identified only up to scale, and
#' conditioning on the mediator changes the residual variance and hence
#' the scale. That is non-collapsibility, not sampling error.
#'
#' @param c Total-effect coefficient.
#' @param c_prime Direct-effect coefficient, controlling for the mediator.
#' @param a,b Optional path coefficients, enabling the identity check.
#' @return A list with `indirect`, `proportion_mediated`, `product`,
#'   `identity_residual`, `matches_product`.
#' @references Judd CM, Kenny DA (1981) \emph{Evaluation Review} 5:602-619.
#'   MacKinnon DP (2008), Ch 3.
#' @export
morie_mediation_difference <- function(c, c_prime, a = NULL, b = NULL) {
  cv <- as.numeric(c); cp <- as.numeric(c_prime)
  ind <- cv - cp
  prod <- if (is.null(a) || is.null(b)) NULL else as.numeric(a) * as.numeric(b)
  resid <- if (is.null(prod)) NULL else abs(ind - prod)
  list(indirect = ind, total = cv, direct = cp,
       proportion_mediated = if (cv != 0) ind / cv else NA_real_,
       product = prod, identity_residual = resid,
       matches_product = if (is.null(resid)) NULL else resid < 1e-8,
       method = "Difference-in-coefficients indirect effect")
}

#' Product-of-coefficients mediation estimator
#'
#' \eqn{ab}, with the Sobel standard error
#' \eqn{\sqrt{a^2 s_b^2 + b^2 s_a^2}}.
#'
#' The Sobel interval assumes \eqn{ab} is normal. It is not: the product
#' of two normals is heavy-tailed and asymmetric, so a symmetric interval
#' misses on one side, and it does so worst at the small-to-moderate
#' effects mediation studies actually report. A percentile bootstrap
#' respects that asymmetry.
#'
#' @param a,b Path coefficients.
#' @param se_a,se_b Optional standard errors, enabling the Sobel interval.
#' @param alpha Two-sided level.
#' @return A list with `indirect`, `sobel_se`, `sobel_ci`.
#' @references MacKinnon DP (2008), Ch 3-4. Sobel ME (1982)
#'   \emph{Sociological Methodology} 13:290-312.
#' @export
morie_mediation_product <- function(a, b, se_a = NULL, se_b = NULL,
                                    alpha = 0.05) {
  av <- as.numeric(a); bv <- as.numeric(b)
  ind <- av * bv
  se <- ci <- NULL
  if (!is.null(se_a) && !is.null(se_b)) {
    se <- sqrt(av^2 * as.numeric(se_b)^2 + bv^2 * as.numeric(se_a)^2)
    z <- stats::qnorm(1 - alpha / 2)
    ci <- c(ind - z * se, ind + z * se)
  }
  list(indirect = ind, a = av, b = bv, sobel_se = se, sobel_ci = ci,
       sobel_symmetric = !is.null(ci),
       method = "Product-of-coefficients indirect effect")
}

#' Kozachenko-Leonenko k-nearest-neighbour differential entropy
#'
#' \deqn{\hat H = -\psi(k) + \psi(n) + \log c_d + (d/n) \sum \log \epsilon_i}
#' with \eqn{\epsilon_i} the distance to the k-th nearest neighbour.
#'
#' The \eqn{-\psi(k)} term is what makes this an estimator rather than a
#' plug-in: the count in a fixed-radius ball is Poisson and
#' \eqn{E\[\log N\] \ne \log E\[N\]}, so using \eqn{\log k} leaves a bias
#' that does not vanish with n.
#'
#' Differential entropy is not scale invariant -- rescaling by \eqn{a}
#' adds \eqn{d \log a} -- so it is not comparable across units and may be
#' negative.
#'
#' @param x Numeric vector or matrix, one row per observation.
#' @param k Neighbour rank.
#' @return A list with `entropy`, `k`, `dimension`,
#'   `distance_concentration`, `gaussian_reference`.
#' @references Kozachenko LF, Leonenko NN (1987). Kraskov A, Stogbauer H,
#'   Grassberger P (2004) \emph{Phys Rev E} 69:066138.
#' @export
morie_knn_entropy <- function(x, k = 3L) {
  X <- as.matrix(x)
  if (ncol(X) > 1L && nrow(X) == 1L) X <- t(X)
  n <- nrow(X); d <- ncol(X)
  k <- as.integer(k)
  if (k < 1L) stop("k must be at least 1.", call. = FALSE)
  if (n <= k) {
    stop(sprintf("need more than k = %d observations, got %d.", k, n),
         call. = FALSE)
  }
  D <- as.matrix(stats::dist(X))
  diag(D) <- Inf
  eps <- apply(D, 1L, function(r) sort(r)[k])
  if (any(eps <= 0)) {
    stop("duplicate points give a zero neighbour distance.", call. = FALSE)
  }
  log_cd <- (d / 2) * log(pi) - lgamma(d / 2 + 1)
  H <- -digamma(k) + digamma(n) + log_cd + (d / n) * sum(log(eps))
  cov <- stats::var(X)
  ld <- determinant(as.matrix(cov) + diag(1e-12, d), logarithm = TRUE)
  gauss <- if (ld$sign > 0) 0.5 * (d * log(2 * pi * exp(1)) + ld$modulus) else NA_real_
  list(entropy = H, k = k, dimension = d,
       neighbour_distances = eps,
       distance_concentration = stats::sd(eps) / mean(eps),
       gaussian_reference = as.numeric(gauss),
       negentropy = as.numeric(gauss) - H,
       n = n,
       method = "Kozachenko-Leonenko k-NN differential entropy")
}
